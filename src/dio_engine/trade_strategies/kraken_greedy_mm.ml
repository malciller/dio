(*
  Kraken Greedy Market Making Strategy

  Maintains persistent orders at the best bid/ask prices in the order book.
  Places buy orders at top bid and sell orders at top ask, recreating positions
  only on buy order fills while continuously adjusting buy orders to maintain
  top-of-book positioning.
*)
open Lwt.Infix
open Dio_types
open Lwt_log_core
open Engine
module K = Kraken


let section = Lwt_log_core.Section.make "engine.strategy.kraken.GMM"

(*
  Strategy State Management

  Tracks price data, open orders, and USD balance for market making operations.
*)

module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16
  let open_orders : (string, K.Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16
  let usd_balance : float ref = ref 0.0
  let last_amend_time : (string, float) Hashtbl.t = Hashtbl.create 16
  let amend_cooldown = 5.0

  (** Update USD balance from exchange *)
  let refresh_usd_balance () =
    Kraken.Kraken_balances.wait_for_balances () >>= fun (_, _, _, balances) ->
    let z_usd_balance = Hashtbl.find_opt balances "ZUSD" |> Option.value ~default:0.0 in
    let usd_balance_val = Hashtbl.find_opt balances "USD" |> Option.value ~default:0.0 in
    let new_balance = z_usd_balance +. usd_balance_val in
    usd_balance := new_balance;
    if Hashtbl.length balances = 0 then
      warning_f ~section "No balance data received from WebSocket, USD balance may be stale: %.2f" new_balance
    else
      info_f ~section "USD balance: %.2f" new_balance

  (** Check if symbol has any open buy orders *)
  let has_open_buy_order symbol =
    let buy_orders = Hashtbl.fold (fun _ (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
        order :: acc
      else
        acc
    ) open_orders [] in
    List.length buy_orders > 0

  (** Get current price data for symbol *)
  let get_price symbol = Hashtbl.find_opt price_info symbol

  (** Get total quantity of an asset locked in open orders for a given symbol *)
  let get_balance_in_open_orders symbol =
    Hashtbl.fold (fun _ (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol then
        acc +. order.qty
      else
        acc
    ) open_orders 0.0

  (** Get current inventory for symbol *)
  let get_current_inventory symbol =
    match K.Kraken_incoming_data.get_instrument symbol with
    | Some instrument ->
      let base_currency = instrument.base in
      K.Kraken_balances.wait_for_balances () >>= fun (spot_balances, _, liquid_balances, _) ->
      let spot_bal = Hashtbl.find_opt spot_balances base_currency |> Option.value ~default:0.0 in
      let liquid_bal = Hashtbl.find_opt liquid_balances base_currency |> Option.value ~default:0.0 in
      Lwt.return (spot_bal +. liquid_bal)
    | None -> Lwt.return 0.0

  type top_price_info = {
    bid_price : Primitives.Price.t;
    ask_price : Primitives.Price.t;
    bid_str : string;
    ask_str : string;
    price_prec : int;
    qty_prec : int;
  }

  let get_precisions_or_default symbol =
    match K.Kraken_incoming_data.get_precisions symbol with
    | Some (price_prec, qty_prec) -> (price_prec, qty_prec)
    | None -> (8, 8)

  let get_top_prices symbol : top_price_info option =
    match K.Kraken_orderbook.get_top_of_book symbol with
    | Some (best_bid, best_ask) ->
        let price_prec, qty_prec = get_precisions_or_default symbol in
        let bid_price = Primitives.Price.of_string_exn ~scale:price_prec best_bid.price_str in
        let ask_price = Primitives.Price.of_string_exn ~scale:price_prec best_ask.price_str in
        Some { bid_price; ask_price; bid_str = best_bid.price_str; ask_str = best_ask.price_str; price_prec; qty_prec }
    | None -> None

  (** Create exchange-compliant order with proper formatting *)
  let create_order ~symbol ~side ~price ~qty =
    match K.Kraken_incoming_data.get_precisions symbol with
    | Some (price_prec, qty_prec) ->
        let price_str = Primitives.Price.to_string price in
        let qty_str = Primitives.Qty.to_string qty in
        let formatted_price = Primitives.Price.of_string_exn ~scale:price_prec price_str in
        let formatted_qty = Primitives.Qty.of_string_exn ~scale:qty_prec qty_str in
        let side_prefix = match side with Core.Buy -> "b-" | Core.Sell -> "s-" in
        let timestamp_str = Int64.to_string (Unix.time () *. 1_000_000. |> Int64.of_float) in
        let client_id = side_prefix ^ timestamp_str in
        let order = Core.Add {
          dst = "kraken";
          client_id;
          symbol;
          side;
          price = formatted_price;
          qty = formatted_qty;
          tif = GTC;
          tags = [];
        } in
        Some order
    | None ->
        None

  (** Create initial buy/sell order pair at top-of-book prices *)
  let create_initial_order (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    let has_buy = has_open_buy_order symbol in
    if not has_buy then (
      let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
        String.equal asset.symbol symbol
      ) runtime_cfg.assets in
      
      match asset_cfg_opt with
      | Some asset_cfg ->
          (* Check both constraints: min_usd_balance and max_exposure *)
          let check_min_usd_balance = match asset_cfg.min_usd_balance with
            | Some min_balance ->
                let min_balance_float = float_of_string (Primitives.Fixed.to_string min_balance) in
                !usd_balance >= min_balance_float
            | None -> true
          in
          
          (* For max_exposure check, we need to get current inventory first *)
          (match asset_cfg.max_exposure with
          | Some max_exposure_fixed ->
              let max_exposure = float_of_string (Primitives.Fixed.to_string max_exposure_fixed) in
              get_current_inventory symbol >>= fun current_inventory ->
              Lwt.return (current_inventory < max_exposure)
          | None ->
              Lwt.return true
          ) >>= fun check_max_exposure ->
          
          (* If both checks pass, place orders *)
          if check_min_usd_balance && check_max_exposure then (
            match get_top_prices symbol with
            | Some top_price_info ->
                let buy_order = create_order ~symbol ~side:Buy ~price:top_price_info.bid_price ~qty:asset_cfg.qty in
                let sell_order = create_order ~symbol ~side:Sell ~price:top_price_info.ask_price ~qty:asset_cfg.qty in
                (match buy_order, sell_order with
                | Some buy_cmd, Some sell_cmd ->
                    Ringbuffer.push cmd_buffer buy_cmd >>= fun () ->
                    Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                    info_f ~section "Placed orders for %s" symbol
                | Some _, None ->
                    error_f ~section "Failed to create sell order for %s" symbol
                | None, Some _ ->
                    error_f ~section "Failed to create buy order for %s" symbol
                | None, None ->
                    error_f ~section "Failed to create both orders for %s" symbol)
            | None ->
                warning_f ~section "No orderbook data for %s" symbol
          ) else (
            (* At least one constraint is violated - try to sell remaining balance if any *)
            if not check_min_usd_balance then (
              info_f ~section "USD balance %.2f is below minimum for %s. Checking for remaining asset balance to sell."
                !usd_balance symbol
            ) else (
              info_f ~section "Max exposure reached for %s. Checking for remaining asset balance to sell." symbol
            ) >>= fun () ->
            
            match K.Kraken_incoming_data.get_instrument symbol with
            | Some instrument ->
                let base_currency = instrument.base in
                let qty_prec = instrument.qty_precision in
                
                Kraken.Kraken_balances.wait_for_balances () >>= fun (spot_balances, _, liquid_balances, _) ->
                let spot_bal = Hashtbl.find_opt spot_balances base_currency |> Option.value ~default:0.0 in
                let liquid_bal = Hashtbl.find_opt liquid_balances base_currency |> Option.value ~default:0.0 in
                let total_balance = spot_bal +. liquid_bal in
                let balance_in_orders = get_balance_in_open_orders symbol in
                let available_balance = total_balance -. balance_in_orders in

                if available_balance > 0.00001 then (
                  let clean_qty = floor (available_balance *. 10.0 ** float_of_int qty_prec) /. (10.0 ** float_of_int qty_prec) in
                  (match K.Kraken_orderbook.get_best_bid_ask symbol with
                  | Some (_, ask_price_float) ->
                      let price_prec = instrument.price_precision in
                      let sell_price_str = Printf.sprintf "%.*f" price_prec ask_price_float in
                      let sell_price = Primitives.Price.of_string_exn ~scale:price_prec sell_price_str in
                      let qty_str = Printf.sprintf "%.*f" qty_prec clean_qty in
                      let sell_qty = Primitives.Qty.of_string_exn ~scale:qty_prec qty_str in
                      let sell_order = create_order ~symbol ~side:Sell ~price:sell_price ~qty:sell_qty in
                      (match sell_order with
                      | Some sell_cmd ->
                          Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                          info_f ~section "Placed sell order for %s (constraint violation)." symbol
                      | None ->
                          error_f ~section "Failed to create sell order for %s." symbol
                      )
                  | None ->
                      warning_f ~section "No orderbook data for %s, cannot place sell order." symbol
                  )
                ) else (
                  Lwt.return_unit
                )
            | None ->
                warning_f ~section "No instrument data for %s, cannot place sell order." symbol
          )
      | None ->
          warning_f ~section "No configuration found for %s" symbol
    ) else (
      Lwt.return_unit
    )

  (** Adjust buy orders to maintain top-of-book positioning *)
  let check_and_adjust_orders (runtime_cfg : Config.runtime_cfg) cmd_buffer (tick : Event.tick) =
    let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
      String.equal asset.symbol tick.symbol
    ) runtime_cfg.assets in
    match asset_cfg_opt with
    | Some _ ->
        debug_f ~section "Found asset config for %s" tick.symbol >>= fun () ->
        let open_buy_orders = Hashtbl.fold (fun order_id (order: K.Kraken_common_types.order) acc ->
          if String.equal order.order_symbol tick.symbol && order.side = Some Core.Buy then
            (order_id, order) :: acc
          else
            acc
        ) open_orders [] in
        debug_f ~section "Found %d open buy orders for %s" (List.length open_buy_orders) tick.symbol >>= fun () ->
        (match open_buy_orders with
        | [(_order_id, order)] ->
            debug_f ~section "Processing single buy order %s for %s" order.order_id tick.symbol >>= fun () ->
            (match get_top_prices tick.symbol with
            | Some top_price_info ->
                let now = Unix.gettimeofday () in
                let last_amend = Hashtbl.find_opt last_amend_time order.order_id |> Option.value ~default:0.0 in
                let time_since_last_amend = now -. last_amend in

                let current_price_str = Printf.sprintf "%.*f" top_price_info.price_prec order.limit_price in

                if String.equal current_price_str top_price_info.bid_str then (
                  debug_f ~section "Order %s already at top bid %s"
                    order.order_id top_price_info.bid_str >>= fun () ->
                  Lwt.return_unit
                ) else if time_since_last_amend >= amend_cooldown then (
                  info_f ~section "Amending order %s from %s to top bid %s"
                    order.order_id current_price_str top_price_info.bid_str >>= fun () ->
                  let amend_cmd = Core.Amend {
                    dst = "kraken";
                    order_id = order.order_id;
                    symbol = order.order_symbol;
                    new_price = top_price_info.bid_price;
                    new_qty = Primitives.Qty.of_string_exn ~scale:top_price_info.qty_prec (Printf.sprintf "%.*f" top_price_info.qty_prec order.qty);
                    ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
                  } in
                  Ringbuffer.push cmd_buffer amend_cmd >>= fun () ->
                  Hashtbl.replace last_amend_time order.order_id now;
                  info_f ~section "Amend queued for %s to price %s"
                    order.order_id top_price_info.bid_str
                ) else (
                  debug_f ~section "Skipping amend for order %s - cooldown active (%.1fs remaining)"
                    order.order_id (amend_cooldown -. time_since_last_amend)
                )
            | None ->
                warning_f ~section "No orderbook data for %s, cannot adjust buy order" tick.symbol
            )
        | [] ->
            debug_f ~section "No open buy orders found for %s" tick.symbol >>= fun () ->
            Lwt.return_unit
        | _ ->
            debug_f ~section "Multiple buy orders found for %s, skipping adjustment" tick.symbol >>= fun () ->
            Lwt.return_unit)
    | None ->
        warning_f ~section "No configuration found for %s" tick.symbol

  (** Sync local order state with exchange *)
  let sync_open_orders () =
    let exchange_orders = K.Kraken_incoming_data.get_all_open_orders () in
    Hashtbl.clear open_orders;
    Hashtbl.iter (fun order_id (order : K.Kraken_common_types.order) ->
      Hashtbl.add open_orders order_id order
    ) exchange_orders;
    Lwt.return_unit

  (** Process market events and recreate positions as needed *)
  let handle_execution runtime_cfg cmd_buffer symbols (event: Core.market_event) =
    match event with
    | Core.Fill { order_id; symbol; price; qty; _ } ->
        if List.mem symbol symbols then (
          match Hashtbl.find_opt open_orders order_id with
          | Some (order : K.Kraken_common_types.order) ->
              let fill_event = {
                Event.src = "kraken";
                symbol = order.order_symbol;
                order_id;
                side = (match order.side with Some Buy -> `Buy | Some Sell -> `Sell | None -> `Buy);
                qty = qty;
                price = price;
                ts = Unix.time () |> Int64.of_float;
              } in
              Kraken.Kraken_balances.handle_fill_event fill_event >>= fun () ->
              sync_open_orders () >>= fun () ->
              refresh_usd_balance () >>= fun () ->

              if not (Hashtbl.mem open_orders order_id) then (
                info_f ~section "Order %s completely filled" order_id >>= fun () ->

                if order.side = Some Core.Buy then (
                  create_initial_order runtime_cfg symbol cmd_buffer
                ) else (
                  Lwt.return_unit
                )
              ) else (
                debug_f ~section "Order %s partially filled, order still exists" order_id >>= fun () ->
                Lwt.return_unit
              )
          | None ->
              warning_f ~section "Fill event for unknown order %s, attempting to create orders anyway" order_id >>= fun () ->
              sync_open_orders () >>= fun () ->
              refresh_usd_balance () >>= fun () ->
              create_initial_order runtime_cfg symbol cmd_buffer
        ) else (
          debug_f ~section "Fill event for %s not in orderbook symbols, ignoring" symbol >>= fun () ->
          Lwt.return_unit
        )
    | Ack { order_id; client_id; state; _ } ->
        (match state with
        | Canceled | Rejected ->
            let symbol_and_side_opt = Hashtbl.fold (fun _ (order: K.Kraken_common_types.order) acc ->
              match order.client_id with
              | Some order_client_id when String.equal order_client_id client_id ->
                  Some (order.order_symbol, order.side)
              | _ -> acc
            ) open_orders None in
            
            (match symbol_and_side_opt with
            | Some (symbol, side) when List.mem symbol symbols ->
                info_f ~section "Order %s cancelled/rejected for %s, side=%s"
                  order_id symbol
                  (match side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown") >>= fun () ->
                if side = Some Core.Buy then (
                  sync_open_orders () >>= fun () ->
                  create_initial_order runtime_cfg symbol cmd_buffer
                ) else (
                  Lwt.return_unit
                )
            | _ ->
                debug_f ~section "Ack event for order %s not in orderbook symbols or not found" order_id >>= fun () ->
                Lwt.return_unit)
        | Filled ->
            let order_opt = Hashtbl.fold (fun _ (order: K.Kraken_common_types.order) acc ->
              if String.equal order.order_id order_id then
                Some order
              else acc
            ) open_orders None in

            (match order_opt with
            | Some order when List.mem order.order_symbol symbols ->
                info_f ~section "Order %s fully filled (Ack confirmation) for %s, side=%s"
                  order_id order.order_symbol
                  (match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown") >>= fun () ->
                sync_open_orders () >>= fun () ->
                if order.side = Some Core.Buy then (
                  create_initial_order runtime_cfg order.order_symbol cmd_buffer
                ) else (
                  Lwt.return_unit
                )
            | _ ->
                debug_f ~section "Filled Ack for order %s not in orderbook symbols or not found" order_id >>= fun () ->
                Lwt.return_unit)
        | _ ->
            debug_f ~section "Ack event for order %s with state %s, no action needed"
              order_id (match state with Open -> "Open" | Filled -> "Filled" | Canceled -> "Canceled" | Rejected -> "Rejected") >>= fun () ->
            Lwt.return_unit)
    | _ -> Lwt.return_unit

  (** Load existing orders and initialize state for orderbook symbols *)
  let initialize_orders (runtime_cfg : Config.runtime_cfg) =
    let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
      match asset.strategy with
      | Config.GMM -> Some asset.symbol
      | _ -> None
    ) runtime_cfg.assets in
    let exchange_orders = K.Kraken_incoming_data.get_all_open_orders () in
    Hashtbl.clear open_orders;
    let log_promises = Hashtbl.fold (fun order_id (order : K.Kraken_common_types.order) promises ->
      let log_promise =
        if List.mem order.order_symbol orderbook_symbols then (
          Hashtbl.replace open_orders order_id order;
          info_f ~section "Loaded existing order %s for %s" order_id order.order_symbol
        ) else
          Lwt.return_unit
      in
      log_promise :: promises
    ) exchange_orders [] in
    Lwt.join log_promises >>= fun () ->
    info_f ~section "Initialized %d open orders from exchange" (Hashtbl.length open_orders)

  (** Store latest price tick data *)
  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    State.update_global_price tick.symbol tick.current_price;
    Lwt.return_unit
end

(** Start the top-level market making strategy *)
let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer =
  info_f ~section "Starting greedy market making strategy" >>= fun () ->

  K.Kraken_incoming_data.wait_for_snapshot () >>= fun () ->
  K.Kraken_incoming_data.wait_for_instruments () >>= fun () ->

  State.refresh_usd_balance () >>= fun () ->
  State.initialize_orders runtime_cfg >>= fun () ->

  let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
    match asset.strategy with
    | Config.GMM -> Some asset.symbol
    | _ -> None
  ) runtime_cfg.assets in

  info_f ~section
    "Starting orderbook strategy for symbols: [%s]" (String.concat ", " orderbook_symbols) >>= fun () ->

  let rec execution_loop () =
    Ringbuffer.pop exec_buffer >>= fun event ->
    State.handle_execution runtime_cfg cmd_buffer orderbook_symbols event >>= fun () ->
    execution_loop ()
  in

  let balance_refresh_interval = 15.0 in
  let rec balance_loop () =
    Lwt_unix.sleep balance_refresh_interval >>= fun () ->
    State.refresh_usd_balance () >>= fun () ->
    balance_loop ()
  in

  let rec tick_loop () =
    Ringbuffer.pop tick_buffer >>= fun (tick : Event.tick) ->
    (if List.mem tick.symbol orderbook_symbols then (
      State.update_price tick >>= fun () ->
      State.sync_open_orders () >>= fun () ->
      State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
      State.create_initial_order runtime_cfg tick.symbol cmd_buffer
    ) else (
      Lwt.return_unit
    )) >>= fun () ->
    tick_loop ()
  in

  Lwt.join [execution_loop (); tick_loop (); balance_loop ()] 