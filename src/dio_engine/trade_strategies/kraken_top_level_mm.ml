(*
  Kraken Top-Level Market Making Strategy

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


let section = Lwt_log_core.Section.make "engine.strategy.kraken.MM"

(*
  Strategy State Management

  Tracks price data, open orders, and USD balance for market making operations.
*)

module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16
  let open_orders : (string, K.Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16
  let usd_balance : float ref = ref 0.0

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
      info_f ~section "Refreshed USD balance: %.2f (from %d balance entries)" new_balance (Hashtbl.length balances)

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
          (match asset_cfg.min_usd_balance with
          | Some min_balance ->
              let min_balance_float = float_of_string (Primitives.Fixed.to_string min_balance) in
              if !usd_balance < min_balance_float then (
              info_f ~section "USD balance %.2f is below minimum %.2f for %s. Skipping order creation."
                  !usd_balance min_balance_float symbol
              ) else (
                match get_price symbol with
                | Some tick ->
                    let buy_price = tick.bid in
                    let sell_price = tick.ask in
                    let buy_order = create_order ~symbol ~side:Buy ~price:buy_price ~qty:asset_cfg.qty in
                    let sell_order = create_order ~symbol ~side:Sell ~price:sell_price ~qty:asset_cfg.qty in
                    (match buy_order, sell_order with
                    | Some buy_cmd, Some sell_cmd ->
                        info_f ~section "Successfully created both orders for %s, pushing to buffer" symbol >>= fun () ->
                        Ringbuffer.push cmd_buffer buy_cmd >>= fun () ->
                        info_f ~section "Buy order pushed to buffer for %s" symbol >>= fun () ->
                        Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                        info_f ~section "Sell order pushed to buffer for %s" symbol
                    | Some _, None ->
                        error_f ~section "Failed to create sell order for %s" symbol >>= fun () ->
                        Lwt.return_unit
                    | None, Some _ ->
                        error_f ~section "Failed to create buy order for %s" symbol >>= fun () ->
                        Lwt.return_unit
                    | None, None ->
                        error_f ~section "Failed to create both orders for %s" symbol >>= fun () ->
                        Lwt.return_unit)
                | None ->
                    warning_f ~section "No price info for %s" symbol
              )
          | None -> 
              warning_f ~section "min_usd_balance not configured for %s" symbol
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
            let top_bid_price = tick.bid in
            let order_price_float = order.limit_price in
            let top_bid_price_float = Float.of_string (Primitives.Price.to_string top_bid_price) in
            
            let price_diff = abs_float (order_price_float -. top_bid_price_float) in
            
            if price_diff > 0.0 then (
              info_f ~section "Prices differ, creating amend command for order %s (%.8f -> %.8f)"
                order.order_id order_price_float top_bid_price_float >>= fun () ->
              let amend_cmd = Core.Amend {
                dst = "kraken";
                order_id = order.order_id;
                symbol = order.order_symbol;
                new_price = top_bid_price;
                new_qty = Primitives.Qty.of_string_exn ~scale:8 (Printf.sprintf "%.8f" order.qty);
                ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
              } in
              Ringbuffer.push cmd_buffer amend_cmd >>= fun () ->
              info_f ~section "Amending order %s to new price %s" order.order_id (Primitives.Price.to_string top_bid_price)
            ) else (
              debug_f ~section "Order %s price %.8f matches top bid %.8f exactly, no amendment needed"
                order.order_id order_price_float top_bid_price_float >>= fun () ->
              Lwt.return_unit
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
                side = (match order.side with Some Buy -> `Buy | Some Sell -> `Sell | None -> `Buy); (* fallback *)
                qty = qty;
                price = price;
                ts = Unix.time () |> Int64.of_float;
              } in
              K.Kraken_balances.handle_fill_event fill_event >>= fun () ->
              sync_open_orders () >>= fun () ->
              refresh_usd_balance () >>= fun () ->

              if not (Hashtbl.mem open_orders order_id) then (
                info_f ~section "Order %s completely filled" order_id >>= fun () ->

                (* Only create new orders if it was a buy order that was filled *)
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
              (* If we can't find the order, we can't determine its side, so we'll be conservative and create orders *)
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
                
                (* Only create new orders if it was a buy order that was cancelled/rejected *)
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

                (* Only create new orders if it was a buy order that was filled *)
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
      | Config.MM -> Some asset.symbol
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
  info_f ~section "Starting orderbook market making strategy" >>= fun () ->

  K.Kraken_incoming_data.wait_for_snapshot () >>= fun () ->
  K.Kraken_incoming_data.wait_for_instruments () >>= fun () ->

  State.refresh_usd_balance () >>= fun () ->
  State.initialize_orders runtime_cfg >>= fun () ->

  let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
    match asset.strategy with
    | Config.MM -> Some asset.symbol
    | _ -> None
  ) runtime_cfg.assets in

  info_f ~section
    "Starting orderbook strategy for symbols: [%s]" (String.concat ", " orderbook_symbols) >>= fun () ->

  let rec execution_loop () =
    Ringbuffer.pop exec_buffer >>= fun event ->
    State.handle_execution runtime_cfg cmd_buffer orderbook_symbols event >>= fun () ->
    execution_loop ()
  in

  let balance_refresh_interval = 300.0 in
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