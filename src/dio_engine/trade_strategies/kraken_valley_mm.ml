(*
  Kraken Valley Market Making Strategy 
*)
open Lwt.Infix
open Dio_types
open Lwt_log_core
open Engine
module K = Kraken


let section = Lwt_log_core.Section.make "engine.strategy.kraken.VMM"

(*
  Strategy State Management

  Tracks price data, open orders, and USD balance for market making operations.
*)

module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16
  let open_orders : (string, K.Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16
  let asset_balances : (string, float) Hashtbl.t = Hashtbl.create 16
  let buy_order_targets : (string, Primitives.Price.t) Hashtbl.t = Hashtbl.create 16
  let usd_balance : float ref = ref 0.0
  (* Real-time inventory tracking *)
  let inventory_tracker : (string, float) Hashtbl.t = Hashtbl.create 16
  (* Track fill quantities for target price cleanup *)
  let fill_qty_tracker : (string, float) Hashtbl.t = Hashtbl.create 16
  let fee_rates : (string, float) Hashtbl.t = Hashtbl.create 16
  let last_amend_time : (string, float) Hashtbl.t = Hashtbl.create 16  (* order_id -> last amend timestamp *)
  let amend_cooldown = 5.0  (* Minimum seconds between amendments for same order *)

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

  (** Update asset balance from exchange *)
  let refresh_asset_balance symbol =
    match K.Kraken_incoming_data.get_instrument symbol with
    | Some instrument ->
      let base_currency = instrument.base in
      Kraken.Kraken_balances.wait_for_balances () >>= fun (spot_balances, _, liquid_balances, _) ->
      let spot_bal = Hashtbl.find_opt spot_balances base_currency |> Option.value ~default:0.0 in
      let liquid_bal = Hashtbl.find_opt liquid_balances base_currency |> Option.value ~default:0.0 in
      let tradeable_balance = spot_bal +. liquid_bal in
      Hashtbl.replace asset_balances base_currency tradeable_balance;
      info_f ~section "Refreshed %s balance: %.8f" base_currency tradeable_balance
    | None ->
      warning_f ~section "No instrument data for %s, cannot refresh balance." symbol

  (** Get fee rate for a given symbol *)
  let get_fee_rate symbol =
    match Hashtbl.find_opt fee_rates symbol with
    | Some fee -> fee
    | None ->
      match K.Kraken_incoming_data.get_instrument symbol with
      | Some instrument ->
        let fee = instrument.maker_fee |> Option.value ~default:0.002 in
        info_f ~section "Fee for %s: %.6f" symbol fee |> ignore;
        Hashtbl.add fee_rates symbol fee;
        fee
      | None ->
        info_f ~section "No instrument data for %s, using default fee: 0.002" symbol |> ignore;
        0.002 (* Conservative default fee rate *)

  (** Check if symbol has any open buy orders *)
  let has_open_buy_order symbol =
    let buy_orders = Hashtbl.fold (fun _ (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
        order :: acc
      else
        acc
    ) open_orders [] in
    List.length buy_orders > 0

  (** Check if symbol has any open sell orders *)
  let has_open_sell_order symbol =
    let sell_orders = Hashtbl.fold (fun _ (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Sell then
        order :: acc
      else
        acc
    ) open_orders [] in
    List.length sell_orders > 0

  (** Get total quantity of open sell orders for symbol *)
  let get_open_sell_order_qty symbol =
    Hashtbl.fold (fun _ (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Sell then
        acc +. order.qty
      else
        acc
    ) open_orders 0.0

  (** Get current price data for symbol *)
  let get_price symbol = Hashtbl.find_opt price_info symbol

  (** Get real-time inventory for symbol *)
  let get_current_inventory symbol =
    match K.Kraken_incoming_data.get_instrument symbol with
    | Some instrument ->
      let base_currency = instrument.base in
      let exchange_balance = Hashtbl.find_opt asset_balances base_currency |> Option.value ~default:0.0 in
      let pending_inventory = Hashtbl.find_opt inventory_tracker base_currency |> Option.value ~default:0.0 in
      exchange_balance +. pending_inventory
    | None -> 0.0

  (** Update real-time inventory tracking *)
  let update_inventory symbol delta =
    match K.Kraken_incoming_data.get_instrument symbol with
    | Some instrument ->
      let base_currency = instrument.base in
      let current = Hashtbl.find_opt inventory_tracker base_currency |> Option.value ~default:0.0 in
      Hashtbl.replace inventory_tracker base_currency (current +. delta)
    | None -> ()

  (** Reset inventory tracker for symbol *)
  let reset_inventory_tracker symbol =
    match K.Kraken_incoming_data.get_instrument symbol with
    | Some instrument ->
      let base_currency = instrument.base in
      Hashtbl.replace inventory_tracker base_currency 0.0
    | None -> ()

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

  (** Manage buy/sell orders based on Valley Market Maker strategy *)
  let manage_orders (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    let has_buy = has_open_buy_order symbol in
    let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
      String.equal asset.symbol symbol
    ) runtime_cfg.assets in

    match asset_cfg_opt with
    | Some asset_cfg ->
      (match asset_cfg.max_exposure with
      | Some max_exposure_fixed ->
        let max_exposure = float_of_string (Primitives.Fixed.to_string max_exposure_fixed) in
        (match K.Kraken_incoming_data.get_instrument symbol with
        | Some instrument ->
          let current_inventory = get_current_inventory symbol in

          if current_inventory < max_exposure then (
            if not has_buy then (
              match get_price symbol with
              | Some tick ->
                let top_ask_price_float = Float.of_string (Primitives.Price.to_string tick.ask) in
                let asset_fee = get_fee_rate symbol in
                let profit_threshold = runtime_cfg.profit_threshold_pct /. 100.0 in

                let buy_price_float = top_ask_price_float *. (1.0 -. ((2.0 *. asset_fee) +. profit_threshold)) in
                let target_sell_price_float = buy_price_float *. (1.0 +. ((2.0 *. asset_fee) +. profit_threshold)) in

                let buy_price = Primitives.Price.of_string_exn ~scale:instrument.price_precision (Printf.sprintf "%.*f" instrument.price_precision buy_price_float) in
                let target_sell_price = Primitives.Price.of_string_exn ~scale:instrument.price_precision (Printf.sprintf "%.*f" instrument.price_precision target_sell_price_float) in

                let order_qty_float = Float.of_string (Primitives.Qty.to_string asset_cfg.qty) in
                let order_cost = buy_price_float *. order_qty_float in

                if !usd_balance >= order_cost then (
                  (match create_order ~symbol ~side:Buy ~price:buy_price ~qty:asset_cfg.qty with
                  | Some buy_cmd ->
                    (match buy_cmd with
                    | Core.Add order_data ->
                      Hashtbl.add buy_order_targets order_data.client_id target_sell_price;
                      Ringbuffer.push cmd_buffer buy_cmd >>= fun () ->
                      info_f ~section "Placed new buy order for %s at %s. Target sell: %s"
                        symbol
                        (Primitives.Price.to_string buy_price)
                        (Primitives.Price.to_string target_sell_price)
                    | _ -> Lwt.return_unit)
                  | None ->
                    error_f ~section "Failed to create buy order for %s" symbol)
                ) else (
                  warning_f ~section "Insufficient USD balance to place buy order for %s. Required: %.2f, Available: %.2f"
                    symbol order_cost !usd_balance
                )
              | None ->
                warning_f ~section "No price info for %s, cannot place buy order." symbol
            ) else Lwt.return_unit
          ) else (
            (* Max exposure reached - cancel buy orders and place consolidated sell *)
            let open_buy_orders = Hashtbl.fold (fun order_id (order: K.Kraken_common_types.order) acc ->
              if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
                (order_id, order) :: acc
              else
                acc
            ) open_orders [] in

            (* Cancel all buy orders first *)
            Lwt_list.iter_s (fun (order_id, _) ->
              let cancel_cmd = Core.Cancel {
                dst = "kraken";
                order_id;
              } in
              Ringbuffer.push cmd_buffer cancel_cmd >>= fun () ->
              info_f ~section "Max exposure reached. Cancelling buy order %s for %s" order_id symbol
            ) open_buy_orders >>= fun () ->

            (* Place consolidated sell order for available inventory *)
            let open_sell_qty = get_open_sell_order_qty symbol in
            let qty_to_sell = current_inventory -. open_sell_qty in

            if qty_to_sell > 0.000001 then ( (* Basic dust filter *)
              (* Round down to qty_precision to exclude dust fractions *)
              let clean_qty = floor (qty_to_sell *. 10.0 ** float_of_int instrument.qty_precision) /. (10.0 ** float_of_int instrument.qty_precision) in
              (match get_price symbol with
              | Some tick ->
                let sell_price = tick.ask in (* Sell at top of book *)
                let sell_qty_obj = Primitives.Qty.of_string_exn ~scale:instrument.qty_precision
                  (Printf.sprintf "%.*f" instrument.qty_precision clean_qty) in
                (match create_order ~symbol ~side:Sell ~price:sell_price ~qty:sell_qty_obj with
                | Some sell_cmd ->
                  Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                  info_f ~section "Max exposure reached. Placed consolidated sell order for %s: %.8f (clean: %.8f) at %s"
                    symbol qty_to_sell clean_qty (Primitives.Price.to_string sell_price)
                | None ->
                  error_f ~section "Failed to create consolidated sell order for %s" symbol)
              | None ->
                warning_f ~section "No price info for %s, cannot place consolidated sell order" symbol)
            ) else (
              Lwt.return_unit
            )
          )
        | None ->
          warning_f ~section "No instrument data for %s" symbol)
      | None ->
        warning_f ~section "max_exposure not configured for %s" symbol)
    | None ->
      warning_f ~section "No configuration found for %s" symbol
  (** Create initial buy/sell order pair at top-of-book prices *)
  let create_initial_order (_runtime_cfg : Config.runtime_cfg) (_symbol : 'a) (_cmd_buffer : 'b) =
    Lwt.return_unit

  (** Adjust buy orders to maintain top-of-book positioning *)
  let check_and_adjust_orders (runtime_cfg : Config.runtime_cfg) cmd_buffer (tick : Event.tick) =
    let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
      String.equal asset.symbol tick.symbol
    ) runtime_cfg.assets in
    match asset_cfg_opt with
    | Some _asset_cfg ->
      (match K.Kraken_incoming_data.get_instrument tick.symbol with
      | Some instrument ->
        let open_buy_orders = Hashtbl.fold (fun order_id (order: K.Kraken_common_types.order) acc ->
          if String.equal order.order_symbol tick.symbol && order.side = Some Core.Buy then
            (order_id, order) :: acc
          else
            acc
        ) open_orders [] in

        (match open_buy_orders with
        | [(_order_id, order)] ->
          let top_ask_price_float = Float.of_string (Primitives.Price.to_string tick.ask) in
          let asset_fee = get_fee_rate tick.symbol in
          let profit_threshold = runtime_cfg.profit_threshold_pct /. 100.0 in

          let new_buy_price_float = top_ask_price_float *. (1.0 -. ((2.0 *. asset_fee) +. profit_threshold)) in
          let new_target_sell_price_float = new_buy_price_float *. (1.0 +. ((2.0 *. asset_fee) +. profit_threshold)) in

          let new_buy_price = Primitives.Price.of_string_exn ~scale:instrument.price_precision (Printf.sprintf "%.*f" instrument.price_precision new_buy_price_float) in
          let new_target_sell_price = Primitives.Price.of_string_exn ~scale:instrument.price_precision (Printf.sprintf "%.*f" instrument.price_precision new_target_sell_price_float) in

          let order_price_float = order.limit_price in
          let price_diff_pct = abs_float (order_price_float -. new_buy_price_float) /. order_price_float in

          if price_diff_pct > 0.0001 then ( (* 0.01% threshold *)
            let now = Unix.gettimeofday () in
            let last_amend = Hashtbl.find_opt last_amend_time order.order_id |> Option.value ~default:0.0 in
            let time_since_last_amend = now -. last_amend in

            if time_since_last_amend >= amend_cooldown then (
              info_f ~section "Top ask changed. Amending buy order %s (%.8f -> %.8f)"
                order.order_id order_price_float new_buy_price_float >>= fun () ->
              let amend_cmd = Core.Amend {
                dst = "kraken";
                order_id = order.order_id;
                symbol = order.order_symbol;
                new_price = new_buy_price;
                new_qty = Primitives.Qty.of_string_exn ~scale:instrument.qty_precision (Printf.sprintf "%.*f" instrument.qty_precision order.qty);
                ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
              } in
              Hashtbl.replace buy_order_targets order.order_id new_target_sell_price;
              Ringbuffer.push cmd_buffer amend_cmd >>= fun () ->
              Hashtbl.replace last_amend_time order.order_id now;
              info_f ~section "Amending order %s to new price %s. New target sell: %s"
                order.order_id
                (Primitives.Price.to_string new_buy_price)
                (Primitives.Price.to_string new_target_sell_price)
            ) else (
              debug_f ~section "Skipping amend for order %s - cooldown active (%.1fs remaining)"
                order.order_id (amend_cooldown -. time_since_last_amend)
            )
          ) else (
            debug_f ~section "Order %s price %.8f matches target %.8f within threshold, no amendment needed"
              order.order_id order_price_float new_buy_price_float
          )
        | [] -> Lwt.return_unit
        | _ ->
          debug_f ~section "Multiple buy orders for %s, skipping adjustment." tick.symbol
        )
      | None -> Lwt.return_unit)
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

  (** Handle sell order fill and resume logic *)
  let handle_sell_fill (runtime_cfg: Config.runtime_cfg) cmd_buffer symbol qty_float =
    update_inventory symbol (-.qty_float);
    info_f ~section "Sell fill: Updated inventory for %s by -%.8f" symbol qty_float >>= fun () ->
    
    (* Clean up target price records for this quantity *)
    let remaining_qty = ref qty_float in
    let to_remove = ref [] in
    Hashtbl.iter (fun target_order_id stored_qty ->
      if !remaining_qty > 0.0 then (
        if stored_qty <= !remaining_qty then (
          remaining_qty := !remaining_qty -. stored_qty;
          to_remove := target_order_id :: !to_remove;
          Hashtbl.remove buy_order_targets target_order_id
        ) else (
          let new_qty = stored_qty -. !remaining_qty in
          Hashtbl.replace fill_qty_tracker target_order_id new_qty;
          remaining_qty := 0.0
        )
      )
    ) fill_qty_tracker;
    List.iter (Hashtbl.remove fill_qty_tracker) !to_remove;
    
    (* Check if we can resume placing buy orders *)
    let current_inventory = get_current_inventory symbol in
    info_f ~section "After sell fill, current inventory for %s: %.8f" symbol current_inventory >>= fun () ->
    
    (* Resume buy orders if inventory dropped below max exposure *)
    let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
      String.equal asset.symbol symbol
    ) runtime_cfg.assets in
    (match asset_cfg_opt with
    | Some asset_cfg ->
      (match asset_cfg.max_exposure with
      | Some max_exposure_fixed ->
        let max_exposure = float_of_string (Primitives.Fixed.to_string max_exposure_fixed) in
        if current_inventory < max_exposure then (
          info_f ~section "Inventory below max exposure. Resuming buy orders for %s" symbol >>= fun () ->
          manage_orders runtime_cfg symbol cmd_buffer
        ) else (
          Lwt.return_unit
        )
      | None -> Lwt.return_unit)
    | None -> Lwt.return_unit)

  (** Process market events and recreate positions as needed *)
  let handle_execution (runtime_cfg : Config.runtime_cfg) cmd_buffer symbols (event: Core.market_event) =
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

              if not (Hashtbl.mem open_orders order_id) then (
                info_f ~section "Order %s completely filled" order_id >>= fun () ->

                (* Handle fills based on order side *)
                if order.side = Some Core.Buy then (
                  (* Buy fill: update inventory and place sell order *)
                  let qty_float = Float.of_string (Primitives.Qty.to_string qty) in
                  update_inventory symbol qty_float;
                  info_f ~section "Buy fill: Updated inventory for %s by +%.8f" symbol qty_float >>= fun () ->
                  
                  match Hashtbl.find_opt buy_order_targets order.order_id with
                  | Some target_sell_price ->
                    (* Store fill quantity for this target price *)
                    Hashtbl.replace fill_qty_tracker order.order_id qty_float;
                    (match get_price symbol with
                    | Some tick ->
                      let current_top_ask_price = tick.ask in
                      let sell_price =
                        if Primitives.Price.gt current_top_ask_price target_sell_price then
                          current_top_ask_price
                        else
                          target_sell_price
                      in
                      (match create_order ~symbol ~side:Sell ~price:sell_price ~qty:qty with
                      | Some sell_cmd ->
                        Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                        info_f ~section "Buy order %s filled. Placed sell order for %s at %s"
                          order_id symbol (Primitives.Price.to_string sell_price)
                      | None ->
                        error_f ~section "Failed to create sell order for %s after buy fill" symbol)
                    | None ->
                      error_f ~section "No price info for %s, cannot place sell order" symbol)
                  | None ->
                    warning_f ~section "Could not find target sell price for filled buy order %s" order_id
                ) else if order.side = Some Core.Sell then (
                  (* Sell fill: reduce inventory and cleanup target prices *)
                  let qty_float = Float.of_string (Primitives.Qty.to_string qty) in
                  handle_sell_fill runtime_cfg cmd_buffer symbol qty_float
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
              manage_orders runtime_cfg symbol cmd_buffer
        ) else (
          debug_f ~section "Fill event for %s not in orderbook symbols, ignoring" symbol >>= fun () ->
          Lwt.return_unit
        )
    | Ack { order_id; client_id; state; _ } ->
        (match state with
        | Open ->
          (match Hashtbl.find_opt buy_order_targets client_id with
          | Some target_price ->
            Hashtbl.remove buy_order_targets client_id;
            Hashtbl.add buy_order_targets order_id target_price;
            info_f ~section "Ack for buy order %s. Mapped client_id %s to order_id." order_id client_id
          | None -> Lwt.return_unit)
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
                  manage_orders runtime_cfg symbol cmd_buffer
                ) else (
                  Lwt.return_unit
                )
            | _ ->
                debug_f ~section "Ack event for order %s not in orderbook symbols or not found" order_id >>= fun () ->
                Lwt.return_unit)
        | Filled -> (* Already handled by the Fill event *)
            Lwt.return_unit)
    | _ -> Lwt.return_unit

  (** Load existing orders and initialize state for orderbook symbols *)
  let initialize_orders (runtime_cfg : Config.runtime_cfg) =
    let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
      match asset.strategy with
      | Config.VMM -> Some asset.symbol
      | _ -> None
    ) runtime_cfg.assets in
    let exchange_orders = K.Kraken_incoming_data.get_all_open_orders () in
    Hashtbl.clear open_orders;
    let log_promises = Hashtbl.fold (fun order_id (order : K.Kraken_common_types.order) promises ->
      let log_promise =
        if List.mem order.order_symbol orderbook_symbols then (
          Hashtbl.add open_orders order_id order;
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
  info_f ~section "Starting valley market making strategy" >>= fun () ->

  K.Kraken_incoming_data.wait_for_snapshot () >>= fun () ->
  K.Kraken_incoming_data.wait_for_instruments () >>= fun () ->

  State.refresh_usd_balance () >>= fun () ->
  State.initialize_orders runtime_cfg >>= fun () ->

  let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
    match asset.strategy with
    | Config.VMM -> Some asset.symbol
    | _ -> None
  ) runtime_cfg.assets in

  info_f ~section
    "Starting orderbook strategy for symbols: [%s]" (String.concat ", " orderbook_symbols) >>= fun () ->

  let rec execution_loop () =
    Ringbuffer.pop exec_buffer >>= fun event ->
    State.handle_execution runtime_cfg cmd_buffer orderbook_symbols event >>= fun () ->
    execution_loop ()
  in

  let rec tick_loop () =
    Ringbuffer.pop tick_buffer >>= fun (tick : Event.tick) ->
    (if List.mem tick.symbol orderbook_symbols then (
      State.update_price tick >>= fun () ->
      State.sync_open_orders () >>= fun () ->
      State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
      State.manage_orders runtime_cfg tick.symbol cmd_buffer
    ) else (
      Lwt.return_unit
    )) >>= fun () ->
    tick_loop ()
  in

  Lwt.join [execution_loop (); tick_loop ()] 