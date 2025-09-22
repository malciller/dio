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

  (** Create initial buy/sell order pair at top-of-book prices *)
  let create_initial_order (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    let has_buy = K.Kraken_common_types.StrategyState.has_open_buy_order symbol in
    if not has_buy then (
      let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
        String.equal asset.symbol symbol
      ) runtime_cfg.assets in

      match asset_cfg_opt with
      | Some asset_cfg ->
          (match asset_cfg.min_usd_balance with
          | Some min_balance ->
              let min_balance_float = float_of_string (Primitives.Fixed.to_string min_balance) in
              if K.Kraken_common_types.StrategyState.get_usd_balance () < min_balance_float then (
                info_f ~section "USD balance %.2f is below minimum %.2f for %s. Creating final sell order."
                  (K.Kraken_common_types.StrategyState.get_usd_balance ()) min_balance_float symbol >>= fun () ->
                K.Kraken_common_types.StrategyState.create_final_sell_order
                  ~symbol ~cmd_buffer ~section
                  ~get_instrument_fn:K.Kraken_incoming_data.get_instrument
                  ~get_precisions_fn:(K.Kraken_common_types.StrategyState.get_precisions K.Kraken_incoming_data.get_precisions)
                  ~get_balances_fn:(fun () -> K.Kraken_balances.wait_for_balances ())
              ) else (
                K.Kraken_common_types.StrategyState.create_top_of_book_orders
                  ~symbol ~qty:asset_cfg.qty ~cmd_buffer ~section
                  ~get_precisions_fn:(K.Kraken_common_types.StrategyState.get_precisions K.Kraken_incoming_data.get_precisions)
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
        ) (K.Kraken_common_types.StrategyState.open_orders) [] in
        debug_f ~section "Found %d open buy orders for %s" (List.length open_buy_orders) tick.symbol >>= fun () ->
        (match open_buy_orders with
        | [(_order_id, order)] ->
            debug_f ~section "Processing single buy order %s for %s" order.order_id tick.symbol >>= fun () ->
            K.Kraken_common_types.StrategyState.amend_order_if_needed ~order ~new_price:tick.bid ~cmd_buffer ~section
        | [] ->
            debug_f ~section "No open buy orders found for %s" tick.symbol >>= fun () ->
            Lwt.return_unit
        | _ ->
            debug_f ~section "Multiple buy orders found for %s, skipping adjustment" tick.symbol >>= fun () ->
            Lwt.return_unit)
    | None ->
        warning_f ~section "No configuration found for %s" tick.symbol

  (** Process market events and recreate positions as needed *)
  let handle_execution runtime_cfg cmd_buffer symbols (event: Core.market_event) =
    match event with
    | Core.Fill { order_id; symbol; price; qty; _ } ->
        if List.mem symbol symbols then (
          match Hashtbl.find_opt (K.Kraken_common_types.StrategyState.open_orders) order_id with
          | Some (order : K.Kraken_common_types.order) ->
              K.Kraken_common_types.StrategyState.handle_order_fill
                ~order_id ~symbol ~price ~qty ~side:(match order.side with Some Buy -> `Buy | Some Sell -> `Sell | None -> `Buy) ~cmd_buffer ~runtime_cfg ~section
                ~handle_fill_fn:K.Kraken_balances.handle_fill_event
                ~sync_orders_fn:(fun () -> K.Kraken_common_types.StrategyState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ()) ())
                ~refresh_balance_fn:(fun () -> K.Kraken_common_types.StrategyState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()))
                ~create_orders_fn:(fun ~symbol ~qty ~cmd_buffer ~section ->
                  K.Kraken_common_types.StrategyState.create_top_of_book_orders
                    ~symbol ~qty ~cmd_buffer ~section
                    ~get_precisions_fn:(K.Kraken_common_types.StrategyState.get_precisions K.Kraken_incoming_data.get_precisions)
                )
          | None ->
              warning_f ~section "Fill event for unknown order %s, attempting to create orders anyway" order_id >>= fun () ->
              (K.Kraken_common_types.StrategyState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->
              K.Kraken_common_types.StrategyState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
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
            ) (K.Kraken_common_types.StrategyState.open_orders) None in
            
            (match symbol_and_side_opt with
            | Some (symbol, side) when List.mem symbol symbols ->
                info_f ~section "Order %s cancelled/rejected for %s, side=%s"
                  order_id symbol
                  (match side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown") >>= fun () ->
                
                (* Only create new orders if it was a buy order that was cancelled/rejected *)
                if side = Some Core.Buy then (
                  (K.Kraken_common_types.StrategyState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->
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
            ) (K.Kraken_common_types.StrategyState.open_orders) None in

            (match order_opt with
            | Some order when List.mem order.order_symbol symbols ->
                info_f ~section "Order %s fully filled (Ack confirmation) for %s, side=%s"
                  order_id order.order_symbol
                  (match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown") >>= fun () ->

                (K.Kraken_common_types.StrategyState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->

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

end

(** Start the top-level market making strategy *)
let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer =
  info_f ~section "Starting greedy market making strategy" >>= fun () ->

  (K.Kraken_common_types.StrategyState.wait_for_snapshot (fun () -> K.Kraken_incoming_data.wait_for_snapshot ())) () >>= fun () ->
  (K.Kraken_common_types.StrategyState.wait_for_instruments (fun () -> K.Kraken_incoming_data.wait_for_instruments ())) () >>= fun () ->

  K.Kraken_common_types.StrategyState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
  K.Kraken_common_types.StrategyState.initialize_orders runtime_cfg Config.GMM (fun () -> K.Kraken_incoming_data.get_all_open_orders ()) >>= fun () ->

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
    K.Kraken_common_types.StrategyState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
    balance_loop ()
  in

  let rec tick_loop () =
    Ringbuffer.pop tick_buffer >>= fun (tick : Event.tick) ->
    (if List.mem tick.symbol orderbook_symbols then (
      K.Kraken_common_types.StrategyState.update_price tick >>= fun () ->
      (K.Kraken_common_types.StrategyState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->
      State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
      State.create_initial_order runtime_cfg tick.symbol cmd_buffer
    ) else (
      Lwt.return_unit
    )) >>= fun () ->
    tick_loop ()
  in

  Lwt.join [execution_loop (); tick_loop (); balance_loop ()] 