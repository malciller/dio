(* src/dio_engine/trade_strategies/kraken_top_level_mm.ml *)

(*
  TOP-LEVEL ORDER BOOK MARKET MAKING STRATEGY FOR KRAKEN

  This strategy implements high-frequency market making by maintaining orders at the
  very top of the order book bid/ask spread. Ideal for zero-fee trading pairs.

  STRATEGY BEHAVIOR:
  - Places buy orders at the current top bid price
  - Places sell orders at the current top ask price
  - Buy order fills trigger immediate recreation of the order pair
  - Continuously adjusts buy orders to maintain top-level positioning
  - Only recreates orders when buy orders are filled (not sell orders)

  KEY FEATURES:
  - Zero-latency order placement at best available prices
  - Automatic order management and position maintenance
  - Real-time synchronization with exchange order book
  - Precision-aware order formatting for exchange compliance
  - Comprehensive logging and state tracking

*)

(*
  ARCHITECTURAL OVERVIEW

  The top-level market making strategy maintains persistent presence at the best
  available prices in the order book:

  COMPONENTS:
  - State Management: Tracks current orders and price information
  - Order Creation: Generates exchange-compliant orders with proper formatting
  - Order Adjustment: Continuously maintains top-level positioning
  - Execution Handling: Processes fills and triggers order recreation
  - State Synchronization: Keeps local state consistent with exchange

  OPERATION:
  - Maintains one buy order at top bid and one sell order at top ask
  - Buy order fills trigger immediate recreation of both orders if min_usd_balance < current_usd_balance
  - Sell order fills are logged but don't trigger recreation
  - Buy orders are continuously adjusted to stay at top bid
  - All operations are synchronized with exchange state
*)
open Lwt.Infix
open Dio_types
open Lwt_log_core
module K = Kraken

let section = Lwt_log_core.Section.make "engine.strategy.kraken.orderbook"

(*
  STATE MANAGEMENT MODULE

  Manages the internal state of the top-level market making strategy including:
  - Current price information for all tracked symbols
  - Open orders synchronized with the exchange
  - Current USD account balance
*)

module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16
  let open_orders : (string, K.Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16
  let usd_balance : float ref = ref 0.0

  (** Fetch and update the USD balance from Kraken. *)
  let refresh_usd_balance (core_cfg : Config.engine_config) =
    K.Kraken_balances.get_account_balance core_cfg >>= fun balances ->
    let z_usd_balance = Hashtbl.find_opt balances "ZUSD" |> Option.value ~default:0.0 in
    let usd_balance_val = Hashtbl.find_opt balances "USD" |> Option.value ~default:0.0 in
    usd_balance := z_usd_balance +. usd_balance_val;
    info_f ~section "Refreshed USD balance: %.2f" !usd_balance

  (** Check if a symbol has any open buy orders.

      @param symbol Trading symbol to check
      @return true if at least one buy order exists for the symbol
  *)
  let has_open_buy_order symbol =
    let buy_orders = Hashtbl.fold (fun _ (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
        order :: acc
      else
        acc
    ) open_orders [] in
    List.length buy_orders > 0

  (** Get current price information for a symbol.

      @param symbol Trading symbol to query
      @return Current price tick data or None if not available
  *)
  let get_price symbol = Hashtbl.find_opt price_info symbol

  (** Create a precisely formatted order for Kraken exchange.

      Generates an order with proper precision formatting and unique client ID.
      Ensures exchange precision requirements are met to avoid order rejections.

      @param symbol Trading symbol for the order
      @param side Buy or Sell side
      @param price Order price (will be reformatted to exchange precision)
      @param qty Order quantity (will be reformatted to exchange precision)
      @return Formatted Core.order_cmd option, None if precision data unavailable
  *)
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

  (** Create initial buy/sell order pair at current top-of-book prices.

      Establishes the baseline market making position by:
      - Placing buy order at current top bid price
      - Placing sell order at current top ask price
      - Only creates orders if no buy order currently exists
      - Uses configured quantity from asset settings

      @param runtime_cfg Runtime configuration containing asset settings
      @param symbol Trading symbol to create orders for
      @param cmd_buffer Command buffer for order submission
      @return Unit promise when orders are created and submitted
  *)
  let create_initial_order (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    let has_buy = has_open_buy_order symbol in
    info_f ~section "has_open_buy_order for %s: %b" symbol has_buy >>= fun () ->

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
                warning_f ~section "USD balance %.2f is below minimum %.2f for %s. Skipping order creation."
                  !usd_balance min_balance_float symbol
              ) else (
                match get_price symbol with
                | Some tick ->
                    info_f ~section "Found price data for %s: bid=%s ask=%s"
                      symbol
                      (Primitives.Price.to_string tick.bid)
                      (Primitives.Price.to_string tick.ask) >>= fun () ->
                    let buy_price = tick.bid in
                    let sell_price = tick.ask in
                    info_f ~section "Creating orders for %s: buy_price=%s sell_price=%s"
                      symbol
                      (Primitives.Price.to_string buy_price)
                      (Primitives.Price.to_string sell_price) >>= fun () ->
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

  (** Check and adjust buy orders to maintain top-level positioning.

      Monitors the price difference between current buy orders and the top bid.
      If any difference exists (however small), amends the buy order to the current top bid.
      This ensures the strategy always maintains the best available buy price.

      @param runtime_cfg Runtime configuration with asset settings
      @param cmd_buffer Command buffer for order amendments
      @param tick Current price tick data
      @return Unit promise when order checking is complete
  *)
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
            
            debug_f ~section "Price comparison for order %s: order_price_float=%.8f top_bid_price_float=%.8f"
              order.order_id order_price_float top_bid_price_float >>= fun () ->
            
            (* No tolerance, any price change triggers an amend *)
            let price_diff = abs_float (order_price_float -. top_bid_price_float) in
            
            debug_f ~section "Price difference: %.10f, needs_amend: %b"
              price_diff (price_diff > 0.0) >>= fun () ->
            
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

  (** Synchronize local order state with exchange.

      Updates local order tracking to match current exchange state.
      Clears and repopulates the local orders hash table.

      @return Unit promise when synchronization is complete
  *)
  let sync_open_orders () =
    let exchange_orders = K.Kraken_incoming_data.get_all_open_orders () in
    Hashtbl.clear open_orders;
    Hashtbl.iter (fun order_id (order : K.Kraken_common_types.order) ->
      Hashtbl.add open_orders order_id order
    ) exchange_orders;
    Lwt.return_unit

  (** Handle execution events (fills, cancellations, acknowledgments).

      Processes order fills by recreating orders after completion.
      Handles order cancellations and acknowledgments appropriately.
      Only recreates orders when buy orders are filled (not sell orders).

      @param runtime_cfg Runtime configuration
      @param cmd_buffer Command buffer for new orders
      @param symbols List of symbols using orderbook strategy
      @param event Market event to process
      @return Unit promise when event is processed
  *)
  let handle_execution runtime_cfg cmd_buffer symbols (event: Core.market_event) =
    match event with
    | Core.Fill { order_id; symbol; price; qty; side; _ } ->
        if List.mem symbol symbols then (
          info_f ~section "Fill event received for %s: order_id=%s side=%s qty=%s price=%s"
            symbol order_id
            (match side with Buy -> "BUY" | Sell -> "SELL")
            (Primitives.Qty.to_string qty)
            (Primitives.Price.to_string price) >>= fun () ->
          
          (* Look up the original order to get its details *)
          match Hashtbl.find_opt open_orders order_id with
          | Some (order : K.Kraken_common_types.order) ->
              let order_side_str = match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown" in
              info_f ~section "Found original order %s: symbol=%s side=%s price=%.8f"
                order_id order.order_symbol order_side_str order.limit_price >>= fun () ->
              
              (* Sync orders first to get the latest state *)
              sync_open_orders () >>= fun () ->
              
              (* Check if the order still exists after sync - if not, it was completely filled *)
              if not (Hashtbl.mem open_orders order_id) then (
                info_f ~section "Order %s completely filled" order_id >>= fun () ->

                (* Only create new orders if it was a buy order that was filled *)
                if order.side = Some Core.Buy then (
                  info_f ~section "Buy order %s filled, creating new orders for %s" order_id symbol >>= fun () ->
                  create_initial_order runtime_cfg symbol cmd_buffer
                ) else (
                  info_f ~section "Sell order %s filled, no new orders needed" order_id >>= fun () ->
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
              create_initial_order runtime_cfg symbol cmd_buffer
        ) else (
          debug_f ~section "Fill event for %s not in orderbook symbols, ignoring" symbol >>= fun () ->
          Lwt.return_unit
        )
    | Ack { order_id; client_id; state; _ } ->
        debug_f ~section "Ack event received: order_id=%s client_id=%s state=%s"
          order_id client_id
          (match state with Open -> "Open" | Filled -> "Filled" | Canceled -> "Canceled" | Rejected -> "Rejected") >>= fun () ->
        
        (match state with
        | Canceled | Rejected ->
            (* Find the order by client_id to determine its symbol and side *)
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
                  info_f ~section "Buy order %s cancelled/rejected, creating new orders for %s" order_id symbol >>= fun () ->
                  sync_open_orders () >>= fun () ->
                  create_initial_order runtime_cfg symbol cmd_buffer
                ) else (
                  info_f ~section "Sell order %s cancelled/rejected, no new orders needed" order_id >>= fun () ->
                  Lwt.return_unit
                )
            | _ ->
                debug_f ~section "Ack event for order %s not in orderbook symbols or not found" order_id >>= fun () ->
                Lwt.return_unit)
        | Filled ->
            (* This is a final Fill confirmation - sync orders and check if we need to create new orders *)
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
                  info_f ~section "Buy order %s fully filled, creating new orders for %s" order_id order.order_symbol >>= fun () ->
                  create_initial_order runtime_cfg order.order_symbol cmd_buffer
                ) else (
                  info_f ~section "Sell order %s fully filled, no new orders needed" order_id >>= fun () ->
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

  (** Load existing orders from exchange and initialize orderbook state.

      Synchronizes local state with exchange orders for orderbook strategy symbols.
      Only loads orders for symbols configured with Orderbook strategy.

      @param runtime_cfg Runtime configuration containing asset settings
      @return Unit promise when initialization is complete
  *)
  let initialize_orders (runtime_cfg : Config.runtime_cfg) =
    let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
      match asset.strategy with
      | Config.Orderbook -> Some asset.symbol
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

  (** Update price information for a symbol.

      @param tick New price tick data to store
      @return Unit promise
  *)
  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    State.update_global_price tick.symbol tick.current_price;
    Lwt.return_unit
end

(** Main entry point for the top-level orderbook market making strategy.

    Initializes the market making strategy by:
    - Loading existing orders from exchange
    - Setting up parallel processing of ticks and executions
    - Maintaining orders at the top of the bid/ask spread
    - Continuously adjusting buy orders to stay at best bid price

    @param runtime_cfg Runtime configuration containing asset settings
    @param _core_cfg Unused core engine configuration
    @param tick_buffer Buffer for receiving price tick updates
    @param cmd_buffer Buffer for submitting orders to exchange
    @param exec_buffer Buffer for receiving order execution confirmations
    @return Never returns (infinite processing loops)
*)
let start (runtime_cfg : Config.runtime_cfg) (core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer =
  info_f ~section "Starting orderbook market making strategy" >>= fun () ->

  K.Kraken_incoming_data.wait_for_snapshot () >>= fun () ->
  K.Kraken_incoming_data.wait_for_instruments () >>= fun () ->

  State.refresh_usd_balance core_cfg >>= fun () ->
  State.initialize_orders runtime_cfg >>= fun () ->

  let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
    match asset.strategy with
    | Config.Orderbook -> Some asset.symbol
    | Config.Grid -> None
  ) runtime_cfg.assets in

  info_f ~section
    "Starting orderbook strategy for symbols: [%s]" (String.concat ", " orderbook_symbols) >>= fun () ->

  let rec execution_loop () =
    Ringbuffer.pop exec_buffer >>= fun event ->
    State.handle_execution runtime_cfg cmd_buffer orderbook_symbols event >>= fun () ->
    execution_loop ()
  in

  let balance_refresh_interval = 300.0 in (* 5 minutes *)
  let rec balance_loop () =
    Lwt_unix.sleep balance_refresh_interval >>= fun () ->
    State.refresh_usd_balance core_cfg >>= fun () ->
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