(* src/dio_engine/trade_strategies/kraken_suicide_grid.ml *)

(*
  GRID TRADING STRATEGY FOR KRAKEN

  This strategy maintains a grid of buy and sell orders around the current market price.
  Orders are spaced according to configured grid intervals and automatically adjusted
  when market conditions change.

  GRID STRUCTURE:
  - Buy orders placed below current price at configured grid intervals
  - Sell orders placed above current price with multiplier-adjusted quantities
  - Grid spacing determined by asset-specific grid_interval configuration

  ORDER MANAGEMENT:
  - Creates initial buy/sell order pairs when strategy starts
  - Adjusts buy orders if price moves too far (beyond 2x grid interval)
  - Recreates buy orders after they are filled
  - Verifies grid spacing and amends orders when necessary
*)

(*
  ARCHITECTURAL OVERVIEW

  The grid strategy maintains persistent buy and sell orders in a structured pattern:

  COMPONENTS:
  - State Management: Tracks open orders, prices, and pending amendments
  - Order Creation: Generates exchange-compliant orders with proper precision
  - Grid Adjustment: Monitors price movements and adjusts orders when needed
  - Execution Handling: Processes fills and recreates orders as required
  - Grid Verification: Ensures optimal spacing between buy and sell orders

  BEHAVIOR:
  - Maintains one buy order below current price 
  - Adjusts buy order if it gets too far from current price (>2x grid interval)
  - Recreates buy order after fill to maintain grid structure
  - Verifies grid spacing and corrects when spread becomes suboptimal
*)

open Lwt.Infix  
open Dio_types 
open Lwt_log_core 

module K = Kraken 


let section = Lwt_log_core.Section.make "engine.strategy.kraken.suicide_grid" 

(*
  STATE MANAGEMENT MODULE

  Manages the internal state of the grid trading strategy including:
  - Current price information for all tracked symbols
  - Open orders synchronized with the exchange
  - Pending order amendments awaiting confirmation
  - Initialization status for each trading symbol
*)

module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16

  let open_orders : (string, K.Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16

  let pending_amends : (string, (Primitives.Price.t * Primitives.Qty.t)) Hashtbl.t = Hashtbl.create 16

  let initialized_symbols : (string, bool) Hashtbl.t = Hashtbl.create 16

  type open_order = {
    order_id: string;
    symbol: string;
    side: Core.side;
    status: Core.order_state;
    limit_price: float;
  }

  (** Check if a symbol has both buy and sell orders open.

      @param symbol Trading symbol to check
      @return true if both buy and sell orders exist for the symbol
  *)
  let has_open_orders symbol =
    let has_buy = ref false in
    let has_sell = ref false in
    Hashtbl.iter (fun _ (order : K.Kraken_common_types.order) ->
      if String.equal order.order_symbol symbol then
        match order.side with
        | Some Core.Buy -> has_buy := true
        | Some Core.Sell -> has_sell := true
        | None -> ()
    ) open_orders;
    !has_buy && !has_sell

  (** Check if a symbol has an active buy order.

      @param symbol Trading symbol to check
      @return true if a buy order exists for the symbol
  *)
  let has_buy_order symbol =
    let found_buy_order = ref false in
    Hashtbl.iter (fun _ (order : K.Kraken_common_types.order) ->
      if not !found_buy_order then
        if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
          found_buy_order := true
    ) open_orders;
    !found_buy_order

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
      @return Formatted Core.order_cmd ready for submission
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
          tags = [`Grid];
        } in
        Lwt_log_core.debug_f ~section
          "Created order: client_id=%s symbol=%s side=%s price=%s qty=%s tags=[Grid]"
            client_id
            symbol
            (match side with Buy -> "BUY" | Sell -> "SELL")
            price_str
            qty_str |> ignore;
        order
    | None ->
        Lwt_main.run (error_f ~section "Precisions not found for symbol: %s. Cannot create order." symbol);
        failwith ("Precision data missing for symbol: " ^ symbol)

  (** Create initial buy/sell order pair for a grid symbol.

      Establishes the baseline grid orders for a symbol by:
      - Calculating grid spacing based on configured interval
      - Creating sell order above current price with adjusted quantity
      - Creating buy order below current price with base quantity
      - Ensuring orders meet exchange precision requirements

      @param runtime_cfg Runtime configuration containing asset settings
      @param symbol Trading symbol to create orders for
      @param cmd_buffer Command buffer for order submission
      @return Unit promise when orders are created and submitted
  *)
  let create_initial_orders : Config.runtime_cfg -> string -> Core.order_cmd Ringbuffer.t -> unit Lwt.t =
    fun runtime_cfg symbol cmd_buffer ->
      if Hashtbl.mem initialized_symbols symbol && not (has_buy_order symbol) then
        match get_price symbol with
        | Some tick ->
            let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
              String.equal asset.symbol symbol
            ) runtime_cfg.assets in

            (match asset_cfg_opt with
            | Some asset_cfg ->
                (match asset_cfg.grid_interval, asset_cfg.sell_mult with
                | Some grid_interval, Some sell_mult ->
                    let current_price = tick.current_price in
                    let current_price_float =
                      Float.of_string (Primitives.Price.to_string current_price) in

                    let grid_pct =
                      Float.of_string (Primitives.Fixed.to_string grid_interval) in

                    let sell_price_raw = current_price_float *. (1.0 +. (grid_pct /. 100.0)) in
                    let buy_price_raw = current_price_float *. (1.0 -. (grid_pct /. 100.0)) in

                    let sell_price = Primitives.Price.of_string_exn ~scale:current_price.scale
                      (Printf.sprintf "%.*f" current_price.scale sell_price_raw) in
                    let buy_price = Primitives.Price.of_string_exn ~scale:current_price.scale
                      (Printf.sprintf "%.*f" current_price.scale buy_price_raw) in

                    let base_qty_float = Float.of_string (Primitives.Qty.to_string asset_cfg.qty) in
                    let sell_mult_float = Float.of_string (Primitives.Fixed.to_string sell_mult) in
                    let sell_qty_float = base_qty_float *. sell_mult_float in
                    let sell_qty = match K.Kraken_incoming_data.get_precisions symbol with
                      | Some (_, qty_prec) ->
                          Primitives.Qty.of_string_exn ~scale:qty_prec
                            (Printf.sprintf "%.*f" qty_prec sell_qty_float)
                      | None ->
                          Primitives.Qty.of_string_exn ~scale:asset_cfg.qty.scale
                            (Printf.sprintf "%.*f" asset_cfg.qty.scale sell_qty_float)
                    in

                    info_f ~section
                      "Order quantities for %s: buy=%.8f sell=%.8f (mult=%.3f -> %.8f * %.3f = %.8f)"
                        symbol
                        base_qty_float
                        sell_qty_float
                        sell_mult_float
                        base_qty_float
                        sell_mult_float
                        (base_qty_float *. sell_mult_float) >>= fun () ->

                    let sell_cmd = create_order ~symbol ~side:Sell ~price:sell_price ~qty:sell_qty in
                    Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                    (match sell_cmd with
                    | Add order ->
                        debug_f ~section
                          "Successfully pushed sell order to cmd_buffer: client_id=%s symbol=%s price=%s qty=%s"
                            order.client_id
                            order.symbol
                            (Primitives.Price.to_string order.price)
                            (Primitives.Qty.to_string order.qty)
                    | _ -> Lwt.return_unit) >>= fun () ->

                    let buy_cmd = create_order ~symbol ~side:Buy ~price:buy_price ~qty:asset_cfg.qty in
                    Ringbuffer.push cmd_buffer buy_cmd >>= fun () ->
                    (match buy_cmd with
                    | Add order ->
                        debug_f ~section
                          "Successfully pushed buy order to cmd_buffer: client_id=%s symbol=%s price=%s qty=%s" 
                            order.client_id
                            order.symbol
                            (Primitives.Price.to_string order.price)
                            (Primitives.Qty.to_string order.qty)
                    | _ -> Lwt.return_unit) >>= fun () ->
                    
                    Lwt.return_unit
                | _, _ ->
                    warning_f ~section
                      "Grid strategy requires grid_interval and sell_mult for %s" symbol >>= fun () -> Lwt.return_unit)
            | None ->
                warning_f ~section
                  "No configuration found for symbol %s in runtime_cfg" symbol >>= fun () -> Lwt.return_unit)
        | None ->
            warning_f ~section
              "No price info available for %s, skipping order creation" symbol >>= fun () -> Lwt.return_unit
      else
        Lwt.return_unit

  (** Update price information for a symbol.

      @param tick New price tick data to store
      @return Unit promise
  *)
  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    Lwt.return_unit 

  (** Check and adjust grid orders based on current market conditions.

      Monitors the distance between buy orders and current market price.
      If the price has moved too far from the buy order (beyond 2x grid interval),
      adjusts the buy order price to maintain optimal grid spacing.

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
    | Some asset_cfg ->
        (match asset_cfg.grid_interval with
        | Some grid_interval ->
            let current_price_float = Float.of_string (Primitives.Price.to_string tick.current_price) in
            let grid_pct = Float.of_string (Primitives.Fixed.to_string grid_interval) in
            let max_distance_pct = grid_pct *. 2.0 in

            debug_f ~section
              "Checking orders for %s - Current Price: %.8f, Grid Interval: %.2f%%, Max Distance: %.2f%%"
                tick.symbol current_price_float grid_pct max_distance_pct >>= fun () ->

            let orders = Hashtbl.to_seq_values (K.Kraken_incoming_data.get_all_open_orders ()) |> List.of_seq in

            debug_f ~section
              "Found %d open orders to check" (List.length orders) >>= fun () ->

            Lwt_list.iter_s (fun (order : K.Kraken_common_types.order) ->
              debug_f ~section
                "Examining order %s: symbol=%s side=%s price=%.8f" 
                  order.order_id 
                  order.order_symbol
                  (match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown")
                  order.limit_price >>= fun () ->
                  
              if String.equal order.order_symbol tick.symbol && 
                 (match order.side with Some Buy -> true | _ -> false) then
                 let price_diff_pct = 
                   ((order.limit_price -. current_price_float) /. current_price_float) *. -100.0 in
                   
                 debug_f ~section
                   "Order %s price difference: %.2f%% (max allowed: %.2f%%)"
                     order.order_id price_diff_pct max_distance_pct >>= fun () ->

                 if price_diff_pct > max_distance_pct then
                   let new_price_float = current_price_float *. (1.0 -. grid_pct /. 100.0) in
                   
                   (if String.equal tick.symbol "USDG/USD" then
                     debug_f ~section:(Lwt_log_core.Section.make "engine.strategy.grid_verify")
                       "USDG/USD adjust: raw_new_float=%.8f"
                         new_price_float
                   else Lwt.return_unit) >>= fun () ->

                   let new_price = match K.Kraken_incoming_data.get_precisions tick.symbol with
                     | Some (price_prec, _) ->
                         Primitives.Price.of_string_exn ~scale:price_prec
                           (Printf.sprintf "%.*f" price_prec new_price_float)
                     | None -> 
                         Primitives.Price.of_string_exn ~scale:tick.current_price.scale
                           (Printf.sprintf "%.*f" tick.current_price.scale new_price_float)
                   in
                   
                   (if String.equal tick.symbol "USDG/USD" then
                     debug_f ~section:(Lwt_log_core.Section.make "engine.strategy.grid_verify")
                       "USDG/USD adjust: formatted_new=%s (scale=%d)"
                         (Primitives.Price.to_string new_price)
                         new_price.scale
                   else Lwt.return_unit) >>= fun () ->
                   
                   let current_qty = Primitives.Qty.of_string_exn ~scale:8 (Printf.sprintf "%.8f" order.qty) in

                   let amend_cmd = Core.Amend {
                     dst = "kraken";
                     order_id = order.order_id; 
                     symbol = order.order_symbol; 
                     new_price = new_price; 
                     new_qty = current_qty; 
                     ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
                   } in

                   info_f ~section
                     "Adjusting order %s price from %.2f to %.2f (current: %.2f, diff: %.1f%%)"
                       order.order_id
                       order.limit_price
                       new_price_float
                       current_price_float
                       price_diff_pct >>= fun () ->

                   Ringbuffer.push cmd_buffer amend_cmd >>= fun () ->
                   debug_f ~section
                     "Amend command %s pushed to buffer: %b"
                       order.order_id true >>= fun () ->

                   if not true then
                     warning_f ~section
                       "Command buffer full! Dropping amend command."
                   else
                     Lwt.return_unit
                 else
                   debug_f ~section
                     "Order %s within acceptable range" order.order_id >>= fun () ->
                   Lwt.return_unit
              else
                debug_f ~section
                  "Skipping order %s (wrong symbol or side)" order.order_id >>= fun () ->
                Lwt.return_unit
            ) orders
        | None ->
            warning_f ~section
              "Grid strategy requires grid_interval for %s" tick.symbol >>= fun () -> Lwt.return_unit)
    | None ->
        warning_f ~section
          "No configuration found for symbol %s in runtime_cfg" tick.symbol >>= fun () -> Lwt.return_unit

  (** Synchronize local order state with exchange.

      Updates local order tracking to match current exchange state.
      Handles order additions, removals, and modifications.

      @param runtime_cfg Runtime configuration
      @param cmd_buffer Command buffer for any required actions
      @param unit Unit parameter (for Lwt compatibility)
      @return Unit promise when synchronization is complete
  *)
  let sync_open_orders runtime_cfg cmd_buffer () =
    let exchange_orders = K.Kraken_incoming_data.get_all_open_orders () in
    let updated_symbols = Hashtbl.create 16 in

    Hashtbl.iter (fun order_id (order : K.Kraken_common_types.order) ->
      if not (Hashtbl.mem exchange_orders order_id) then (
        Hashtbl.add updated_symbols order.order_symbol true;
        Hashtbl.remove open_orders order_id;
        info_f ~section "Removed order %s from local state (no longer on exchange)" order_id |> Lwt.ignore_result
      )
    ) open_orders;

    Hashtbl.iter (fun order_id (order : K.Kraken_common_types.order) ->
      match Hashtbl.find_opt open_orders order_id with
      | Some existing_order ->
          if existing_order.limit_price <> order.limit_price then (
            debug_f ~section
              "Order %s price changed from %.8f to %.8f"
                order_id existing_order.limit_price order.limit_price |> Lwt.ignore_result;
            Hashtbl.add updated_symbols order.order_symbol true
          );
      | None ->
          Hashtbl.add updated_symbols order.order_symbol true;
          info_f ~section "Added new order %s to local state from exchange sync" order_id |> Lwt.ignore_result
      ;
      Hashtbl.replace open_orders order_id order
    ) exchange_orders;

    Hashtbl.iter (fun symbol _ ->
      match get_price symbol with
      | Some tick ->
          debug_f ~section
            "Checking orders after sync for %s" symbol |> Lwt.ignore_result;
          check_and_adjust_orders runtime_cfg cmd_buffer tick |> Lwt.ignore_result
      | None -> ()
    ) updated_symbols;

    Lwt.return_unit

  (** Handle execution events (fills, cancellations, acknowledgments).

      Processes order fills by recreating orders after completion.
      Handles cancellations and acknowledgments appropriately.

      @param runtime_cfg Runtime configuration
      @param cmd_buffer Command buffer for new orders
      @param grid_symbols List of symbols using grid strategy
      @param event Market event to process
      @return Unit promise when event is processed
  *)
  let handle_execution runtime_cfg cmd_buffer grid_symbols (event : Core.market_event) =
    match event with
    | Core.Fill { order_id; symbol; price; qty; side; _ } ->
        if List.mem symbol grid_symbols then (
          match Hashtbl.find_opt open_orders order_id with
          | Some (order : K.Kraken_common_types.order) ->
            let side_str = match side with Buy -> "BUY" | Sell -> "SELL" in
            let order_side_str =
              match order.side with
              | Some Buy -> "Buy"
              | Some Sell -> "Sell"
              | None -> "unknown"
            in
            
            info_f ~section
              "Order %s filled: %s %s %s @ %s (original side: %s)"
                order_id
                side_str
                (Primitives.Qty.to_string qty)
                symbol
                (Primitives.Price.to_string price)
                order_side_str >>= fun () ->
            sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
            if not (Hashtbl.mem open_orders order_id) then (
              info_f ~section
                "Order %s completely filled" order_id >>= fun () ->
              if order.side = Some Core.Buy then (
                info_f ~section
                  "Buy order %s filled, creating new orders for %s" order_id symbol >>= fun () ->
                create_initial_orders runtime_cfg symbol cmd_buffer
              ) else (
                info_f ~section
                  "Sell order %s filled, no action needed" order_id >>= fun () ->
                Lwt.return_unit
              )
            ) else (
              debug_f ~section
                "Order %s partially filled, order still exists" order_id >>= fun () ->
              Lwt.return_unit
            )
          | None -> Lwt.return_unit
        ) else (
          Lwt.return_unit
        )
    | Ack { order_id; state; _ } ->
        begin match Hashtbl.find_opt open_orders order_id with
        | Some order ->
            let symbol = order.order_symbol in
            if List.mem symbol grid_symbols then (
            match state with
            | Canceled | Rejected ->
                Hashtbl.remove open_orders order_id;
                info_f ~section
                  "Order %s %s" order_id
                    (match state with Canceled -> "canceled" | Rejected -> "rejected" | _ -> "") >>= fun () ->
                if order.side = Some Core.Buy then (
                  info_f ~section
                    "Buy order %s cancelled/rejected, creating new orders for %s" order_id symbol >>= fun () ->
                  sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
                  create_initial_orders runtime_cfg symbol cmd_buffer
                ) else (
                  info_f ~section
                    "Sell order %s cancelled/rejected, no action needed" order_id >>= fun () ->
                  Lwt.return_unit
                )
            | Open ->
                begin match Hashtbl.find_opt pending_amends order_id with
                | Some (new_price, new_qty) ->
                    debug_f ~section "Processing confirmed amend for order %s" order_id >>= fun () ->
                    begin match Hashtbl.find_opt open_orders order_id with
                    | Some order ->
                        let updated_order = { order with
                          limit_price = float_of_string (Primitives.Price.to_string new_price);
                          qty = float_of_string (Primitives.Qty.to_string new_qty);
                        } in
                        Hashtbl.replace open_orders order_id updated_order;
                        Hashtbl.remove pending_amends order_id;
                        Lwt.return_unit
                    | None ->
                        Hashtbl.remove pending_amends order_id;
                        sync_open_orders runtime_cfg cmd_buffer ()
                    end
                | None ->
                    debug_f ~section
                      "Order %s state updated to Open - syncing orders" order_id >>= fun () ->
                    sync_open_orders runtime_cfg cmd_buffer ()
                end
            | Filled ->
                
                sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
                info_f ~section
                  "Order %s fully filled (Ack confirmation)" order_id >>= fun () ->
                if order.side = Some Core.Buy then (
                  info_f ~section
                    "Buy order %s fully filled, creating new orders for %s" order_id symbol >>= fun () ->
                  create_initial_orders runtime_cfg symbol cmd_buffer
                ) else (
                  info_f ~section
                    "Sell order %s fully filled, no action needed" order_id >>= fun () ->
                  Lwt.return_unit
                )
            ) else (
              Lwt.return_unit
            )
        | None -> Lwt.return_unit (* No local order found, likely an old ack, ignore *)
        end
    | _ -> Lwt.return_unit

  (** Load existing orders from exchange and initialize grid state.

      Synchronizes local state with exchange orders for grid strategy symbols.
      Marks symbols as initialized when orders are loaded.

      @param runtime_cfg Runtime configuration containing asset settings
      @return Unit promise when initialization is complete
  *)
  let initialize_orders (runtime_cfg : Config.runtime_cfg) =
    let grid_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
      match asset.strategy with
      | Config.Grid -> Some asset.symbol
      | Config.Orderbook -> None
    ) runtime_cfg.assets in

    List.iter (fun symbol -> Hashtbl.replace initialized_symbols symbol false) grid_symbols;

    let exchange_orders = K.Kraken_incoming_data.get_all_open_orders () in
    Hashtbl.clear open_orders;
    let log_promises = Hashtbl.fold (fun order_id (order : K.Kraken_common_types.order) promises -> 
      let log_promise = 
        let symbol_str = order.order_symbol in 
        if symbol_str <> "N/A" && List.mem symbol_str grid_symbols then ( 
          Hashtbl.replace open_orders order_id order;
          Hashtbl.replace initialized_symbols symbol_str true; 
          info_f ~section
            "Loaded existing order %s for %s" order_id symbol_str
        ) else (
          warning_f ~section
            "Order %s not for grid strategy symbol, skipping" order_id
        )
      in
      log_promise :: promises
    ) exchange_orders [] in
    
    Lwt.join log_promises >>= fun () ->
    info_f ~section
      "Initialized %d open orders from exchange" (Hashtbl.length open_orders)

  (** Verify and correct grid spacing between buy and sell orders.

      Checks if the spread between highest buy and lowest sell orders matches
      the configured grid interval. If not, amends the buy order to correct spacing.

      @param runtime_cfg Runtime configuration with asset settings
      @param symbol Trading symbol to verify
      @param cmd_buffer Command buffer for order amendments
      @param current_market_price_float Current market price as float
      @return Unit promise when verification is complete
  *)
  let verify_grid_spacing (runtime_cfg : Config.runtime_cfg) (symbol : string) (cmd_buffer : Core.order_cmd Ringbuffer.t) (current_market_price_float : float) : unit Lwt.t =
    let verify_section = Lwt_log_core.Section.make "engine.strategy.grid_verify" in
    match List.find_opt (fun (asset : Config.asset_cfg) -> String.equal asset.symbol symbol) runtime_cfg.assets with
    | None ->
        warning_f ~section:verify_section
          "Grid Verify [%s]: No asset config found." symbol
    | Some asset_cfg ->
        (match asset_cfg.grid_interval with
        | Some grid_interval ->
            let open_orders_for_symbol =
              Hashtbl.to_seq_values open_orders
              |> List.of_seq
              |> List.filter (fun (o : K.Kraken_common_types.order) -> String.equal o.order_symbol symbol)
            in

            let buy_orders = List.filter (fun (o : K.Kraken_common_types.order) -> o.side = Some Core.Buy) open_orders_for_symbol in
            let sell_orders = List.filter (fun (o : K.Kraken_common_types.order) -> o.side = Some Core.Sell) open_orders_for_symbol in

            if List.length buy_orders > 0 && List.length sell_orders > 0 then
              let highest_buy_order =
                List.fold_left (fun (acc : K.Kraken_common_types.order) (curr : K.Kraken_common_types.order) ->
                  if curr.limit_price > acc.limit_price then curr else acc
                ) (List.hd buy_orders) (List.tl buy_orders)
              in
              let lowest_sell_order =
                List.fold_left (fun (acc : K.Kraken_common_types.order) (curr : K.Kraken_common_types.order) ->
                  if curr.limit_price < acc.limit_price then curr else acc
                ) (List.hd sell_orders) (List.tl sell_orders)
              in

              let max_buy_price_float = highest_buy_order.limit_price in
              let min_sell_price_float = lowest_sell_order.limit_price in

              (* Skip verification if sell price is below buy price - this would be an invalid grid state *)
              if min_sell_price_float <= max_buy_price_float then
                Lwt.return_unit
              else
                let actual_spread_value = min_sell_price_float -. max_buy_price_float in
                let p_mid_reference = (min_sell_price_float +. max_buy_price_float) /. 2.0 in

                (* Skip spread calculation if midpoint reference price is invalid (zero or negative) *)
                if p_mid_reference <= 0.0 then
                  Lwt.return_unit
                else
                  let actual_spread_pct_of_mid = (actual_spread_value /. p_mid_reference) *. 100.0 in
                  let configured_grid_interval_pct =
                    Float.of_string (Primitives.Fixed.to_string grid_interval)
                  in
                  let expected_total_spread_pct = 2.0 *. configured_grid_interval_pct in
                  let tolerance_pct = 0.01 (* Tolerance for comparison *) in
                  let diff_pct = abs_float (actual_spread_pct_of_mid -. expected_total_spread_pct) in

                  if diff_pct <= tolerance_pct then
                    info_f ~section:verify_section
                      "Grid Verify [%s]: < Tolerance Threshold."
                      symbol
                  else
                    (* Grid check FAILED, attempt to amend the highest buy order *)
                    let new_target_buy_price_float = min_sell_price_float *. (1.0 -. (expected_total_spread_pct /. 100.0)) in

                    if new_target_buy_price_float >= current_market_price_float then
                      info_f ~section:verify_section
                      "Grid Verify [%s]: PASSED."
                        symbol
                    else
                      (* Safe to amend, check precision and if new price is actually different *)
                      match K.Kraken_incoming_data.get_precisions symbol with
                      | None ->
                          error_f ~section:verify_section
                            "Grid Verify [%s]: No Precision."
                            symbol
                      | Some (price_prec, qty_prec) ->
                          let new_buy_price_primitive =
                            Primitives.Price.of_string_exn ~scale:price_prec
                              (Printf.sprintf "%.*f" price_prec new_target_buy_price_float)
                          in
                          
                          (* Compare formatted strings directly to avoid precision loss *)
                          let existing_price_formatted = Printf.sprintf "%.*f" price_prec highest_buy_order.limit_price in
                          let new_price_formatted = Printf.sprintf "%.*f" price_prec new_target_buy_price_float in
                          
                          (* see the actual values being compared *)
                          debug_f ~section:verify_section
                            "Grid Verify [%s] DEBUG: existing_price=%.8f (formatted: %s), new_price=%.8f (formatted: %s), equal=%b"
                            symbol
                            highest_buy_order.limit_price
                            existing_price_formatted
                            new_target_buy_price_float
                            new_price_formatted
                            (String.equal existing_price_formatted new_price_formatted) >>= fun () ->
                          Lwt.return_unit >>= fun () ->
                          
                          if String.equal existing_price_formatted new_price_formatted then
                            info_f ~section:verify_section
                              "Grid Verify [%s]: PASSED."
                              symbol
                          else
                            (* Check if price difference is meaningful enough for amendment *)
                            let existing_formatted_float = float_of_string existing_price_formatted in
                            let new_formatted_float = float_of_string new_price_formatted in
                            let price_diff = abs_float (existing_formatted_float -. new_formatted_float) in
                            let min_price_diff = 10.0 ** (-.float_of_int price_prec) *. 2.0 in (* 2x the precision minimum *)

                            debug_f ~section:verify_section
                              "Grid Verify [%s] DEBUG: price_diff=%.8f, min_diff=%.8f, amendable=%b"
                              symbol
                              price_diff
                              min_price_diff
                              (price_diff >= min_price_diff) >>= fun () ->

                            if price_diff < min_price_diff then
                              info_f ~section:verify_section
                                "Grid Verify [%s]: SKIPPED (price diff too small)."
                                symbol
                            else
                              (* Prices are different and difference is meaningful, proceed with amend *)
                              let existing_qty_primitive =
                                 Primitives.Qty.of_string_exn ~scale:qty_prec (Printf.sprintf "%.*f" qty_prec highest_buy_order.qty)
                              in
                              let amend_cmd = Core.Amend {
                                dst = "kraken";
                                order_id = highest_buy_order.order_id;
                                symbol = symbol;
                                new_price = new_buy_price_primitive;
                                new_qty = existing_qty_primitive;
                                ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
                              } in

                              if Hashtbl.mem pending_amends highest_buy_order.order_id then
                                debug_f ~section:verify_section
                                  "Grid Verify [%s]: Skipping amend for %s, already pending."
                                  symbol highest_buy_order.order_id
                              else (
                                Hashtbl.add pending_amends highest_buy_order.order_id (new_buy_price_primitive, existing_qty_primitive);
                                Ringbuffer.push cmd_buffer amend_cmd >>= fun () ->
                                info_f ~section:verify_section
                                  "Grid Verify [%s]: FAILED & AMENDING."
                                  symbol
                              )
            else
              info_f ~section:verify_section
                "Grid Verify [%s]: Skipping, not enough buy/sell orders to form a grid (Buys: %d, Sells: %d)."
                symbol (List.length buy_orders) (List.length sell_orders)
        | None ->
            warning_f ~section:verify_section
              "Grid Verify [%s]: grid_interval not configured for asset."
              symbol)

  (** Get list of all open orders in simplified format.

      Converts exchange order format to local open_order type.
      Filters out orders with invalid sides.

      @return List of open orders with simplified structure
  *)
  let get_open_orders () : open_order list =
    let all_feed_orders = K.Kraken_incoming_data.get_all_open_orders () in
    let orders = Hashtbl.to_seq_values all_feed_orders |> List.of_seq in
    List.filter_map (fun (order : K.Kraken_common_types.order) ->
      match order.side with
      | Some s -> Some {
          order_id = order.order_id;
          symbol = order.order_symbol;
          side = s;
          status = order.status;
          limit_price = order.limit_price;
        }
      | None ->
          Lwt_main.run (error_f ~section "Invalid side in order: %s (Order ID: %s)" order.order_id order.order_id);
          None
    ) orders
end

(** Main entry point for the grid trading strategy.

    Initializes the grid strategy by:
    - Loading existing orders from exchange
    - Setting up parallel processing of ticks and executions
    - Maintaining grid orders and adjusting them as needed

    @param runtime_cfg Runtime configuration containing asset settings
    @param _core_cfg Unused core engine configuration
    @param tick_buffer Buffer for receiving price tick updates
    @param cmd_buffer Buffer for submitting orders to exchange
    @param exec_buffer Buffer for receiving order execution confirmations
    @return Never returns (infinite processing loops)
*)
let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer =
  info_f ~section
    "Strategy received runtime_cfg: %s"
       (Yojson.Safe.to_string (Config.runtime_cfg_to_yojson runtime_cfg)) >>= fun () ->

  info_f ~section
    "Waiting for execution snapshot from Kraken..." >>= fun () ->
  K.Kraken_incoming_data.wait_for_snapshot () >>= fun () ->
  info_f ~section
    "Execution snapshot received, initializing strategy state..." >>= fun () ->

  info_f ~section
    "Waiting for instrument data from Kraken..." >>= fun () ->
  K.Kraken_incoming_data.wait_for_instruments () >>= fun () ->
  info_f ~section
    "Instrument data received." >>= fun () ->

  State.initialize_orders runtime_cfg >>= fun () ->

  let grid_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
    match asset.strategy with
    | Config.Grid -> Some asset.symbol
    | Config.Orderbook -> None
  ) runtime_cfg.assets in

  info_f ~section
    "Starting grid strategy for symbols: [%s]" (String.concat ", " grid_symbols) >>= fun () ->

  let rec execution_loop () =
    Ringbuffer.pop exec_buffer >>= fun event ->
    State.handle_execution runtime_cfg cmd_buffer grid_symbols event >>= fun () ->
    execution_loop ()
  in

  let rec tick_loop () =
    Ringbuffer.pop tick_buffer >>= fun (tick : Event.tick) ->
    (if List.mem tick.symbol grid_symbols then (
      let (should_update, bid_changed, ask_changed) =
        match State.get_price tick.symbol with
        | Some prev_tick ->
            let bid_changed = not (Primitives.Price.equal prev_tick.bid tick.bid) in
            let ask_changed = not (Primitives.Price.equal prev_tick.ask tick.ask) in
            let should_update = bid_changed || ask_changed in
            (should_update, bid_changed, ask_changed)
        | None -> (true, false, false)
      in
      
      (if bid_changed || ask_changed then
        match State.get_price tick.symbol with
        | Some prev_tick ->
            debug_f ~section
              "Price changed for %s: bid %s->%s, ask %s->%s" tick.symbol
              (Primitives.Price.to_string prev_tick.bid) (Primitives.Price.to_string tick.bid)
              (Primitives.Price.to_string prev_tick.ask) (Primitives.Price.to_string tick.ask)
        | None -> Lwt.return_unit
      else if not should_update then
        debug_f ~section "Price unchanged for %s" tick.symbol
      else
        Lwt.return_unit) >>= fun () ->
      
      if should_update then (
        info_f ~section
          "Processing price update for %s" tick.symbol >>= fun () ->
        State.update_price tick >>= fun () ->
        State.sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
        State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
        (let has_orders = State.has_buy_order tick.symbol in
        debug_f ~section
          "%s has buy order: %b" tick.symbol has_orders >>= fun () ->
        if not has_orders then
          State.create_initial_orders runtime_cfg tick.symbol cmd_buffer
        else
          debug_f ~section
            "Skipping order creation for %s - already has buy order" tick.symbol >>= fun () ->
          Lwt.return_unit) >>= fun () ->
        let current_price_for_verify = Float.of_string (Primitives.Price.to_string tick.current_price) in
        State.verify_grid_spacing runtime_cfg tick.symbol cmd_buffer current_price_for_verify
      ) else (
        debug_f ~section
          "Skipping update for %s - price unchanged" tick.symbol
      )
    ) else (
      debug_f ~section
        "Skipping tick for %s - not a grid strategy symbol" tick.symbol
    )) >>= fun () ->
    tick_loop ()
  in

  Lwt.join [execution_loop (); tick_loop ()]