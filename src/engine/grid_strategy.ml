(*
  Position sizing for BTC: 0.00025 BTC x (unrealized value of portfolio / 100)
  Position sizing for alts: $10 order size min, adjust to maintain new $5 thresholds on 
  increased price movement.
    i.e. if price of alt increases to where buy order = $15, new min threshold for that asset becomes
      $15. If price of alt decreases below min threshold, volume of asset traded increased to meet 
      threshold in USD value.
*)

(* src/engine/strategy.ml *)
open Lwt.Infix  (* for >>= *)
open Dio_types 

module K = Kraken (* To get open orders *)

(* Module-level state *)
module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16

  let open_orders : (string, K.Common.order) Hashtbl.t = Hashtbl.create 16
  
  let initialized_symbols : (string, bool) Hashtbl.t = Hashtbl.create 16

  type open_order = {
    order_id: string;
    symbol: string;
    side: Core.side;
    status: Core.order_state;
    limit_price: float;
  }

  let has_open_orders symbol =
    let has_buy = ref false in
    let has_sell = ref false in
    Hashtbl.iter (fun _ (order : K.Common.order) ->
      if String.equal order.order_symbol symbol then
        match order.side with
        | Some Core.Buy -> has_buy := true
        | Some Core.Sell -> has_sell := true
        | None -> ()
    ) open_orders;
    !has_buy && !has_sell (* Returns true only if both a buy and a sell exist *)

  let has_buy_order symbol =
    let found_buy_order = ref false in
    Hashtbl.iter (fun _ (order : K.Common.order) ->
      if not !found_buy_order then (* Short-circuit if already found *)
        if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
          found_buy_order := true
    ) open_orders;
    !found_buy_order

  let get_price symbol = Hashtbl.find_opt price_info symbol

  let create_order ~symbol ~side ~price ~qty =
    match K.Ws_feed.get_precisions symbol with
    | Some (price_prec, qty_prec) ->
        let price_str = Primitives.Price.to_string price in
        let qty_str = Primitives.Qty.to_string qty in
        (* Reformat price and qty based on fetched precision *) 
        let formatted_price = Primitives.Price.of_string_exn ~scale:price_prec price_str in
        let formatted_qty = Primitives.Qty.of_string_exn ~scale:qty_prec qty_str in
        
        (* Generate client_id based on side and timestamp for uniqueness *)
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
        Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
          (Printf.sprintf "Created order: client_id=%s symbol=%s side=%s price=%s qty=%s tags=[Grid]"
            client_id
            symbol
            (match side with Buy -> "BUY" | Sell -> "SELL")
            price_str
            qty_str) |> ignore;
        order
    | None -> 
        (* Log error and potentially raise an exception or return an error type *)
        let () = Logs.err (fun m -> m "Precisions not found for symbol: %s. Cannot create order." symbol) in
        failwith ("Precision data missing for symbol: " ^ symbol)

  let create_initial_orders : Config.runtime_cfg -> string -> Core.order_cmd Ringbuffer.t -> unit Lwt.t = 
    fun runtime_cfg symbol cmd_buffer ->
      (* Only proceed if we've initialized orders for this symbol *)
      if Hashtbl.mem initialized_symbols symbol && not (has_buy_order symbol) then
        match get_price symbol with
        | Some tick ->
            (* Find the asset configuration for this symbol *)
            let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) -> 
              String.equal asset.symbol symbol
            ) runtime_cfg.assets in

            (match asset_cfg_opt with
            | Some asset_cfg ->
                (* Get current price from stored price info *)
                let current_price = tick.current_price in
                let current_price_float = 
                  Float.of_string (Primitives.Price.to_string current_price) in
                
                (* Convert grid_interval to float percentage *)
                let grid_pct = 
                  Float.of_string (Primitives.Fixed.to_string asset_cfg.grid_interval) in
                
                (* Calculate raw prices using percentage adjustment *)
                let sell_price_raw = current_price_float *. (1.0 +. (grid_pct /. 100.0)) in
                let buy_price_raw = current_price_float *. (1.0 -. (grid_pct /. 100.0)) in
                
                (* Convert back to Fixed point with proper scale *)
                let sell_price = Primitives.Price.of_string_exn ~scale:current_price.scale 
                  (Printf.sprintf "%.*f" current_price.scale sell_price_raw) in
                let buy_price = Primitives.Price.of_string_exn ~scale:current_price.scale 
                  (Printf.sprintf "%.*f" current_price.scale buy_price_raw) in
                
                (* Log the price calculations *)
                Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Calculating grid prices for %s: current=%.2f buy=%.2f sell=%.2f interval=%.1f%%" 
                    symbol
                    current_price_float
                    buy_price_raw
                    sell_price_raw
                    grid_pct) >>= fun () ->
                
                (* Calculate sell quantity by applying sell_mult *)
                let base_qty_float = Float.of_string (Primitives.Qty.to_string asset_cfg.qty) in
                let sell_mult_float = Float.of_string (Primitives.Fixed.to_string asset_cfg.sell_mult) in
                let sell_qty_float = base_qty_float *. sell_mult_float in
                let sell_qty = match K.Ws_feed.get_precisions symbol with
                  | Some (_, qty_prec) ->
                      Primitives.Qty.of_string_exn ~scale:qty_prec
                        (Printf.sprintf "%.*f" qty_prec sell_qty_float)
                  | None ->
                      Primitives.Qty.of_string_exn ~scale:asset_cfg.qty.scale
                        (Printf.sprintf "%.*f" asset_cfg.qty.scale sell_qty_float)
                in

                (* Log quantities with more precision to verify calculation *)
                Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Order quantities for %s: buy=%.8f sell=%.8f (mult=%.3f -> %.8f * %.3f = %.8f)" 
                    symbol
                    base_qty_float
                    sell_qty_float
                    sell_mult_float
                    base_qty_float
                    sell_mult_float
                    (base_qty_float *. sell_mult_float)) >>= fun () ->
                
                (* Create and push sell order first with adjusted quantity *)
                let sell_cmd = create_order ~symbol ~side:Sell ~price:sell_price ~qty:sell_qty in
                (if not (Ringbuffer.push cmd_buffer sell_cmd) then
                  Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy") 
                    "Command buffer full! Dropping sell command."
                else
                  match sell_cmd with
                  | Add order ->
                      Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                        (Printf.sprintf "Successfully pushed sell order to cmd_buffer: client_id=%s symbol=%s price=%s qty=%s" 
                          order.client_id
                          order.symbol
                          (Primitives.Price.to_string order.price)
                          (Primitives.Qty.to_string order.qty))
                  | _ -> Lwt.return_unit) >>= fun () ->

                (* Create and push buy order second with base quantity *)
                let buy_cmd = create_order ~symbol ~side:Buy ~price:buy_price ~qty:asset_cfg.qty in
                (if not (Ringbuffer.push cmd_buffer buy_cmd) then
                  Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy") 
                    "Command buffer full! Dropping buy command."
                else
                  match buy_cmd with
                  | Add order ->
                      Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                        (Printf.sprintf "Successfully pushed buy order to cmd_buffer: client_id=%s symbol=%s price=%s qty=%s" 
                          order.client_id
                          order.symbol
                          (Primitives.Price.to_string order.price)
                          (Primitives.Qty.to_string order.qty))
                  | _ -> Lwt.return_unit) >>= fun () ->
                
                Lwt.return_unit
            | None ->
                Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "No configuration found for symbol %s in runtime_cfg" symbol))
        | None ->
            Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy")
              (Printf.sprintf "No price info available for %s, skipping order creation" symbol)
      else
        Lwt.return_unit

  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    Lwt.return_unit 

  let check_and_adjust_orders (runtime_cfg : Config.runtime_cfg) cmd_buffer (tick : Event.tick) =
    (* Find the asset configuration for this symbol *)
    let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) -> 
      String.equal asset.symbol tick.symbol
    ) runtime_cfg.assets in

    match asset_cfg_opt with
    | Some asset_cfg ->
        let current_price_float = Float.of_string (Primitives.Price.to_string tick.current_price) in
        let grid_pct = Float.of_string (Primitives.Fixed.to_string asset_cfg.grid_interval) in
        let max_distance_pct = grid_pct *. 2.0 in (* 2x grid interval *)
        
        (* Log the price and grid settings *)
        Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
          (Printf.sprintf "Checking orders for %s - Current Price: %.8f, Grid Interval: %.2f%%, Max Distance: %.2f%%" 
            tick.symbol current_price_float grid_pct max_distance_pct) >>= fun () ->
        
        (* Get all open orders for this symbol *)
        let orders = Hashtbl.to_seq_values (K.Ws_feed.get_all_open_orders ()) |> List.of_seq in
        
        (* Log how many orders we're checking *)
        Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
          (Printf.sprintf "Found %d open orders to check" (List.length orders)) >>= fun () ->
        
        (* Process each order *)
        Lwt_list.iter_s (fun (order : K.Common.order) ->
          (* Log each order we're examining *)
          Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
            (Printf.sprintf "Examining order %s: symbol=%s side=%s price=%.8f" 
              order.order_id 
              order.order_symbol
              (match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown")
              order.limit_price) >>= fun () ->
              
          if String.equal order.order_symbol tick.symbol && 
             (match order.side with Some Buy -> true | _ -> false) then
             let price_diff_pct = 
               ((order.limit_price -. current_price_float) /. current_price_float) *. -100.0 in
               
             (* Log the price difference *)
             Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
               (Printf.sprintf "Order %s price difference: %.2f%% (max allowed: %.2f%%)" 
                 order.order_id price_diff_pct max_distance_pct) >>= fun () ->
              
             (* If price difference exceeds 2x grid interval, adjust the order *)
             if price_diff_pct > max_distance_pct then
               let new_price_float = current_price_float *. (1.0 -. grid_pct /. 100.0) in
               
               (if String.equal tick.symbol "USDG/USD" then
                 Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                   (Printf.sprintf "USDG/USD adjust: raw_new_float=%.8f"
                     new_price_float)
               else Lwt.return_unit) >>= fun () ->

               let new_price = match K.Ws_feed.get_precisions tick.symbol with
                 | Some (price_prec, _) ->
                     Primitives.Price.of_string_exn ~scale:price_prec
                       (Printf.sprintf "%.*f" price_prec new_price_float)
                 | None -> 
                     Primitives.Price.of_string_exn ~scale:tick.current_price.scale
                       (Printf.sprintf "%.*f" tick.current_price.scale new_price_float)
               in
               
               (if String.equal tick.symbol "USDG/USD" then
                 Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                   (Printf.sprintf "USDG/USD adjust: formatted_new=%s (scale=%d)"
                     (Primitives.Price.to_string new_price)
                     new_price.scale)
               else Lwt.return_unit) >>= fun () ->
               
               let current_qty = Primitives.Qty.of_string_exn ~scale:8 (Printf.sprintf "%.8f" order.qty) in (* Use order.qty from K.order *)
               
               (* Create amend command *) 
               let amend_cmd = Core.Amend {
                 dst = "kraken";
                 order_id = order.order_id; 
                 symbol = order.order_symbol; 
                 new_price = new_price; 
                 new_qty = current_qty; 
                 ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
               } in
               
               (* Log the adjustment *)
               Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
                 (Printf.sprintf "Adjusting order %s price from %.2f to %.2f (current: %.2f, diff: %.1f%%)"
                   order.order_id
                   order.limit_price
                   new_price_float
                   current_price_float
                   price_diff_pct) >>= fun () ->
               
               (* Push the amend command *)
               let pushed = Ringbuffer.push cmd_buffer amend_cmd in
               Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                 (Printf.sprintf "Amend command %s pushed to buffer: %b" 
                   order.order_id pushed) >>= fun () ->
               
               if not pushed then
                 Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy")
                   "Command buffer full! Dropping amend command."
               else
                 Lwt.return_unit
             else
               Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                 (Printf.sprintf "Order %s within acceptable range" order.order_id) >>= fun () ->
               Lwt.return_unit
          else
            Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
              (Printf.sprintf "Skipping order %s (wrong symbol or side)" order.order_id) >>= fun () ->
            Lwt.return_unit
        ) orders
    | None ->
        Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy")
          (Printf.sprintf "No configuration found for symbol %s in runtime_cfg" tick.symbol)

  let sync_open_orders runtime_cfg cmd_buffer () =
    let exchange_orders = K.Ws_feed.get_all_open_orders () in
    (* Track which orders were updated *)
    let updated_symbols = Hashtbl.create 16 in
    
    (* Remove orders that no longer exist on exchange *)
    Hashtbl.iter (fun order_id (order : K.Common.order) ->
      if not (Hashtbl.mem exchange_orders order_id) then (
        Hashtbl.add updated_symbols order.order_symbol true;
        Hashtbl.remove open_orders order_id
      )
    ) open_orders;
    
    (* Add/update orders from exchange *)
    Hashtbl.iter (fun order_id (order : K.Common.order) ->
      match Hashtbl.find_opt open_orders order_id with
      | Some existing_order ->
          (* Check if order was modified *)
          if existing_order.limit_price <> order.limit_price then (
            Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
              (Printf.sprintf "Order %s price changed from %.8f to %.8f" 
                order_id existing_order.limit_price order.limit_price) |> ignore;
            Hashtbl.add updated_symbols order.order_symbol true
          );
      | None ->
          Hashtbl.add updated_symbols order.order_symbol true;
      ;
      Hashtbl.replace open_orders order_id order
    ) exchange_orders;
    
    (* Check orders for any symbols that had updates *)
    Hashtbl.iter (fun symbol _ ->
      match get_price symbol with
      | Some tick ->
          Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
            (Printf.sprintf "Checking orders after sync for %s" symbol) |> ignore;
          check_and_adjust_orders runtime_cfg cmd_buffer tick |> ignore
      | None -> ()
    ) updated_symbols;
    
    Lwt.return_unit

  let handle_execution runtime_cfg cmd_buffer grid_symbols (event : Core.market_event) =
    match event with
    | Core.Fill { order_id; symbol; price; qty; side; _ } ->
        (* Only process fills for grid strategy symbols *)
        if List.mem symbol grid_symbols then (
          match Hashtbl.find_opt open_orders order_id with
          | Some (order : K.Common.order) ->
            let side_str = match side with Buy -> "BUY" | Sell -> "SELL" in
            let order_side_str = 
              match order.side with
              | Some Buy -> "Buy"
              | Some Sell -> "Sell"
              | None -> "unknown"
            in
            Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
              (Printf.sprintf "Order %s filled: %s %s %s @ %s (original side: %s)" 
                order_id
                side_str
                (Primitives.Qty.to_string qty)
                symbol
                (Primitives.Price.to_string price)
                order_side_str) >>= fun () ->
            (* Sync state after fill to get latest from exchange (e.g., remaining qty or removal if full) *)
            sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
            (* Check if the order still exists after sync - if not, it was completely filled *)
            if not (Hashtbl.mem open_orders order_id) then
              (* Order was completely filled, create new orders if we have none for this symbol *)
              Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
                (Printf.sprintf "Order %s completely filled, checking if new orders needed" order_id) >>= fun () ->
              if not (has_open_orders symbol) then
                create_initial_orders runtime_cfg symbol cmd_buffer
              else
                Lwt.return_unit
            else
              (* Order still exists, so it was a partial fill - don't create new orders yet *)
              Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                (Printf.sprintf "Order %s partially filled, order still exists" order_id) >>= fun () ->
              Lwt.return_unit
          | None -> Lwt.return_unit
        ) else (
          Lwt.return_unit
        )
    | Ack { order_id; state; _ } ->
        begin match Hashtbl.find_opt open_orders order_id with
        | Some order ->
            let symbol = order.order_symbol in
            (* Only process acks for grid strategy symbols *)
            if List.mem symbol grid_symbols then (
            match state with
            | Canceled | Rejected ->
                Hashtbl.remove open_orders order_id;
                Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Order %s %s" order_id 
                    (match state with Canceled -> "canceled" | Rejected -> "rejected" | _ -> "")) >>= fun () ->
                (* After cancellation/rejection, sync, check, and create if needed *)
                sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
                (match get_price symbol with
                | Some tick ->
                    check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
                    if not (has_open_orders symbol) then
                      create_initial_orders runtime_cfg symbol cmd_buffer
                    else Lwt.return_unit
                | None -> Lwt.return_unit)
            | Open ->
                (* When an order is amended/opened (including after partial fills), just sync and check *)
                Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Order %s state updated to Open - syncing orders" order_id) >>= fun () ->
                sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
                (* Don't create new orders on Open state - wait for Filled *)
                Lwt.return_unit
            | Filled ->
                (* For Ack Filled (confirmation after final partial), ensure sync and create if no orders left *)
                sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
                Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Order %s fully filled (Ack confirmation)" order_id) >>= fun () ->
                (match get_price symbol with
                | Some tick ->
                    check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
                    if not (has_open_orders symbol) then
                      create_initial_orders runtime_cfg symbol cmd_buffer
                    else Lwt.return_unit
                | None -> Lwt.return_unit)
            ) else (
              Lwt.return_unit
            )
        | None -> Lwt.return_unit
        end
    | _ -> Lwt.return_unit

  let initialize_orders (runtime_cfg : Config.runtime_cfg) =
    (* Get only symbols configured for Grid strategy *)
    let grid_symbols = List.filter_map (fun (asset: Config.asset_cfg) -> 
      match asset.strategy with 
      | Config.Grid -> Some asset.symbol
      | Config.Orderbook -> None
    ) runtime_cfg.assets in
    
    (* Initialize only grid strategy symbols *) 
    List.iter (fun symbol -> Hashtbl.replace initialized_symbols symbol false) grid_symbols;
    
    (* Fetch existing orders *) 
    let exchange_orders = K.Ws_feed.get_all_open_orders () in
    Hashtbl.clear open_orders;
    (* Process each order and collect logging promises *)
    let log_promises = Hashtbl.fold (fun order_id (order : K.Common.order) promises -> 
      let log_promise = 
        let symbol_str = order.order_symbol in 
        if symbol_str <> "N/A" && List.mem symbol_str grid_symbols then ( 
          Hashtbl.replace open_orders order_id order;
          Hashtbl.replace initialized_symbols symbol_str true; 
          Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
            (Printf.sprintf "Loaded existing order %s for %s" order_id symbol_str)
        ) else (
          Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy")
            (Printf.sprintf "Order %s not for grid strategy symbol, skipping" order_id)
        )
      in
      log_promise :: promises
    ) exchange_orders [] in
    
    (* Wait for all logging to complete, then log summary *)
    Lwt.join log_promises >>= fun () ->
    Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
      (Printf.sprintf "Initialized %d open orders from exchange" (Hashtbl.length open_orders))

  let verify_grid_spacing (runtime_cfg : Config.runtime_cfg) (symbol : string) (cmd_buffer : Core.order_cmd Ringbuffer.t) (current_market_price_float : float) : unit Lwt.t =
    let section = Lwt_log_core.Section.make "engine.strategy.grid_verify" in
    match List.find_opt (fun (asset : Config.asset_cfg) -> String.equal asset.symbol symbol) runtime_cfg.assets with
    | None ->
        Lwt_log_core.warning ~section
          (Printf.sprintf "Grid Verify [%s]: No asset config found." symbol)
    | Some asset_cfg ->
        let open_orders_for_symbol =
          Hashtbl.to_seq_values open_orders
          |> List.of_seq
          |> List.filter (fun (o : K.Common.order) -> String.equal o.order_symbol symbol)
        in

        let buy_orders = List.filter (fun (o : K.Common.order) -> o.side = Some Core.Buy) open_orders_for_symbol in
        let sell_orders = List.filter (fun (o : K.Common.order) -> o.side = Some Core.Sell) open_orders_for_symbol in

        if List.length buy_orders > 0 && List.length sell_orders > 0 then
          let highest_buy_order =
            List.fold_left (fun (acc : K.Common.order) (curr : K.Common.order) ->
              if curr.limit_price > acc.limit_price then curr else acc
            ) (List.hd buy_orders) (List.tl buy_orders)
          in
          let lowest_sell_order =
            List.fold_left (fun (acc : K.Common.order) (curr : K.Common.order) ->
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
                Float.of_string (Primitives.Fixed.to_string asset_cfg.grid_interval)
              in
              let expected_total_spread_pct = 2.0 *. configured_grid_interval_pct in
              let tolerance_pct = 0.0 (* Tolerance for comparison, e.g., 0.1% *) in
              let diff_pct = abs_float (actual_spread_pct_of_mid -. expected_total_spread_pct) in

              if diff_pct <= tolerance_pct then
                Lwt_log_core.info ~section
                  (Printf.sprintf "Grid Verify [%s]: < Tolerance Threshold."
                    symbol)
              else
                (* Grid check FAILED, attempt to amend the highest buy order *)
                let new_target_buy_price_float = min_sell_price_float *. (1.0 -. (expected_total_spread_pct /. 100.0)) in

                (if String.equal symbol "USDG/USD" then
                  Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy.grid_verify")
                    (Printf.sprintf "USDG/USD verify: current=%.8f, min_sell=%.8f, grid_pct=%.4f, expected_spread_pct=%.4f, raw_new_float=%.8f"
                      current_market_price_float min_sell_price_float configured_grid_interval_pct expected_total_spread_pct new_target_buy_price_float)
                else Lwt.return_unit) >>= fun () ->

                if new_target_buy_price_float >= current_market_price_float then
                  Lwt_log_core.info ~section
                  (* Not actually passing, but for strategy purposes is *)
                    (Printf.sprintf "Grid Verify [%s]: PASSED."
                      symbol)
                else
                  (* Safe to amend, check precision and if new price is actually different *)
                  match K.Ws_feed.get_precisions symbol with
                  | None ->
                      Lwt_log_core.error ~section
                        (Printf.sprintf "Grid Verify [%s]:(No Precision)."
                          symbol)
                  | Some (price_prec, qty_prec) ->
                      let new_buy_price_primitive =
                        Primitives.Price.of_string_exn ~scale:price_prec
                          (Printf.sprintf "%.*f" price_prec new_target_buy_price_float)
                      in
                      let existing_buy_price_primitive = (* Convert existing float price to primitive for accurate comparison *)
                        Primitives.Price.of_string_exn ~scale:price_prec
                          (Printf.sprintf "%.*f" price_prec highest_buy_order.limit_price)
                      in

                      if Stdlib.compare new_buy_price_primitive existing_buy_price_primitive = 0 then
                        Lwt_log_core.info ~section
                        (* Not actually passing, but for strategy purpooses it does *)
                          (Printf.sprintf "Grid Verify [%s]: PASSED."
                            symbol)
                      else
                        (* Prices are different after formatting, proceed with amend *)
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

                        if Ringbuffer.push cmd_buffer amend_cmd then
                          Lwt_log_core.info ~section
                            (Printf.sprintf "Grid Verify [%s]: FAILED & AMENDING."
                              symbol)
                        else
                          Lwt_log_core.warning ~section
                            (Printf.sprintf "Grid Verify [%s]: FAILED & AMEND FAILED (Buffer Full)."
                              symbol)
        else
          Lwt_log_core.info ~section
            (Printf.sprintf "Grid Verify [%s]: Skipping, not enough buy/sell orders to form a grid (Buys: %d, Sells: %d)."
              symbol (List.length buy_orders) (List.length sell_orders))

  let get_open_orders () : open_order list =
    let all_feed_orders = K.Ws_feed.get_all_open_orders () in
    let orders = Hashtbl.to_seq_values all_feed_orders |> List.of_seq in
    List.filter_map (fun (order : K.Common.order) -> 
      Some {
          order_id = order.order_id;
          symbol = order.order_symbol;
          side = (match order.side with Some s -> s | None -> failwith ("Invalid side in order: " ^ order.order_id));
          status = order.status;
          limit_price = order.limit_price;
        }
    ) orders
end

let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer =
  (* Log the runtime config to use the variable *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
    (Printf.sprintf "Strategy received runtime_cfg: %s" 
       (Yojson.Safe.to_string (Config.runtime_cfg_to_yojson runtime_cfg))) >>= fun () ->

  (* Wait for the execution snapshot to be processed *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
    "Waiting for execution snapshot from Kraken..." >>= fun () ->
  K.Ws_feed.wait_for_snapshot () >>= fun () ->
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
    "Execution snapshot received, initializing strategy state..." >>= fun () ->

  (* Wait for instrument data to be loaded *) 
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy") 
    "Waiting for instrument data from Kraken..." >>= fun () ->
  K.Ws_feed.wait_for_instruments () >>= fun () ->
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy") 
    "Instrument data received." >>= fun () ->

  State.initialize_orders runtime_cfg >>= fun () -> 

  (* Get only symbols configured for Grid strategy *)
  let grid_symbols = List.filter_map (fun (asset: Config.asset_cfg) -> 
    match asset.strategy with 
    | Config.Grid -> Some asset.symbol
    | Config.Orderbook -> None
  ) runtime_cfg.assets in
  
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
    (Printf.sprintf "Starting grid strategy for symbols: [%s]" (String.concat ", " grid_symbols)) >>= fun () ->

  let rec loop () =
    (* First process any executions *)
    begin match Ringbuffer.pop_opt exec_buffer with
    | Some event -> 
        State.handle_execution runtime_cfg cmd_buffer grid_symbols event >>= fun () ->
        loop () (* Continue processing executions *)
    | None ->
        (* Then process any ticks *)
        (match Ringbuffer.peek_opt tick_buffer with
        | Some (tick : Event.tick) ->
            ignore (Ringbuffer.pop_opt tick_buffer); (* ALWAYS pop the peeked tick *)
            (* Only process ticks for grid strategy symbols *)
            if List.mem tick.symbol grid_symbols then (
              let should_update =
                match State.get_price tick.symbol with
                | Some prev_tick ->
                    not (prev_tick.bid = tick.bid && prev_tick.ask = tick.ask)
                | None -> true
              in
              if should_update then (
                (* Only update state if price changed *)
                Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Processing price update for %s" tick.symbol) >>= fun () ->
                State.update_price tick >>= fun () ->
                State.sync_open_orders runtime_cfg cmd_buffer () >>= fun () ->
                (* First check and adjust existing orders *)
                Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Checking existing orders for %s" tick.symbol) >>= fun () ->
                State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
                (* Then create new orders only if we don't have any for this symbol *)
                (let has_orders = State.has_open_orders tick.symbol in
                Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "%s has open orders: %b" tick.symbol has_orders) >>= fun () ->
                if not has_orders then
                  State.create_initial_orders runtime_cfg tick.symbol cmd_buffer
                else
                  Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                    (Printf.sprintf "Skipping order creation for %s - already has orders" tick.symbol) >>= fun () ->
                  Lwt.return_unit) >>= fun () ->
                (* Verify grid spacing after potential order adjustments or creations *)
                let current_price_for_verify = Float.of_string (Primitives.Price.to_string tick.current_price) in
                State.verify_grid_spacing runtime_cfg tick.symbol cmd_buffer current_price_for_verify >>= fun () ->
                loop ()
              ) else (
                (* Price unchanged, state not updated, just loop *)
                Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                  (Printf.sprintf "Skipping update for %s - price unchanged" tick.symbol) >>= fun () ->
                loop ()
              )
            ) else (
              (* Skip processing non-grid strategy symbols *)
              Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "engine.strategy")
                (Printf.sprintf "Skipping tick for %s - not a grid strategy symbol" tick.symbol) >>= fun () ->
              loop ()
            )
        | None -> Lwt_unix.sleep 0.01 >>= loop (* Sleep briefly if buffer empty *)
        )
    end (* End of outer begin for exec/tick processing *)
  in
  loop ()