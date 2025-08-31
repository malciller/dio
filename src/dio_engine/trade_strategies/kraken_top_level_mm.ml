(*
  Top-level order book market-making.
  - Ideal for 0 fee pegged assets (hft)
  - Places buy and sell orders at top level bid/ask prices.
  - Filling of a buy order triggers new order pair
  - Buy order updated to maintain top-level of order book.

*)

(* src/engine/strategy/kraken_top_level_orderbook_mm.ml *)
open Lwt.Infix
open Dio_types
open Lwt_log_core
module K = Kraken

let section = Lwt_log_core.Section.make "engine.strategy.orderbook"

module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16
  let open_orders : (string, K.Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16

  let has_open_buy_order symbol =
    debug_f ~section "Checking for open buy orders for %s" symbol |> ignore;
    let buy_orders = Hashtbl.fold (fun _ (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
        order :: acc
      else
        acc
    ) open_orders [] in
    debug_f ~section "Found %d buy orders for %s: [%s]"
      (List.length buy_orders)
      symbol
      (String.concat "; " (List.map (fun (o : K.Kraken_common_types.order) -> Printf.sprintf "%s@%.8f" o.order_id o.limit_price) buy_orders)) |> ignore;
    List.length buy_orders > 0

  let get_price symbol = Hashtbl.find_opt price_info symbol

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
        debug_f ~section
          "Created order: client_id=%s symbol=%s side=%s price=%s qty=%s"
            client_id symbol (match side with Buy -> "BUY" | Sell -> "SELL") price_str qty_str |> ignore;
        Some order
    | None ->
        error_f ~section "Precisions not found for symbol: %s. Cannot create order." symbol |> ignore;
        None

  let create_initial_order (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    
    (* First, let's log what orders we currently have *)
    let _current_orders = Hashtbl.fold (fun order_id (order : K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol then
        let side_str = match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown" in
        Printf.sprintf "%s(%s@%.8f)" order_id side_str order.limit_price :: acc
      else acc
    ) open_orders [] in

    
    let has_buy = has_open_buy_order symbol in
    info_f ~section "has_open_buy_order for %s: %b" symbol has_buy >>= fun () ->
    
    if not has_buy then (
      match get_price symbol with
      | Some tick ->
          info_f ~section "Found price data for %s: bid=%s ask=%s"
            symbol
            (Primitives.Price.to_string tick.bid)
            (Primitives.Price.to_string tick.ask) >>= fun () ->
          let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
            String.equal asset.symbol symbol
          ) runtime_cfg.assets in
          (match asset_cfg_opt with
          | Some asset_cfg ->
              info_f ~section "Found asset config for %s: qty=%s strategy=%s"
                symbol (Primitives.Qty.to_string asset_cfg.qty)
                (match asset_cfg.strategy with Config.Orderbook -> "Orderbook" | Config.Grid -> "Grid") >>= fun () ->
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
              warning_f ~section "No configuration found for %s" symbol)
      | None ->
          warning_f ~section "No price info for %s" symbol
    ) else (
      Lwt.return_unit
    )

  let check_and_adjust_orders (runtime_cfg : Config.runtime_cfg) cmd_buffer (tick : Event.tick) =
    debug_f ~section "check_and_adjust_orders called for %s" tick.symbol >>= fun () ->
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
            
            (* More detailed logging to debug precision issues *)
            debug_f ~section "Price comparison for order %s: order_price_float=%.8f top_bid_price_float=%.8f"
              order.order_id order_price_float top_bid_price_float >>= fun () ->
            
            (* No tolerance - any price change triggers an amend *)
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

  let sync_open_orders () =
    debug_f ~section "sync_open_orders called" >>= fun () ->
    let exchange_orders = K.Kraken_incoming_data.get_all_open_orders () in
    debug_f ~section "Found %d orders from exchange" (Hashtbl.length exchange_orders) >>= fun () ->
    Hashtbl.clear open_orders;
    Hashtbl.iter (fun order_id (order : K.Kraken_common_types.order) ->
      debug_f ~section "Syncing order %s: symbol=%s side=%s"
        order_id
        order.order_symbol
        (match order.side with Some Core.Buy -> "Buy" | Some Core.Sell -> "Sell" | None -> "None") |> ignore;
      Hashtbl.add open_orders order_id order
    ) exchange_orders;
    debug_f ~section "Synced %d orders to local state" (Hashtbl.length open_orders) >>= fun () ->
    Lwt.return_unit

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
            let symbol_and_side_opt = Hashtbl.fold (fun _ (order: K.Kraken_common_types.order) acc ->
              if String.equal order.order_id order_id then
                Some (order.order_symbol, order.side)
              else acc
            ) open_orders None in
            
            (match symbol_and_side_opt with
            | Some (symbol, side) when List.mem symbol symbols ->
                info_f ~section "Order %s fully filled (Ack confirmation) for %s, side=%s"
                  order_id symbol
                  (match side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown") >>= fun () ->
                
                sync_open_orders () >>= fun () ->
                
                (* Only create new orders if it was a buy order that was filled *)
                if side = Some Core.Buy then (
                  info_f ~section "Buy order %s fully filled, creating new orders for %s" order_id symbol >>= fun () ->
                  create_initial_order runtime_cfg symbol cmd_buffer
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

  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    Lwt.return_unit
end

let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer =
  info_f ~section "Starting orderbook market making strategy" >>= fun () ->
  
  (* Wait for execution snapshot and instruments *)
  K.Kraken_incoming_data.wait_for_snapshot () >>= fun () ->
  K.Kraken_incoming_data.wait_for_instruments () >>= fun () ->
  
  (* Initialize orders for orderbook strategy *)
  State.initialize_orders runtime_cfg >>= fun () ->
  
  (* Get symbols configured for Orderbook strategy *)
  let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
    match asset.strategy with
    | Config.Orderbook -> Some asset.symbol
    | Config.Grid -> None
  ) runtime_cfg.assets in
  
  info_f ~section
    "Starting orderbook strategy for symbols: [%s]" (String.concat ", " orderbook_symbols) >>= fun () ->

  (* --- Task 1: Process executions --- *)
  let rec execution_loop () =
    Ringbuffer.pop exec_buffer >>= fun event ->
    debug_f ~section "Processing execution event: %s"
      (match event with
      | Core.Fill { symbol; _ } -> Printf.sprintf "Fill for %s" symbol
      | Core.Ack { order_id; state; _ } -> Printf.sprintf "Ack for %s, state: %s" order_id
        (match state with Open -> "Open" | Filled -> "Filled" | Canceled -> "Canceled" | Rejected -> "Rejected")
      | _ -> "Other") >>= fun () ->
    State.handle_execution runtime_cfg cmd_buffer orderbook_symbols event >>= fun () ->
    execution_loop ()
  in

  (* --- Task 2: Process ticks --- *)
  let rec tick_loop () =
    Ringbuffer.pop tick_buffer >>= fun (tick : Event.tick) ->
    debug_f ~section "Processing tick for %s: bid=%s ask=%s"
      tick.symbol
      (Primitives.Price.to_string tick.bid)
      (Primitives.Price.to_string tick.ask) >>= fun () ->
    (if List.mem tick.symbol orderbook_symbols then (
      debug_f ~section "%s is in orderbook symbols, checking and adjusting orders" tick.symbol >>= fun () ->
      (* Sync orders first to ensure we have the latest state *)
      State.update_price tick >>= fun () ->
      State.sync_open_orders () >>= fun () ->
      State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
      (* Also try to create initial orders if none exist *)
      State.create_initial_order runtime_cfg tick.symbol cmd_buffer
    ) else (
      debug_f ~section "%s is not in orderbook symbols [%s], skipping"
        tick.symbol (String.concat ", " orderbook_symbols)
    )) >>= fun () ->
    tick_loop ()
  in

  (* Run both loops in parallel *)
  Lwt.join [execution_loop (); tick_loop ()] 