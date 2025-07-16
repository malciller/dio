open Lwt.Infix
open Dio_types

module K = Kraken
let section = Lwt_log_core.Section.make "engine.strategy.orderbook"

module State = struct
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16
  let open_orders : (string, K.Common.order) Hashtbl.t = Hashtbl.create 16

  let has_open_buy_order symbol =
    Lwt_log_core.debug ~section (Printf.sprintf "Checking for open buy orders for %s" symbol) |> ignore;
    let buy_orders = Hashtbl.fold (fun _ (order : K.Common.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
        order :: acc
      else
        acc
    ) open_orders [] in
    Lwt_log_core.debug ~section (Printf.sprintf "Found %d buy orders for %s: [%s]" 
      (List.length buy_orders) 
      symbol 
      (String.concat "; " (List.map (fun (o : K.Common.order) -> Printf.sprintf "%s@%.8f" o.order_id o.limit_price) buy_orders))) |> ignore;
    List.length buy_orders > 0

  let get_price symbol = Hashtbl.find_opt price_info symbol

  let create_order ~symbol ~side ~price ~qty =
    match K.Ws_feed.get_precisions symbol with
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
        Lwt_log_core.debug ~section
          (Printf.sprintf "Created order: client_id=%s symbol=%s side=%s price=%s qty=%s"
            client_id symbol (match side with Buy -> "BUY" | Sell -> "SELL") price_str qty_str) |> ignore;
        Some order
    | None ->
        Logs.err (fun m -> m "Precisions not found for symbol: %s. Cannot create order." symbol);
        None

  let create_initial_order (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    Lwt_log_core.info ~section (Printf.sprintf "create_initial_order called for %s" symbol) >>= fun () ->
    
    (* First, let's log what orders we currently have *)
    let current_orders = Hashtbl.fold (fun order_id (order : K.Common.order) acc ->
      if String.equal order.order_symbol symbol then
        let side_str = match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown" in
        Printf.sprintf "%s(%s@%.8f)" order_id side_str order.limit_price :: acc
      else acc
    ) open_orders [] in
    Lwt_log_core.info ~section (Printf.sprintf "Current orders for %s: [%s]" symbol (String.concat "; " current_orders)) >>= fun () ->
    
    let has_buy = has_open_buy_order symbol in
    Lwt_log_core.info ~section (Printf.sprintf "has_open_buy_order for %s: %b" symbol has_buy) >>= fun () ->
    
    if not has_buy then (
      Lwt_log_core.info ~section (Printf.sprintf "No open buy order found for %s, proceeding with order creation" symbol) >>= fun () ->
      match get_price symbol with
      | Some tick ->
          Lwt_log_core.info ~section (Printf.sprintf "Found price data for %s: bid=%s ask=%s" 
            symbol 
            (Primitives.Price.to_string tick.bid) 
            (Primitives.Price.to_string tick.ask)) >>= fun () ->
          let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
            String.equal asset.symbol symbol
          ) runtime_cfg.assets in
          (match asset_cfg_opt with
          | Some asset_cfg ->
              Lwt_log_core.info ~section (Printf.sprintf "Found asset config for %s: qty=%s strategy=%s" 
                symbol (Primitives.Qty.to_string asset_cfg.qty)
                (match asset_cfg.strategy with Config.Orderbook -> "Orderbook" | Config.Grid -> "Grid")) >>= fun () ->
              let buy_price = tick.bid in
              let sell_price = tick.ask in
              Lwt_log_core.info ~section (Printf.sprintf "Creating orders for %s: buy_price=%s sell_price=%s" 
                symbol 
                (Primitives.Price.to_string buy_price) 
                (Primitives.Price.to_string sell_price)) >>= fun () ->
              let buy_order = create_order ~symbol ~side:Buy ~price:buy_price ~qty:asset_cfg.qty in
              let sell_order = create_order ~symbol ~side:Sell ~price:sell_price ~qty:asset_cfg.qty in
              (match buy_order, sell_order with
              | Some buy_cmd, Some sell_cmd ->
                  Lwt_log_core.info ~section (Printf.sprintf "Successfully created both orders for %s, pushing to buffer" symbol) >>= fun () ->
                  if not (Ringbuffer.push cmd_buffer buy_cmd) then
                    Lwt_log_core.warning ~section "Command buffer full! Dropping buy command."
                  else Lwt_log_core.info ~section (Printf.sprintf "Buy order pushed to buffer for %s" symbol) >>= fun () ->
                  if not (Ringbuffer.push cmd_buffer sell_cmd) then
                    Lwt_log_core.warning ~section "Command buffer full! Dropping sell command."
                  else Lwt_log_core.info ~section (Printf.sprintf "Sell order pushed to buffer for %s" symbol)
              | Some _, None ->
                  Lwt_log_core.error ~section (Printf.sprintf "Failed to create sell order for %s" symbol) >>= fun () ->
                  Lwt.return_unit
              | None, Some _ ->
                  Lwt_log_core.error ~section (Printf.sprintf "Failed to create buy order for %s" symbol) >>= fun () ->
                  Lwt.return_unit
              | None, None ->
                  Lwt_log_core.error ~section (Printf.sprintf "Failed to create both orders for %s" symbol) >>= fun () ->
                  Lwt.return_unit)
          | None ->
              Lwt_log_core.warning ~section (Printf.sprintf "No configuration found for %s" symbol))
      | None ->
          Lwt_log_core.warning ~section (Printf.sprintf "No price info for %s" symbol)
    ) else (
      Lwt_log_core.info ~section (Printf.sprintf "Open buy order already exists for %s, skipping order creation" symbol) >>= fun () ->
      Lwt.return_unit
    )

  let check_and_adjust_orders (runtime_cfg : Config.runtime_cfg) cmd_buffer (tick : Event.tick) =
    Lwt_log_core.debug ~section (Printf.sprintf "check_and_adjust_orders called for %s" tick.symbol) >>= fun () ->
    let asset_cfg_opt = List.find_opt (fun (asset: Config.asset_cfg) ->
      String.equal asset.symbol tick.symbol
    ) runtime_cfg.assets in
    match asset_cfg_opt with
    | Some _ ->
        Lwt_log_core.debug ~section (Printf.sprintf "Found asset config for %s" tick.symbol) >>= fun () ->
        let open_buy_orders = Hashtbl.fold (fun order_id (order: K.Common.order) acc ->
          if String.equal order.order_symbol tick.symbol && order.side = Some Core.Buy then
            (order_id, order) :: acc
          else
            acc
        ) open_orders [] in
        Lwt_log_core.debug ~section (Printf.sprintf "Found %d open buy orders for %s" (List.length open_buy_orders) tick.symbol) >>= fun () ->
        (match open_buy_orders with
        | [(_order_id, order)] ->
            Lwt_log_core.debug ~section (Printf.sprintf "Processing single buy order %s for %s" order.order_id tick.symbol) >>= fun () ->
            let top_bid_price = tick.bid in
            let order_price_float = order.limit_price in
            let top_bid_price_float = Float.of_string (Primitives.Price.to_string top_bid_price) in
            
            (* More detailed logging to debug precision issues *)
            Lwt_log_core.debug ~section (Printf.sprintf "Price comparison for order %s: order_price_float=%.8f top_bid_price_float=%.8f" 
              order.order_id order_price_float top_bid_price_float) >>= fun () ->
            
            (* Use a more reasonable tolerance based on the price level *)
            let price_tolerance = 
              if top_bid_price_float > 1.0 then 0.0001 (* For prices > 1.0, use 0.0001 tolerance *)
              else 0.00001 (* For prices < 1.0, use smaller tolerance *)
            in
            let price_diff = abs_float (order_price_float -. top_bid_price_float) in
            
            Lwt_log_core.debug ~section (Printf.sprintf "Price difference: %.10f, tolerance: %.10f, needs_amend: %b" 
              price_diff price_tolerance (price_diff > price_tolerance)) >>= fun () ->
            
            if price_diff > price_tolerance then (
              Lwt_log_core.info ~section (Printf.sprintf "Prices differ significantly, creating amend command for order %s (%.8f -> %.8f)" 
                order.order_id order_price_float top_bid_price_float) >>= fun () ->
              let amend_cmd = Core.Amend {
                dst = "kraken";
                order_id = order.order_id;
                symbol = order.order_symbol;
                new_price = top_bid_price;
                new_qty = Primitives.Qty.of_string_exn ~scale:8 (Printf.sprintf "%.8f" order.qty);
                ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
              } in
              if not (Ringbuffer.push cmd_buffer amend_cmd) then
                Lwt_log_core.warning ~section "Command buffer full! Dropping amend command."
              else
                Lwt_log_core.info ~section (Printf.sprintf "Amending order %s to new price %s" order.order_id (Primitives.Price.to_string top_bid_price))
            ) else (
              Lwt_log_core.debug ~section (Printf.sprintf "Order %s price %.8f matches top bid %.8f (within tolerance %.8f), no amendment needed" 
                order.order_id order_price_float top_bid_price_float price_tolerance) >>= fun () ->
              Lwt.return_unit
            )
        | [] ->
            Lwt_log_core.debug ~section (Printf.sprintf "No open buy orders found for %s" tick.symbol) >>= fun () ->
            Lwt.return_unit
        | _ ->
            Lwt_log_core.debug ~section (Printf.sprintf "Multiple buy orders found for %s, skipping adjustment" tick.symbol) >>= fun () ->
            Lwt.return_unit)
    | None ->
        Lwt_log_core.warning ~section (Printf.sprintf "No configuration found for %s" tick.symbol)

  let sync_open_orders () =
    Lwt_log_core.debug ~section "sync_open_orders called" >>= fun () ->
    let exchange_orders = K.Ws_feed.get_all_open_orders () in
    Lwt_log_core.debug ~section (Printf.sprintf "Found %d orders from exchange" (Hashtbl.length exchange_orders)) >>= fun () ->
    Hashtbl.clear open_orders;
    Hashtbl.iter (fun order_id (order : K.Common.order) ->
      Lwt_log_core.debug ~section (Printf.sprintf "Syncing order %s: symbol=%s side=%s" 
        order_id 
        order.order_symbol 
        (match order.side with Some Core.Buy -> "Buy" | Some Core.Sell -> "Sell" | None -> "None")) |> ignore;
      Hashtbl.add open_orders order_id order
    ) exchange_orders;
    Lwt_log_core.debug ~section (Printf.sprintf "Synced %d orders to local state" (Hashtbl.length open_orders)) >>= fun () ->
    Lwt.return_unit

  let handle_execution runtime_cfg cmd_buffer symbols (event: Core.market_event) =
    match event with
    | Core.Fill { order_id; symbol; price; qty; side; _ } ->
        if List.mem symbol symbols then (
          Lwt_log_core.info ~section (Printf.sprintf "Fill event received for %s: order_id=%s side=%s qty=%s price=%s" 
            symbol order_id 
            (match side with Buy -> "BUY" | Sell -> "SELL")
            (Primitives.Qty.to_string qty)
            (Primitives.Price.to_string price)) >>= fun () ->
          
          (* Look up the original order to get its details *)
          match Hashtbl.find_opt open_orders order_id with
          | Some (order : K.Common.order) ->
              let order_side_str = match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown" in
              Lwt_log_core.info ~section (Printf.sprintf "Found original order %s: symbol=%s side=%s price=%.8f" 
                order_id order.order_symbol order_side_str order.limit_price) >>= fun () ->
              
              (* Sync orders first to get the latest state *)
              sync_open_orders () >>= fun () ->
              
              (* Check if the order still exists after sync - if not, it was completely filled *)
              if not (Hashtbl.mem open_orders order_id) then (
                Lwt_log_core.info ~section (Printf.sprintf "Order %s completely filled" order_id) >>= fun () ->
                
                (* Only create new orders if it was a buy order that was filled *)
                if order.side = Some Core.Buy then (
                  Lwt_log_core.info ~section (Printf.sprintf "Buy order %s filled, creating new orders for %s" order_id symbol) >>= fun () ->
                  create_initial_order runtime_cfg symbol cmd_buffer
                ) else (
                  Lwt_log_core.info ~section (Printf.sprintf "Sell order %s filled, no new orders needed" order_id) >>= fun () ->
                  Lwt.return_unit
                )
              ) else (
                Lwt_log_core.debug ~section (Printf.sprintf "Order %s partially filled, order still exists" order_id) >>= fun () ->
                Lwt.return_unit
              )
          | None ->
              Lwt_log_core.warning ~section (Printf.sprintf "Fill event for unknown order %s, attempting to create orders anyway" order_id) >>= fun () ->
              (* If we can't find the order, we can't determine its side, so we'll be conservative and create orders *)
              sync_open_orders () >>= fun () ->
              create_initial_order runtime_cfg symbol cmd_buffer
        ) else (
          Lwt_log_core.debug ~section (Printf.sprintf "Fill event for %s not in orderbook symbols, ignoring" symbol) >>= fun () ->
          Lwt.return_unit
        )
    | Ack { order_id; client_id; state; _ } ->
        Lwt_log_core.debug ~section (Printf.sprintf "Ack event received: order_id=%s client_id=%s state=%s" 
          order_id client_id 
          (match state with Open -> "Open" | Filled -> "Filled" | Canceled -> "Canceled" | Rejected -> "Rejected")) >>= fun () ->
        
        (match state with
        | Canceled | Rejected ->
            (* Find the order by client_id to determine its symbol and side *)
            let symbol_and_side_opt = Hashtbl.fold (fun _ (order: K.Common.order) acc ->
              match order.client_id with
              | Some order_client_id when String.equal order_client_id client_id -> 
                  Some (order.order_symbol, order.side)
              | _ -> acc
            ) open_orders None in
            
            (match symbol_and_side_opt with
            | Some (symbol, side) when List.mem symbol symbols ->
                Lwt_log_core.info ~section (Printf.sprintf "Order %s cancelled/rejected for %s, side=%s" 
                  order_id symbol 
                  (match side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown")) >>= fun () ->
                
                (* Only create new orders if it was a buy order that was cancelled/rejected *)
                if side = Some Core.Buy then (
                  Lwt_log_core.info ~section (Printf.sprintf "Buy order %s cancelled/rejected, creating new orders for %s" order_id symbol) >>= fun () ->
                  sync_open_orders () >>= fun () ->
                  create_initial_order runtime_cfg symbol cmd_buffer
                ) else (
                  Lwt_log_core.info ~section (Printf.sprintf "Sell order %s cancelled/rejected, no new orders needed" order_id) >>= fun () ->
                  Lwt.return_unit
                )
            | _ -> 
                Lwt_log_core.debug ~section (Printf.sprintf "Ack event for order %s not in orderbook symbols or not found" order_id) >>= fun () ->
                Lwt.return_unit)
        | Filled ->
            (* This is a final Fill confirmation - sync orders and check if we need to create new orders *)
            let symbol_and_side_opt = Hashtbl.fold (fun _ (order: K.Common.order) acc ->
              if String.equal order.order_id order_id then 
                Some (order.order_symbol, order.side)
              else acc
            ) open_orders None in
            
            (match symbol_and_side_opt with
            | Some (symbol, side) when List.mem symbol symbols ->
                Lwt_log_core.info ~section (Printf.sprintf "Order %s fully filled (Ack confirmation) for %s, side=%s" 
                  order_id symbol 
                  (match side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown")) >>= fun () ->
                
                sync_open_orders () >>= fun () ->
                
                (* Only create new orders if it was a buy order that was filled *)
                if side = Some Core.Buy then (
                  Lwt_log_core.info ~section (Printf.sprintf "Buy order %s fully filled, creating new orders for %s" order_id symbol) >>= fun () ->
                  create_initial_order runtime_cfg symbol cmd_buffer
                ) else (
                  Lwt_log_core.info ~section (Printf.sprintf "Sell order %s fully filled, no new orders needed" order_id) >>= fun () ->
                  Lwt.return_unit
                )
            | _ -> 
                Lwt_log_core.debug ~section (Printf.sprintf "Filled Ack for order %s not in orderbook symbols or not found" order_id) >>= fun () ->
                Lwt.return_unit)
        | _ -> 
            Lwt_log_core.debug ~section (Printf.sprintf "Ack event for order %s with state %s, no action needed" 
              order_id (match state with Open -> "Open" | Filled -> "Filled" | Canceled -> "Canceled" | Rejected -> "Rejected")) >>= fun () ->
            Lwt.return_unit)
    | _ -> Lwt.return_unit

  let initialize_orders (runtime_cfg : Config.runtime_cfg) =
    let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
      match asset.strategy with
      | Config.Orderbook -> Some asset.symbol
      | _ -> None
    ) runtime_cfg.assets in
    let exchange_orders = K.Ws_feed.get_all_open_orders () in
    Hashtbl.clear open_orders;
    let log_promises = Hashtbl.fold (fun order_id (order : K.Common.order) promises ->
      let log_promise =
        if List.mem order.order_symbol orderbook_symbols then (
          Hashtbl.replace open_orders order_id order;
          Lwt_log_core.info ~section (Printf.sprintf "Loaded existing order %s for %s" order_id order.order_symbol)
        ) else
          Lwt.return_unit
      in
      log_promise :: promises
    ) exchange_orders [] in
    Lwt.join log_promises >>= fun () ->
    Lwt_log_core.info ~section (Printf.sprintf "Initialized %d open orders from exchange" (Hashtbl.length open_orders))

  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    Lwt.return_unit
end

let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer =
  Lwt_log_core.info ~section "Starting orderbook market making strategy" >>= fun () ->
  
  (* Wait for execution snapshot and instruments *)
  K.Ws_feed.wait_for_snapshot () >>= fun () ->
  K.Ws_feed.wait_for_instruments () >>= fun () ->
  
  (* Initialize orders for orderbook strategy *)
  State.initialize_orders runtime_cfg >>= fun () ->
  
  (* Get symbols configured for Orderbook strategy *)
  let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) -> 
    match asset.strategy with 
    | Config.Orderbook -> Some asset.symbol
    | Config.Grid -> None
  ) runtime_cfg.assets in
  
  Lwt_log_core.info ~section
    (Printf.sprintf "Starting orderbook strategy for symbols: [%s]" (String.concat ", " orderbook_symbols)) >>= fun () ->

  let rec loop () =
    (* First process any executions *)
    (match Ringbuffer.pop_opt exec_buffer with
    | Some event ->
        Lwt_log_core.debug ~section (Printf.sprintf "Processing execution event: %s" 
          (match event with
          | Core.Fill { symbol; _ } -> Printf.sprintf "Fill for %s" symbol
          | Core.Ack { order_id; state; _ } -> Printf.sprintf "Ack for %s, state: %s" order_id 
            (match state with Open -> "Open" | Filled -> "Filled" | Canceled -> "Canceled" | Rejected -> "Rejected")
          | _ -> "Other")) >>= fun () ->
        State.handle_execution runtime_cfg cmd_buffer orderbook_symbols event >>= fun () ->
        loop ()
    | None ->
        (* Then process any ticks *)
        match Ringbuffer.pop_opt tick_buffer with
        | Some (tick : Event.tick) ->
            Lwt_log_core.debug ~section (Printf.sprintf "Processing tick for %s: bid=%s ask=%s" 
              tick.symbol 
              (Primitives.Price.to_string tick.bid) 
              (Primitives.Price.to_string tick.ask)) >>= fun () ->
            State.update_price tick >>= fun () ->
            if List.mem tick.symbol orderbook_symbols then (
              Lwt_log_core.debug ~section (Printf.sprintf "%s is in orderbook symbols, checking and adjusting orders" tick.symbol) >>= fun () ->
              (* Sync orders first to ensure we have the latest state *)
              State.sync_open_orders () >>= fun () ->
              State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
              (* Also try to create initial orders if none exist *)
              State.create_initial_order runtime_cfg tick.symbol cmd_buffer >>= fun () ->
              loop ()
            ) else (
              Lwt_log_core.debug ~section (Printf.sprintf "%s is not in orderbook symbols [%s], skipping" 
                tick.symbol (String.concat ", " orderbook_symbols)) >>= fun () ->
              loop ()
            )
        | None -> 
            Lwt_log_core.debug ~section "No ticks or executions to process, sleeping" >>= fun () ->
            Lwt_unix.sleep 0.01 >>= loop (* Sleep briefly if buffer empty *)
    )
  in
  loop () 