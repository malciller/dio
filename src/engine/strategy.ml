(* src/engine/strategy.ml *)
open Lwt.Infix  (* for >>= *)


open Types.Core

open Types (* For Event.tick type *)
open Types.Primitives (* Needed for Qty, Price etc. *)

module K = Kraken.Ws_feed (* To get open orders *)

(* Module-level state *)
module State = struct
  (* Track latest prices per symbol *)
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16

  (* Track our open orders - USE NEW TYPE *)
  let open_orders : (string, K.order) Hashtbl.t = Hashtbl.create 16
  
  (* Track whether we've initialized orders for each symbol *)
  let initialized_symbols : (string, bool) Hashtbl.t = Hashtbl.create 16

  (* Type for open order information *)
  type open_order = {
    order_id: string;
    symbol: string;
    side: side;
    status: order_state;
    limit_price: float;
  }

  (* Update price info for a symbol *)
  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy") 
      (Printf.sprintf "Updated price for %s: current_price=%s" 
        tick.symbol 
        (Price.to_string tick.current_price))

  (* Get latest price info for a symbol *)
  let get_price symbol = Hashtbl.find_opt price_info symbol

  (* Check if we have any open orders for a symbol *)
  let has_open_orders symbol =
    Hashtbl.fold (fun _ (order : K.order) has_orders -> (* USE NEW TYPE *)
      has_orders || String.equal order.order_symbol symbol (* Use field from K.order *)
    ) open_orders false

  (* Create a new order command *)
  let create_order ~symbol ~side ~price ~qty =
    Add {
      dst = "kraken";
      client_id = "strategy-" ^ Int64.to_string (Unix.time () *. 1_000_000. |> Int64.of_float);
      symbol;
      side;
      price;
      qty = Qty.of_string_exn ~scale:8 qty;
      tif = GTC;
      tags = [`Grid];
    }

  (* Update open orders based on execution *)
  let handle_execution (event : market_event) =
    match event with
    | Fill { order_id; symbol; price; qty; side; _ } ->
        begin match Hashtbl.find_opt open_orders order_id with
        | Some (order : K.order) -> (* USE NEW TYPE, though find_opt implies it *)
            let side_str = match side with Buy -> "BUY" | Sell -> "SELL" in
            let order_side_str = 
              match order.side with (* Use field from K.order *)
              | Some Buy -> "Buy"
              | Some Sell -> "Sell"
              | None -> "unknown"
            in
            Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
              (Printf.sprintf "Order %s filled: %s %s %s @ %s (original side: %s)" 
                order_id
                side_str
                (Qty.to_string qty)
                symbol
                (Price.to_string price)
                order_side_str) >>= fun () ->
            Hashtbl.remove open_orders order_id;
            Lwt.return_unit
        | None -> Lwt.return_unit
        end
    | Ack { order_id; state = Canceled; _ } ->
        Hashtbl.remove open_orders order_id;
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
          (Printf.sprintf "Order %s canceled" order_id)
    | Ack { order_id; state = Rejected; _ } ->
        Hashtbl.remove open_orders order_id;
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
          (Printf.sprintf "Order %s rejected" order_id)
    | _ -> Lwt.return_unit

  (* Initialize order state from exchange *)
  let initialize_orders (cfg : Types.Core.config) =
    (* Initialize all configured symbols to false *) 
    List.iter (fun symbol -> Hashtbl.replace initialized_symbols symbol false) cfg.symbols;
    
    (* Fetch existing orders *) 
    let exchange_orders = K.get_open_buy_orders () in
    Hashtbl.clear open_orders;
    (* Process each order and collect logging promises *)
    let log_promises = Hashtbl.fold (fun order_id (order : K.order) promises -> (* USE NEW TYPE *)
      let log_promise = 
        let symbol_str = order.order_symbol in (* Use field from K.order *)
        if symbol_str <> "N/A" && List.mem symbol_str cfg.symbols then ( (* Only process orders for configured symbols *) 
          Hashtbl.replace open_orders order_id order;
          Hashtbl.replace initialized_symbols symbol_str true; (* Set to true if existing order found *) 
          Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
            (Printf.sprintf "Loaded existing order %s for %s" order_id symbol_str)
        ) else (
          Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy")
            (Printf.sprintf "Order %s has no symbol, skipping" order_id)
        )
      in
      log_promise :: promises
    ) exchange_orders [] in
    
    (* Wait for all logging to complete, then log summary *)
    Lwt.join log_promises >>= fun () ->
    Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
      (Printf.sprintf "Initialized %d open orders from exchange" (Hashtbl.length open_orders))

  (* Sync our open orders with exchange's state *)
  let sync_open_orders () =
    let exchange_orders = K.get_open_buy_orders () in
    (* Remove orders that no longer exist on exchange *)
    Hashtbl.iter (fun order_id _ ->
      if not (Hashtbl.mem exchange_orders order_id) then
        Hashtbl.remove open_orders order_id
    ) open_orders;
    (* Add/update orders from exchange *)
    Hashtbl.iter (fun order_id (order : K.order) -> (* USE NEW TYPE *)
      Hashtbl.replace open_orders order_id order
    ) exchange_orders;
    Lwt.return_unit

  (* Create initial orders for a symbol if none exist *)
  let create_initial_orders symbol cmd_buffer =
    (* Only proceed if we've initialized orders for this symbol *)
    if Hashtbl.mem initialized_symbols symbol && not (has_open_orders symbol) then
      match get_price symbol with
      | Some tick ->
          (* Create a buy order slightly below current bid *)
          let bid_float = Price.to_string tick.bid |> float_of_string in
          let buy_price = Price.of_string_exn ~scale:8 (Printf.sprintf "%.8f" (bid_float *. 0.995)) in
          let buy_cmd = create_order ~symbol ~side:Buy ~price:buy_price ~qty:"0.001" in
          
          (* Create a sell order slightly above current ask *)
          let ask_float = Price.to_string tick.ask |> float_of_string in
          let sell_price = Price.of_string_exn ~scale:8 (Printf.sprintf "%.8f" (ask_float *. 1.005)) in
          let sell_cmd = create_order ~symbol ~side:Sell ~price:sell_price ~qty:"0.001" in
          
          (* Push both orders to the command buffer *)
          (if not (Ringbuffer.push cmd_buffer buy_cmd) then
            Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy") 
              "Command buffer full! Dropping buy command."
          else
            Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
              (Printf.sprintf "Created buy order for %s @ %s" symbol (Price.to_string buy_price))) >>= fun () ->

          (if not (Ringbuffer.push cmd_buffer sell_cmd) then
            Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy") 
              "Command buffer full! Dropping sell command."
          else
            Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
              (Printf.sprintf "Created sell order for %s @ %s" symbol (Price.to_string sell_price))) >>= fun () ->
          
          Lwt.return_unit
      | None ->
          Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy")
            (Printf.sprintf "No price info available for %s, skipping order creation" symbol)
    else
      Lwt.return_unit

  let get_open_orders () : open_order list =
    let buy_orders = K.get_open_buy_orders () in
    let orders = Hashtbl.to_seq_values buy_orders |> List.of_seq in
    List.filter_map (fun (order : K.order) -> (* USE NEW TYPE *)
      Some {
          order_id = order.order_id;
          symbol = order.order_symbol;
          side = (match order.side with Some s -> s | None -> failwith ("Invalid side in order: " ^ order.order_id));
          status = order.status;
          limit_price = order.limit_price;
        }
    ) orders
end

let start cfg ~tick_buffer ~cmd_buffer ~exec_buffer =
  (* Initialize state using the config *)
  State.initialize_orders cfg >>= fun () -> (* Explicitly initialize state *)

  (* Log strategy startup with config info *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy")
    (Printf.sprintf "Starting grid strategy for symbols: [%s]" (String.concat ", " cfg.symbols)) >>= fun () ->

  (* Grid strategy: consume ticks and executions, emit Core.order_cmd via cmd_buffer *)
  let rec loop () =
    (* First process any executions *)
    begin match Ringbuffer.pop_opt exec_buffer with
    | Some event -> 
        State.handle_execution event >>= fun () ->
        loop () (* Continue processing executions *)
    | None ->
        (* Then process any ticks *)
        (match Ringbuffer.peek_opt tick_buffer with
        | Some (tick : Event.tick) ->
            ignore (Ringbuffer.pop_opt tick_buffer); (* ALWAYS pop the peeked tick *)
            let should_update =
              match State.get_price tick.symbol with
              | Some prev_tick ->
                  not (prev_tick.bid = tick.bid && prev_tick.ask = tick.ask)
              | None -> true
            in
            if should_update then (
              (* Only update state if price changed *)
              State.update_price tick >>= fun () ->
              State.sync_open_orders () >>= fun () ->
              State.create_initial_orders tick.symbol cmd_buffer >>= fun () ->
              loop ()
            ) else (
              (* Price unchanged, state not updated, just loop *)
              loop ()
            )
        | None -> Lwt_unix.sleep 0.01 >>= loop (* Sleep briefly if buffer empty *)
        )
    end (* End of outer begin for exec/tick processing *)
  in
  loop ()