(*
  Kraken Market Making Strategy (Unified)

  Supports two market making strategies:
  - GMM (Greedy Market Making): Maintains persistent orders at best bid/ask
  - VMM (Valley Market Making): Adaptive strategy that switches between greedy and valley modes based on spread
*)
open Lwt.Infix
open Dio_types
open Lwt_log_core
open Engine
module K = Kraken
module SharedState = K.Kraken_common_types.StrategyState

let section = Lwt_log_core.Section.make "engine.strategy.kraken.MM"

(*
  Strategy State Management
  
  Unified state for both GMM and VMM strategies
*)

module State = struct
  (* Shared data structures *)
  let asset_balances : (string, float) Hashtbl.t = Hashtbl.create 16
  let inventory_tracker : (string, float) Hashtbl.t = Hashtbl.create 16
  let fill_qty_tracker : (string, float) Hashtbl.t = Hashtbl.create 16
  let maker_fee_cache : (string, float) Hashtbl.t = Hashtbl.create 16

  (** Update asset balance from exchange *)
  let refresh_asset_balance symbol =
    match K.Kraken_incoming_data.get_instrument symbol with
    | Some instrument ->
      let base_currency = instrument.base in
      K.Kraken_balances.wait_for_balances () >>= fun (spot_balances, _, liquid_balances, _) ->
      let spot_bal = Hashtbl.find_opt spot_balances base_currency |> Option.value ~default:0.0 in
      let liquid_bal = Hashtbl.find_opt liquid_balances base_currency |> Option.value ~default:0.0 in
      let tradeable_balance = spot_bal +. liquid_bal in
      Hashtbl.replace asset_balances base_currency tradeable_balance;
      debug_f ~section "Refreshed %s balance: %.8f" base_currency tradeable_balance
    | None ->
      warning_f ~section "No instrument data for %s, cannot refresh balance." symbol >>= fun () ->
      Lwt.return_unit

  (** Get maker fee rate for a given symbol *)
  let get_maker_fee symbol =
    let normalized_symbol = String.uppercase_ascii symbol in
    match Hashtbl.find_opt maker_fee_cache normalized_symbol with
    | Some fee -> fee
    | None ->
        let fee =
          match K.Kraken_fee_cache.get_fee_rate normalized_symbol ~is_maker:true with
          | Some cached -> cached
          | None ->
              (match K.Kraken_fee_cache.fallback_fee_of normalized_symbol ~is_maker:true with
              | Some fallback -> fallback
              | None ->
                  warning_f ~section "Missing maker fee for %s, defaulting to 0.002" symbol |> ignore;
                  0.002)
        in
        Hashtbl.replace maker_fee_cache normalized_symbol fee;
        fee

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

  (** Get balance already locked in open sell orders *)
  let get_balance_in_open_sell_orders symbol =
    Hashtbl.fold (fun _ (order: K.Kraken_common_types.order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Sell then
        acc +. order.qty
      else
        acc
    ) SharedState.open_orders 0.0

  (** Create exchange-compliant order with proper formatting *)
  let create_order ~symbol ~side ~price ~qty =
    SharedState.create_standard_order
      ~symbol ~side ~price ~qty ~get_precisions_fn:(SharedState.get_precisions K.Kraken_incoming_data.get_precisions)

  (*
    GMM (Greedy Market Making) Strategy Functions
  *)

  (** Check if spread is sufficient for profitable greedy market making *)
  let is_spread_sufficient_for_greedy runtime_cfg symbol =
    match SharedState.get_price symbol with
    | Some tick ->
        let bid_float = Float.of_string (Primitives.Price.to_string tick.bid) in
        let ask_float = Float.of_string (Primitives.Price.to_string tick.ask) in
        let spread = ask_float -. bid_float in
        let asset_fee = get_maker_fee symbol in
        let profit_threshold = runtime_cfg.Config.profit_threshold_pct /. 100.0 in
        let required_spread = ask_float *. ((2.0 *. asset_fee) +. profit_threshold) in
        spread >= required_spread
    | None -> false

  (** Create greedy MM orders at top of book *)
  let create_greedy_orders (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    let asset_cfg_opt = SharedState.get_asset_config runtime_cfg symbol in
    match asset_cfg_opt with
    | Some asset_cfg ->
        (match SharedState.get_price symbol with
        | Some tick ->
            let buy_price = tick.bid in
            let sell_price = tick.ask in
            let order_qty_float = Float.of_string (Primitives.Qty.to_string asset_cfg.qty) in
            let buy_price_float = Float.of_string (Primitives.Price.to_string buy_price) in
            let order_cost = buy_price_float *. order_qty_float in

            if SharedState.get_usd_balance () >= order_cost then (
              (match create_order ~symbol ~side:Buy ~price:buy_price ~qty:asset_cfg.qty with
              | Some buy_cmd ->
                  Ringbuffer.push cmd_buffer buy_cmd >>= fun () ->
                  info_f ~section "[GMM-style] Placed buy order for %s at %s (top bid)" 
                    symbol (Primitives.Price.to_string buy_price)
              | None ->
                  error_f ~section "Failed to create greedy buy order for %s" symbol) >>= fun () ->
              
              (match create_order ~symbol ~side:Sell ~price:sell_price ~qty:asset_cfg.qty with
              | Some sell_cmd ->
                  Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                  info_f ~section "[GMM-style] Placed sell order for %s at %s (top ask)" 
                    symbol (Primitives.Price.to_string sell_price)
              | None ->
                  error_f ~section "Failed to create greedy sell order for %s" symbol)
            ) else (
              warning_f ~section "Insufficient USD balance for greedy order on %s. Required: %.2f, Available: %.2f"
                symbol order_cost (SharedState.get_usd_balance ())
            )
        | None ->
            warning_f ~section "No price info for %s, cannot place greedy orders." symbol)
    | None ->
        warning_f ~section "No configuration found for %s" symbol

  (*
    VMM (Valley Market Making) Strategy Functions
  *)

  (** Create valley MM orders with calculated profitable prices *)
  let create_valley_orders (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    let asset_cfg_opt = SharedState.get_asset_config runtime_cfg symbol in
    match asset_cfg_opt with
    | Some asset_cfg ->
        (match K.Kraken_incoming_data.get_instrument symbol with
        | Some instrument ->
            (match SharedState.get_price symbol with
            | Some tick ->
                let top_ask_price_float = Float.of_string (Primitives.Price.to_string tick.ask) in
                let asset_fee = get_maker_fee symbol in
                let profit_threshold = runtime_cfg.profit_threshold_pct /. 100.0 in

                let buy_price_float = top_ask_price_float *. (1.0 -. ((2.0 *. asset_fee) +. profit_threshold)) in
                let buy_price = Primitives.Price.of_string_exn ~scale:instrument.price_precision 
                  (Printf.sprintf "%.*f" instrument.price_precision buy_price_float) in
                let sell_price = tick.ask in

                let order_qty_float = Float.of_string (Primitives.Qty.to_string asset_cfg.qty) in
                let order_cost = buy_price_float *. order_qty_float in

                if SharedState.get_usd_balance () >= order_cost then (
                  (match create_order ~symbol ~side:Buy ~price:buy_price ~qty:asset_cfg.qty with
                  | Some buy_cmd ->
                      Ringbuffer.push cmd_buffer buy_cmd >>= fun () ->
                      info_f ~section "[VMM-style] Placed buy order for %s at %s (valley)" 
                        symbol (Primitives.Price.to_string buy_price)
                  | None ->
                      error_f ~section "Failed to create valley buy order for %s" symbol) >>= fun () ->
                  
                  (match create_order ~symbol ~side:Sell ~price:sell_price ~qty:asset_cfg.qty with
                  | Some sell_cmd ->
                      Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                      info_f ~section "[VMM-style] Placed sell order for %s at %s (top ask)" 
                        symbol (Primitives.Price.to_string sell_price)
                  | None ->
                      error_f ~section "Failed to create valley sell order for %s" symbol)
                ) else (
                  warning_f ~section "Insufficient USD balance for valley order on %s. Required: %.2f, Available: %.2f"
                    symbol order_cost (SharedState.get_usd_balance ())
                )
            | None ->
                warning_f ~section "No price info for %s, cannot place valley orders." symbol)
        | None ->
            warning_f ~section "No instrument data for %s" symbol)
    | None ->
        warning_f ~section "No configuration found for %s" symbol

  (** Unified order management for both GMM and VMM *)
  let manage_orders (runtime_cfg : Config.runtime_cfg) symbol cmd_buffer =
    let has_buy = SharedState.has_open_buy_order symbol in
    let asset_cfg_opt = SharedState.get_asset_config runtime_cfg symbol in

    match asset_cfg_opt with
    | Some asset_cfg ->
        (* Check both constraints: min_usd_balance and max_exposure *)
        let check_min_usd_balance = match asset_cfg.min_usd_balance with
          | Some min_balance ->
              let min_balance_float = float_of_string (Primitives.Fixed.to_string min_balance) in
              SharedState.get_usd_balance () >= min_balance_float
          | None -> true
        in
        
        let current_inventory = get_current_inventory symbol in
        let check_max_exposure = match asset_cfg.max_exposure with
          | Some max_exposure_fixed ->
              let max_exposure = float_of_string (Primitives.Fixed.to_string max_exposure_fixed) in
              current_inventory < max_exposure
          | None -> true
        in
        
        (* If both checks pass and no buy order exists, place orders *)
        if check_min_usd_balance && check_max_exposure then (
          if not has_buy then (
            (* Determine which strategy to use based on symbol configuration *)
            match asset_cfg.strategy with
            | Config.GMM ->
                (* Pure GMM: always use greedy orders *)
                create_greedy_orders runtime_cfg symbol cmd_buffer
            | Config.VMM ->
                (* VMM: use greedy if spread is sufficient, otherwise valley *)
                if is_spread_sufficient_for_greedy runtime_cfg symbol then (
                  info_f ~section "Spread sufficient for %s, using GMM-style orders" symbol >>= fun () ->
                  create_greedy_orders runtime_cfg symbol cmd_buffer
                ) else (
                  info_f ~section "Spread insufficient for %s, using VMM-style orders" symbol >>= fun () ->
                  create_valley_orders runtime_cfg symbol cmd_buffer
                )
            | _ ->
                warning_f ~section "Unsupported strategy for symbol %s in MM module" symbol
          ) else (
            Lwt.return_unit
          )
        ) else (
          (* At least one constraint is violated - cancel buy orders and try to sell remaining balance *)
          if not check_max_exposure then (
            (* Max exposure reached - cancel existing buy orders *)
            let open_buy_orders = Hashtbl.fold (fun order_id (order: K.Kraken_common_types.order) acc ->
              if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
                (order_id, order) :: acc
              else
                acc
            ) SharedState.open_orders [] in

            (if List.length open_buy_orders > 0 then (
              info_f ~section "Max exposure reached for %s (%.8f). Cancelling %d buy order(s)."
                symbol current_inventory (List.length open_buy_orders) >>= fun () ->
              Lwt_list.iter_s (fun (order_id, _) ->
                let cancel_cmd = Core.Cancel {
                  dst = "kraken";
                  order_id;
                } in
                Ringbuffer.push cmd_buffer cancel_cmd >>= fun () ->
                info_f ~section "Cancelled buy order %s for %s" order_id symbol
              ) open_buy_orders
            ) else (
              Lwt.return_unit
            )) >>= fun () ->
            info_f ~section "Max exposure reached for %s. Checking for remaining asset balance to sell." symbol
          ) else (
            (* Must be min_usd_balance constraint *)
            info_f ~section "USD balance %.2f is below minimum for %s. Checking for remaining asset balance to sell."
              (SharedState.get_usd_balance ()) symbol
          ) >>= fun () ->
          
          (* Check for available balance to place sell order *)
          (match K.Kraken_incoming_data.get_instrument symbol with
          | Some instrument ->
              let base_currency = instrument.base in
              let qty_prec = instrument.qty_precision in
              
              K.Kraken_balances.wait_for_balances () >>= fun (spot_balances, _, liquid_balances, _) ->
              let spot_bal = Hashtbl.find_opt spot_balances base_currency |> Option.value ~default:0.0 in
              let liquid_bal = Hashtbl.find_opt liquid_balances base_currency |> Option.value ~default:0.0 in
              let total_balance = spot_bal +. liquid_bal in
              
              let balance_in_orders = get_balance_in_open_sell_orders symbol in
              let available_balance = total_balance -. balance_in_orders in
              
              if available_balance > 0.00001 then (
                let clean_qty = floor (available_balance *. 10.0 ** float_of_int qty_prec) /. (10.0 ** float_of_int qty_prec) in
                (match SharedState.get_price symbol with
                | Some tick ->
                    let sell_price = tick.ask in
                    let qty_str = Printf.sprintf "%.*f" qty_prec clean_qty in
                    let sell_qty = Primitives.Qty.of_string_exn ~scale:qty_prec qty_str in
                    (match create_order ~symbol ~side:Sell ~price:sell_price ~qty:sell_qty with
                    | Some sell_cmd ->
                        Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                        info_f ~section "Placed sell order for remaining %s balance: %.8f (constraint violation)"
                          symbol clean_qty
                    | None ->
                        error_f ~section "Failed to create sell order for remaining %s balance" symbol
                    )
                | None ->
                    warning_f ~section "No price info for %s, cannot place sell order" symbol
                )
              ) else (
                debug_f ~section "No remaining available balance for %s to sell" symbol
              )
          | None ->
              warning_f ~section "No instrument data for %s, cannot place sell order" symbol
          )
        )
    | None ->
        warning_f ~section "No configuration found for %s" symbol

  (** Adjust buy orders to maintain optimal positioning *)
  let check_and_adjust_orders (runtime_cfg : Config.runtime_cfg) cmd_buffer (tick : Event.tick) =
    let asset_cfg_opt = SharedState.get_asset_config runtime_cfg tick.symbol in
    match asset_cfg_opt with
    | Some asset_cfg ->
        (match K.Kraken_incoming_data.get_instrument tick.symbol with
        | Some instrument ->
            let open_buy_orders = Hashtbl.fold (fun order_id (order: K.Kraken_common_types.order) acc ->
              if String.equal order.order_symbol tick.symbol && order.side = Some Core.Buy then
                (order_id, order) :: acc
              else
                acc
            ) SharedState.open_orders [] in

            (match open_buy_orders with
            | [(_order_id, order)] ->
                (* Determine target price based on strategy *)
                let new_buy_price = match asset_cfg.strategy with
                | Config.GMM ->
                    (* GMM: always target top bid *)
                    tick.bid
                | Config.VMM ->
                    (* VMM: use greedy if spread sufficient, otherwise valley *)
                    if is_spread_sufficient_for_greedy runtime_cfg tick.symbol then
                      tick.bid
                    else
                      let top_ask_price_float = Float.of_string (Primitives.Price.to_string tick.ask) in
                      let asset_fee = get_maker_fee tick.symbol in
                      let profit_threshold = runtime_cfg.profit_threshold_pct /. 100.0 in
                      let new_buy_price_float = top_ask_price_float *. (1.0 -. ((2.0 *. asset_fee) +. profit_threshold)) in
                      Primitives.Price.of_string_exn ~scale:instrument.price_precision 
                        (Printf.sprintf "%.*f" instrument.price_precision new_buy_price_float)
                | _ -> tick.bid (* fallback *)
                in

                SharedState.amend_order_with_callback
                  ~order ~new_price:new_buy_price ~cmd_buffer ~section
                  ~qty_precision:instrument.qty_precision
                  ~post_amend_callback:(fun () -> Lwt.return_unit)
            | [] -> Lwt.return_unit
            | _ ->
                debug_f ~section "Multiple buy orders for %s, skipping adjustment." tick.symbol
            )
        | None -> Lwt.return_unit)
    | None ->
        warning_f ~section "No configuration found for %s" tick.symbol

  (** Handle sell order fill cleanup and resume logic (common for both GMM and VMM) *)
  let handle_sell_fill (runtime_cfg: Config.runtime_cfg) cmd_buffer symbol qty_float ~is_vmm =
    (* Note: inventory already updated by caller *)
    
    (* Clean up fill quantity tracker for VMM only *)
    (if is_vmm then (
      let remaining_qty = ref qty_float in
      let to_remove = ref [] in
      Hashtbl.iter (fun target_order_id stored_qty ->
        if !remaining_qty > 0.0 then (
          if stored_qty <= !remaining_qty then (
            remaining_qty := !remaining_qty -. stored_qty;
            to_remove := target_order_id :: !to_remove
          ) else (
            let new_qty = stored_qty -. !remaining_qty in
            Hashtbl.replace fill_qty_tracker target_order_id new_qty;
            remaining_qty := 0.0
          )
        )
      ) fill_qty_tracker;
      List.iter (Hashtbl.remove fill_qty_tracker) !to_remove;
      Lwt.return_unit
    ) else (
      Lwt.return_unit
    )) >>= fun () ->
    
    (* Refresh balances before checking inventory (both strategies) *)
    SharedState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
    refresh_asset_balance symbol >>= fun () ->
    
    (* Check if we can resume placing buy orders (both strategies) *)
    let current_inventory = get_current_inventory symbol in
    info_f ~section "After sell fill, current inventory for %s: %.8f" symbol current_inventory >>= fun () ->
    
    (* Resume buy orders if inventory dropped below max exposure *)
    let asset_cfg_opt = SharedState.get_asset_config runtime_cfg symbol in
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
          match Hashtbl.find_opt SharedState.open_orders order_id with
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
              K.Kraken_balances.handle_fill_event fill_event >>= fun () ->
              (SharedState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->

              if not (Hashtbl.mem SharedState.open_orders order_id) then (
                info_f ~section "Order %s completely filled" order_id >>= fun () ->

                (* Get asset config to determine strategy *)
                let asset_cfg_opt = SharedState.get_asset_config runtime_cfg symbol in
                (match asset_cfg_opt with
                | Some asset_cfg ->
                    (* Handle fills based on order side and strategy *)
                    if order.side = Some Core.Buy then (
                      (* Update inventory for both strategies *)
                      let qty_float = Float.of_string (Primitives.Qty.to_string qty) in
                      update_inventory symbol qty_float;
                      info_f ~section "Buy fill: Updated inventory for %s by +%.8f" symbol qty_float >>= fun () ->
                      
                      match asset_cfg.strategy with
                      | Config.GMM ->
                          (* GMM: recreate orders immediately *)
                          manage_orders runtime_cfg symbol cmd_buffer
                      | Config.VMM ->
                          (* VMM: place calculated sell order based on fill price *)
                          
                          (match K.Kraken_incoming_data.get_instrument symbol with
                          | Some instrument ->
                              let buy_price_float = Float.of_string (Primitives.Price.to_string price) in
                              let asset_fee = get_maker_fee symbol in
                              let profit_threshold = runtime_cfg.profit_threshold_pct /. 100.0 in
                              let target_sell_price_float = buy_price_float *. (1.0 +. ((2.0 *. asset_fee) +. profit_threshold)) in
                              let target_sell_price = Primitives.Price.of_string_exn ~scale:instrument.price_precision 
                                (Printf.sprintf "%.*f" instrument.price_precision target_sell_price_float) in
                              
                              Hashtbl.replace fill_qty_tracker order.order_id qty_float;
                              
                              (match SharedState.get_price symbol with
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
                                      info_f ~section "Buy order %s filled at %s. Placed sell order for %s at %s (target: %s)"
                                        order_id (Primitives.Price.to_string price) symbol 
                                        (Primitives.Price.to_string sell_price)
                                        (Primitives.Price.to_string target_sell_price)
                                  | None ->
                                      error_f ~section "Failed to create sell order for %s after buy fill" symbol)
                              | None ->
                                  error_f ~section "No price info for %s, cannot place sell order" symbol)
                          | None ->
                              error_f ~section "No instrument data for %s, cannot calculate target sell price" symbol)
                      | _ -> Lwt.return_unit
                    ) else if order.side = Some Core.Sell then (
                      (* Update inventory for both strategies *)
                      let qty_float = Float.of_string (Primitives.Qty.to_string qty) in
                      update_inventory symbol (-.qty_float);
                      info_f ~section "Sell fill: Updated inventory for %s by -%.8f" symbol qty_float >>= fun () ->
                      
                      match asset_cfg.strategy with
                      | Config.GMM ->
                          (* GMM: refresh balances and potentially resume buy orders *)
                          handle_sell_fill runtime_cfg cmd_buffer symbol qty_float ~is_vmm:false
                      | Config.VMM ->
                          (* VMM: cleanup fill tracker, refresh balances, and potentially resume buy orders *)
                          handle_sell_fill runtime_cfg cmd_buffer symbol qty_float ~is_vmm:true
                      | _ -> Lwt.return_unit
                    ) else (
                      Lwt.return_unit
                    )
                | None ->
                    warning_f ~section "No configuration found for %s, cannot handle fill properly" symbol
                )
              ) else (
                debug_f ~section "Order %s partially filled, order still exists" order_id >>= fun () ->
                Lwt.return_unit
              )
          | None ->
              warning_f ~section "Fill event for unknown order %s, attempting to create orders anyway" order_id >>= fun () ->
              (SharedState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->
              SharedState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
              refresh_asset_balance symbol >>= fun () ->
              manage_orders runtime_cfg symbol cmd_buffer
        ) else (
          debug_f ~section "Fill event for %s not in managed symbols, ignoring" symbol >>= fun () ->
          Lwt.return_unit
        )
    | Ack { order_id; client_id; state; _ } ->
        (match state with
        | Open ->
            debug_f ~section "Ack for order %s (client_id: %s) - order opened" order_id client_id
        | Canceled | Rejected ->
            let symbol_and_side_opt = Hashtbl.fold (fun _ (order: K.Kraken_common_types.order) acc ->
              match order.client_id with
              | Some order_client_id when String.equal order_client_id client_id ->
                  Some (order.order_symbol, order.side)
              | _ -> acc
            ) SharedState.open_orders None in
            
            (match symbol_and_side_opt with
            | Some (symbol, side) when List.mem symbol symbols ->
                info_f ~section "Order %s cancelled/rejected for %s, side=%s"
                  order_id symbol
                  (match side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown") >>= fun () ->
                
                (* Only create new orders if it was a buy order that was cancelled/rejected *)
                if side = Some Core.Buy then (
                  (SharedState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->
                  SharedState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
                  refresh_asset_balance symbol >>= fun () ->
                  manage_orders runtime_cfg symbol cmd_buffer
                ) else (
                  Lwt.return_unit
                )
            | _ ->
                debug_f ~section "Ack event for order %s not in managed symbols or not found" order_id >>= fun () ->
                Lwt.return_unit)
        | Filled ->
            Lwt.return_unit)
    | _ -> Lwt.return_unit
end

(** Start the unified market making strategy *)
let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config) ~tick_buffer ~cmd_buffer ~exec_buffer ~strategy_type =
  let strategy_name = match strategy_type with
    | Config.GMM -> "Greedy (GMM)"
    | Config.VMM -> "Valley (VMM)"
    | _ -> "Unknown"
  in
  
  info_f ~section "Starting unified market making strategy: %s" strategy_name >>= fun () ->

  (SharedState.wait_for_snapshot (fun () -> K.Kraken_incoming_data.wait_for_snapshot ())) () >>= fun () ->
  (SharedState.wait_for_instruments (fun () -> K.Kraken_incoming_data.wait_for_instruments ())) () >>= fun () ->

  SharedState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
  SharedState.initialize_orders runtime_cfg strategy_type (fun () -> K.Kraken_incoming_data.get_all_open_orders ()) >>= fun () ->

  let managed_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
    match asset.strategy with
    | s when s = strategy_type -> Some asset.symbol
    | _ -> None
  ) runtime_cfg.assets in

  info_f ~section "Starting MM strategy for symbols: [%s]" (String.concat ", " managed_symbols) >>= fun () ->

  (* Initialize asset balances for all symbols *)
  Lwt_list.iter_s (fun symbol ->
    State.refresh_asset_balance symbol
  ) managed_symbols >>= fun () ->

  (* Register event-driven execution processor *)
  let process_execution event =
    State.handle_execution runtime_cfg cmd_buffer managed_symbols event
  in
  
  let consumer_name = match strategy_type with
    | Config.GMM -> "gmm_execution"
    | Config.VMM -> "vmm_execution"
    | _ -> "mm_execution"
  in
  Ringbuffer.create_consumer exec_buffer ~name:consumer_name ~processor:process_execution;

  (* Register event-driven tick processor *)
  let process_tick (tick : Event.tick) =
    (if List.mem tick.symbol managed_symbols then (
      SharedState.update_price tick >>= fun () ->
      (SharedState.sync_open_orders (fun () -> K.Kraken_incoming_data.get_all_open_orders ())) () >>= fun () ->
      State.check_and_adjust_orders runtime_cfg cmd_buffer tick >>= fun () ->
      SharedState.refresh_usd_balance (fun () -> K.Kraken_balances.wait_for_balances ()) >>= fun () ->
      State.refresh_asset_balance tick.symbol >>= fun () ->
      State.manage_orders runtime_cfg tick.symbol cmd_buffer
    ) else (
      Lwt.return_unit
    ))
  in
  
  let tick_consumer_name = match strategy_type with
    | Config.GMM -> "gmm_tick"
    | Config.VMM -> "vmm_tick"
    | _ -> "mm_tick"
  in
  Ringbuffer.create_consumer tick_buffer ~name:tick_consumer_name ~processor:process_tick;

  fst (Lwt.wait ())

