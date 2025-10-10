open Lwt.Infix
open Alcotest_lwt 
open Dio_types

(* Comprehensive config parsing and validation tests *)
let test_config_parsing_and_validation _switch () =
  let open Alcotest in
  
  (* Test successful config parsing *)
      let json = Yojson.Safe.from_file "config.json" in
      let runtime_cfg : Config.runtime_cfg = Config.runtime_cfg_of_yojson_exn json in
  check bool "config should parse successfully" true true;

  (* Test config validation *)
  let validation_result = Config.validate_runtime_cfg runtime_cfg in
  (match validation_result with
   | Ok () -> check bool "config should validate successfully" true true
   | Error msg -> fail ("Config validation failed: " ^ msg));

  (* Test specific config values *)
  check int "queues_cap should be 1024" 1024 runtime_cfg.queues_cap;
  check (float 0.001) "profit_threshold_pct should be 0.0010" 0.0010 runtime_cfg.profit_threshold_pct;

  (* Test asset config *)
  check int "should have 1 asset" 1 (List.length runtime_cfg.assets);
  let asset = List.hd runtime_cfg.assets in
  check string "asset symbol should be BTC/USD" "BTC/USD" asset.symbol;
  check bool "strategy should be Grid" true (asset.strategy = Config.Grid);

      Lwt.return_unit

(* Comprehensive engine components initialization tests *)
let test_engine_components_init _switch () =
  let open Alcotest in

  (* Test ringbuffer creation with telemetry *)
  let buffer_cap = 1024 in
  let tick_buffer = Ringbuffer.create ~name:"test_tick_buffer" buffer_cap in
  let exec_buffer = Ringbuffer.create ~name:"test_exec_buffer" buffer_cap in
  let cmd_buffer = Ringbuffer.create ~name:"test_cmd_buffer" buffer_cap in

  (* Verify buffer capacities *)
  check int "tick buffer capacity" buffer_cap (Array.length tick_buffer.buf);
  check int "exec buffer capacity" buffer_cap (Array.length exec_buffer.buf);
  check int "cmd buffer capacity" buffer_cap (Array.length cmd_buffer.buf);

  (* Verify buffers are empty initially *)
  check bool "tick buffer should be empty initially" true (Ringbuffer.is_empty tick_buffer);
  check bool "exec buffer should be empty initially" true (Ringbuffer.is_empty exec_buffer);
  check bool "cmd buffer should be empty initially" true (Ringbuffer.is_empty cmd_buffer);

  (* Test ringbuffer operations *)
  let test_tick : Event.tick = {
    src = "kraken";
    symbol = "BTC/USD";
    bid = Primitives.Price.of_string_exn ~scale:8 "50000.00";
    ask = Primitives.Price.of_string_exn ~scale:8 "50001.00";
    current_price = Primitives.Price.of_string_exn ~scale:8 "50000.50";
    ts = Int64.of_float (Unix.time () *. 1000000.);
    ask_qty = 1.5;
    bid_qty = 2.0;
    change = 100.0;
    change_pct = 0.002;
    high = 51000.0;
    last_price = 50000.5;
    low = 49000.0;
    volume = 1000.0;
    vwap = 50000.0;
  } in

  Ringbuffer.push tick_buffer test_tick >>= fun () ->
  check bool "tick buffer should not be empty after push" false (Ringbuffer.is_empty tick_buffer);

  (* Test buffer pop (since there's no peek function) *)
  Ringbuffer.try_pop tick_buffer >>= fun peeked ->
  (match peeked with
   | Some tick -> check string "popped tick symbol" "BTC/USD" tick.symbol
   | None -> fail "Should be able to pop tick from buffer");

  (* Test state management integration *)
  let initial_state = State.initial in
  let updated_state = State.update_price "BTC/USD" (Primitives.Price.of_string_exn ~scale:8 "50000.50") initial_state in

  (match State.get_price "BTC/USD" updated_state with
   | Some price -> check bool "price should be updated" true (Primitives.Price.equal price (Primitives.Price.of_string_exn ~scale:8 "50000.50"))
   | None -> fail "Price should be retrievable after update");

    Lwt.return_unit
    
(* Comprehensive type system and data flow integration tests *)
let test_type_system_consistency _switch () =
  let open Alcotest in
  
  (* Test primitive types work together *)
    let symbol = "BTC/USD" in
    let qty = Primitives.Qty.of_string_exn ~scale:8 "0.01000000" in
  let grid_interval = Primitives.Fixed.of_string_exn ~scale:8 "100.00000000" in
  let sell_mult = Primitives.Fixed.of_string_exn ~scale:8 "0.990" in
  let price = Primitives.Price.of_string_exn ~scale:8 "50000.00" in
    
  (* Test all strategy types *)
  let strategies = [Config.Grid; Config.GMM; Config.VMM] in
  List.iter (fun strategy ->
    let asset_cfg : Config.asset_cfg = {
      Config.symbol;
      qty;
      grid_interval = Some grid_interval;
      sell_mult = Some sell_mult;
      min_usd_balance = Some (Primitives.Fixed.of_string_exn ~scale:2 "1000.00");
      max_exposure = Some (Primitives.Fixed.of_string_exn ~scale:2 "5000.00");
      strategy;
    } in

    (* Test JSON round-trip for all strategy types *)
    let json = Config.asset_cfg_to_yojson asset_cfg in
    let asset_cfg' = Config.asset_cfg_of_yojson_exn json in

    check string "symbol round-trip" symbol asset_cfg'.symbol;
    check bool "strategy round-trip" true (asset_cfg'.strategy = strategy);
  ) strategies;

  (* Test runtime config round-trip *)
  let runtime_cfg : Config.runtime_cfg = {
    assets = [{
      symbol;
      qty;
      grid_interval = Some grid_interval;
      sell_mult = Some sell_mult;
      min_usd_balance = None;
      max_exposure = None;
      strategy = Config.Grid;
    }];
    queues_cap = 1024;
    profit_threshold_pct = 0.001;
  } in

  let json = Config.runtime_cfg_to_yojson runtime_cfg in
  let runtime_cfg' = Config.runtime_cfg_of_yojson_exn json in

  check int "runtime config queues_cap round-trip" 1024 runtime_cfg'.queues_cap;
  check (float 0.001) "runtime config profit_threshold_pct round-trip" 0.001 runtime_cfg'.profit_threshold_pct;

  (* Test event types *)
  let tick_event : Event.tick = {
    src = "kraken";
    symbol;
    bid = price;
    ask = Primitives.Price.of_string_exn ~scale:8 "50001.00";
    current_price = Primitives.Price.of_string_exn ~scale:8 "50000.50";
    ts = Int64.of_float (Unix.time () *. 1000000.);
    ask_qty = 1.0;
    bid_qty = 1.0;
    change = 0.0;
    change_pct = 0.0;
    high = 50001.0;
    last_price = 50000.5;
    low = 50000.0;
    volume = 100.0;
    vwap = 50000.25;
  } in

  let json = Event.tick_to_yojson tick_event in
  let tick_event' = Event.tick_of_yojson_exn json in

  check string "tick event symbol" symbol tick_event'.symbol;

  (* Test state operations *)
  let state = State.initial in
  let state = State.update_price symbol price state in
  let state = State.inc_pending symbol state in
  let state = State.inc_pending symbol state in

  (match State.get_price symbol state with
   | Some p -> check bool "state price update" true (Primitives.Price.equal p price)
   | None -> fail "Price should be in state");

  (* Test pending orders count - need to access the map directly *)
  let pending_count = State.SMap.find_opt symbol state.pending_orders |> Option.value ~default:0 in
  check int "pending orders count" 2 pending_count;

  (* Test transaction history integration *)
  let tx : Primitives.transaction = {
    id = "test-tx-123";
    asset = "BTC";
    amount = 0.001;
    timestamp = Int64.of_float (Unix.time () *. 1000000.);
    transaction_type = Trade {
      order_id = "test-order-123";
      side = `Buy;
      price = price;
      qty = qty;
    };
    cost_basis = Some 50000.00;
    total_cost = Some 50.00;
    balance_after = 1.5;
  } in

  Transaction_history.add_transaction tx |> ignore;

  let transactions = Transaction_history.get_transactions "BTC" in
  check bool "should have transaction" true (List.length transactions > 0);

  let first_tx = List.hd transactions in
  check string "transaction asset" "BTC" first_tx.asset;

  Lwt.return_unit

(* Error handling and validation tests *)
let test_error_handling _switch () =
  let open Alcotest in

  (* Test invalid config validation *)
  let invalid_config : Config.runtime_cfg = {
    assets = [];
    queues_cap = -1; (* Invalid negative capacity *)
    profit_threshold_pct = -0.1; (* Invalid negative threshold *)
  } in

  (match Config.validate_runtime_cfg invalid_config with
   | Error _ -> check bool "invalid config should fail validation" true true
   | Ok () -> fail "Invalid config should not pass validation");

  (* Test invalid asset config *)
  let invalid_asset : Config.asset_cfg = {
    symbol = "";
    qty = Primitives.Qty.of_string_exn ~scale:8 "0.00000000";
    grid_interval = Some (Primitives.Fixed.of_string_exn ~scale:8 "-100.0"); (* Invalid negative interval *)
    sell_mult = Some (Primitives.Fixed.of_string_exn ~scale:8 "1.5"); (* Invalid multiplier > 1 *)
    min_usd_balance = None;
    max_exposure = None;
    strategy = Config.Grid;
  } in

  let runtime_cfg_with_invalid_asset : Config.runtime_cfg = {
    assets = [invalid_asset];
    queues_cap = 1024;
    profit_threshold_pct = 0.001;
  } in

  (match Config.validate_runtime_cfg runtime_cfg_with_invalid_asset with
   | Error _ -> check bool "config with invalid asset should fail validation" true true
   | Ok () -> fail "Config with invalid asset should not pass validation");

  (* Test primitive parsing errors *)
  (try
    let _ = Primitives.Qty.of_string_exn ~scale:8 "invalid" in
    fail "Should fail to parse invalid quantity"
  with _ -> check bool "invalid quantity parsing should fail" true true);

  (try
    let _ = Primitives.Price.of_string_exn ~scale:8 "not_a_price" in
    fail "Should fail to parse invalid price"
  with _ -> check bool "invalid price parsing should fail" true true);

  Lwt.return_unit

(* Data flow integration tests *)
let test_data_flow_integration _switch () =
  let open Alcotest in

  (* Create complete trading setup *)
  let symbol = "BTC/USD" in
  let buffer_cap = 64 in

  (* Initialize components *)
  let tick_buffer : Event.tick Ringbuffer.t = Ringbuffer.create ~name:"flow_tick_buffer" buffer_cap in
  let exec_buffer : Core.market_event Ringbuffer.t = Ringbuffer.create ~name:"flow_exec_buffer" buffer_cap in
  let state = ref State.initial in

  (* Create sample market data *)
  let price1 = Primitives.Price.of_string_exn ~scale:8 "50000.00" in
  let price2 = Primitives.Price.of_string_exn ~scale:8 "50001.00" in

  let tick1 : Event.tick = {
    src = "kraken";
    symbol;
    bid = price1;
    ask = price2;
    current_price = Primitives.Price.of_string_exn ~scale:8 "50000.50";
    ts = Int64.of_float (Unix.time () *. 1000000.);
    ask_qty = 1.0;
    bid_qty = 1.0;
    change = 0.0;
    change_pct = 0.0;
    high = 50001.0;
    last_price = 50000.5;
    low = 50000.0;
    volume = 100.0;
    vwap = 50000.25;
  } in

  let tick2 : Event.tick = {
    src = "kraken";
    symbol;
    bid = Primitives.Price.of_string_exn ~scale:8 "50000.50";
    ask = Primitives.Price.of_string_exn ~scale:8 "50001.50";
    current_price = Primitives.Price.of_string_exn ~scale:8 "50001.00";
    ts = Int64.of_float ((Unix.time () +. 1.0) *. 1000000.);
    ask_qty = 1.5;
    bid_qty = 2.0;
    change = 1.0;
    change_pct = 0.00002;
    high = 50001.5;
    last_price = 50001.0;
    low = 50000.0;
    volume = 150.0;
    vwap = 50000.75;
  } in

  (* Simulate data flow: tick -> buffer -> state update *)
  Ringbuffer.push tick_buffer tick1 >>= fun () ->
  Ringbuffer.push tick_buffer tick2 >>= fun () ->

  (* Process ticks and update state *)
  let process_tick (tick : Event.tick) =
    state := State.update_price tick.symbol tick.ask !state;
    state := State.inc_pending tick.symbol !state
  in

  Ringbuffer.pop tick_buffer >>= fun tick1 ->
  process_tick tick1;
  Ringbuffer.pop tick_buffer >>= fun tick2 ->
  process_tick tick2;

  (* Verify state updates *)
  (match State.get_price symbol !state with
   | Some price -> check bool "final price should be updated" true (Primitives.Price.equal price (Primitives.Price.of_string_exn ~scale:8 "50001.50"))
   | None -> fail "Price should be in state");

  let pending_count = State.SMap.find_opt symbol !state.pending_orders |> Option.value ~default:0 in
  check int "should have pending orders" 2 pending_count;

  (* Test execution event processing - using Fill event *)
  let exec_event = Core.Fill {
    symbol;
    order_id = "test-order-123";
    client_id = "test-client";
    price = price1;
    qty = Primitives.Qty.of_string_exn ~scale:8 "0.00100000";
    side = Core.Buy;
    ts = Int64.of_float (Unix.time () *. 1000000.);
  } in

  Ringbuffer.push exec_buffer exec_event >>= fun () ->

  Ringbuffer.pop exec_buffer >>= fun exec_event ->
  (match exec_event with
   | Core.Fill e ->
     check string "execution symbol" symbol e.symbol;
     check string "execution order_id" "test-order-123" e.order_id;
     state := State.dec_pending e.symbol !state;
     Lwt.return_unit
   | _ -> fail "Should have execution event") >>= fun () ->

  let final_pending_count = State.SMap.find_opt symbol !state.pending_orders |> Option.value ~default:0 in
  check int "pending orders should be decremented" 1 final_pending_count;

    Lwt.return_unit
    
(* Strategy integration tests *)
let test_strategy_integration _switch () =
  let open Alcotest in

  (* Test strategy configuration parsing *)
  let grid_config : Config.asset_cfg = {
    symbol = "BTC/USD";
    qty = Primitives.Qty.of_string_exn ~scale:8 "0.00100000";
    grid_interval = Some (Primitives.Fixed.of_string_exn ~scale:8 "100.00000000");
    sell_mult = Some (Primitives.Fixed.of_string_exn ~scale:8 "0.990");
    min_usd_balance = None;
    max_exposure = None;
    strategy = Config.Grid;
  } in

  let gmm_config : Config.asset_cfg = {
    symbol = "ETH/USD";
    qty = Primitives.Qty.of_string_exn ~scale:8 "0.01000000";
    grid_interval = None;
    sell_mult = None;
    min_usd_balance = Some (Primitives.Fixed.of_string_exn ~scale:2 "1000.00");
    max_exposure = Some (Primitives.Fixed.of_string_exn ~scale:2 "5000.00");
    strategy = Config.GMM;
  } in

  (* Test strategy-specific validation *)
  (match Config.validate_asset_cfg grid_config with
   | Ok () -> check bool "grid strategy config should be valid" true true
   | Error msg -> fail ("Grid config should be valid: " ^ msg));

  (match Config.validate_asset_cfg gmm_config with
   | Ok () -> check bool "gmm strategy config should be valid" true true
   | Error msg -> fail ("GMM config should be valid: " ^ msg));

  (* Test invalid strategy configurations *)
  let invalid_grid = { grid_config with grid_interval = None } in
  (match Config.validate_asset_cfg invalid_grid with
   | Error _ -> check bool "grid without interval should be invalid" true true
   | Ok () -> fail "Grid strategy without interval should be invalid");

  let invalid_gmm = { gmm_config with min_usd_balance = None; max_exposure = None } in
  (match Config.validate_asset_cfg invalid_gmm with
   | Error _ -> check bool "gmm without min balance and max exposure should be invalid" true true
   | Ok () -> fail "GMM strategy without min balance and max exposure should be invalid");

  Lwt.return_unit

let () =
  Lwt_main.run (run "integration_tests" [
      "Configuration", [
        test_case "Config parsing and validation" `Quick test_config_parsing_and_validation;
      ];
      "Engine", [
        test_case "Engine components initialization" `Quick test_engine_components_init;
      ];
      "Types", [
        test_case "Type system consistency" `Quick test_type_system_consistency;
      ];
      "Error Handling", [
        test_case "Error handling and validation" `Quick test_error_handling;
      ];
      "Data Flow", [
        test_case "Data flow integration" `Quick test_data_flow_integration;
      ];
      "Strategy", [
        test_case "Strategy integration" `Quick test_strategy_integration;
      ];
    ])
