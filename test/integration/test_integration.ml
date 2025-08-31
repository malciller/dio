open Lwt.Infix
open Alcotest_lwt 
open Dio_types


(* Config parsing + validation *)
let test_config_parsing_and_validation _switch () =
  Lwt_io.printl "Testing config parsing and validation integration" >>= fun () ->
  
  (* Test that config.json can be parsed and validated *)
  let config_result =
    try
      let json = Yojson.Safe.from_file "/Users/malciller/dev/Dio/test/integration/config.json" in
      let runtime_cfg : Config.runtime_cfg = Config.runtime_cfg_of_yojson_exn json in
      Config.validate_runtime_cfg runtime_cfg
    with
    | exn -> Error (Printexc.to_string exn)
  in
  
  match config_result with
  | Ok () -> 
      Lwt_io.printl "Config parsing and validation successful" >>= fun () ->
      Alcotest.(check bool "config should parse and validate successfully" true true);
      Lwt.return_unit
  | Error msg ->
      Lwt_io.printl ("Config validation failed: " ^ msg) >>= fun () ->
      Alcotest.fail ("Config validation failed: " ^ msg)

(* Engine components initialization *)
let test_engine_components_init _switch () =
  Lwt_io.printl "Testing engine components initialization" >>= fun () ->
  
  try
    (* Test that we can create ring buffers *)
    let buffer_cap = 64 in
    let tick_buffer = Ringbuffer.create buffer_cap in
    let exec_buffer = Ringbuffer.create buffer_cap in
    
    (* Verify buffers are created properly *)
    Alcotest.(check int "tick buffer capacity" buffer_cap (Array.length tick_buffer.buf));
    Alcotest.(check int "exec buffer capacity" buffer_cap (Array.length exec_buffer.buf));
    Alcotest.(check bool "buffers should be empty initially" true (Ringbuffer.is_empty tick_buffer));
    
    Lwt_io.printl "Engine components initialized successfully" >>= fun () ->
    Lwt.return_unit
    
  with exn ->
    Lwt_io.printl ("Engine initialization failed: " ^ Printexc.to_string exn) >>= fun () ->
    Alcotest.fail ("Engine initialization failed: " ^ Printexc.to_string exn)

(* Type system consistency *)
let test_type_system_consistency _switch () =
  Lwt_io.printl "Testing type system consistency across modules" >>= fun () ->
  
  try
    (* Test that types from different modules work together *)
    let symbol = "BTC/USD" in
    let qty = Primitives.Qty.of_string_exn ~scale:8 "0.01000000" in
    let grid_interval = Primitives.Fixed.of_string_exn ~scale:8 "1.0" in
    let sell_mult = Primitives.Fixed.of_string_exn ~scale:8 "0.999" in
    
    (* Create asset config using primitives from different modules *)
    let asset_cfg : Config.asset_cfg = {
      Config.symbol;
      qty;
      grid_interval;
      sell_mult;
      strategy = Config.Grid;
    } in
    
    (* Test JSON round-trip *)
    let json = Config.asset_cfg_to_yojson asset_cfg in
    let asset_cfg' = Config.asset_cfg_of_yojson_exn json in
    
    Alcotest.(check string "symbol round-trip" symbol asset_cfg'.symbol);
    Alcotest.(check bool "strategy round-trip" true (asset_cfg'.strategy = Config.Grid));
    
    Lwt_io.printl "Type system consistency verified" >>= fun () ->
    Lwt.return_unit
    
  with exn ->
    Lwt_io.printl ("Type system test failed: " ^ Printexc.to_string exn) >>= fun () ->
    Alcotest.fail ("Type system test failed: " ^ Printexc.to_string exn)

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
    ])
