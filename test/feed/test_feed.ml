(* Remove warning suppression *)
(* [@@@warning "-32-34"] *) 

(* test/feed/test_feed.ml *)
open Alcotest_lwt
open Lwt.Infix
open Types




(* Initialize RNG ... *) 
let () = Mirage_crypto_rng_unix.use_default ()

(* --- Mocking Kraken.Ws_feed --- *) 

module Mock_state = struct
  let start_called = ref false
  let start_executions_called = ref false
  let last_on_tick_callback = ref (fun _ -> Lwt.return_unit)
  let last_on_exec_callback = ref (fun _ -> Lwt.return_unit)

  let reset () =
    start_called := false;
    start_executions_called := false;
    last_on_tick_callback := (fun _ -> Lwt.return_unit);
    last_on_exec_callback := (fun _ -> Lwt.return_unit)
end

(* Mock implementation matching the Engine.Feed.WS interface *) 
module Mock_kraken_ws_feed : Engine.Feed.WS = struct (* Declare implementation of the signature *) 
  (* Use the config type defined in the WS interface (which is Core.config) *) 
  type config = Core.config (* Use Core.config directly *) 

  let start (cfg: config) ~on_tick = 
    let _ = cfg.auth_token in 
    Mock_state.start_called := true;
    Mock_state.last_on_tick_callback := on_tick;
    Lwt.return_unit

  let start_executions (cfg: config) ~on_execution = 
    let _ = cfg.auth_token in 
    Mock_state.start_executions_called := true;
    Mock_state.last_on_exec_callback := on_execution;
    Lwt.return_unit

  let get_open_buy_orders () = 
    Hashtbl.create 0 
end

(* Instantiate Feed functor with the Mock *) 
module Mock_feed = Engine.Feed.Make(Mock_kraken_ws_feed)

(* REMOVED Kraken shadowing *)
(* REMOVED dummy references *) 

(* --- Test Fixtures & Helpers --- *) 
let test_config_no_auth : Core.config = { (* Use explicit Core.config *) 
  ws_host = "localhost"; ws_port = 8080; ws_path = "/test";
  symbols = ["BTC/USD"]; auth_token = None;
}
let test_config_with_auth : Core.config = {
  ws_host = "localhost"; ws_port = 8080; ws_path = "/test";
  symbols = ["BTC/USD"]; auth_token = Some "test_token";
}

let dummy_on_tick _tick = Lwt.return_unit
let dummy_on_exec _events = Lwt.return_unit

(* --- Test Cases --- *) 

let test_start_calls_mock _switch () =
  Mock_state.reset ();
  Mock_feed.start test_config_no_auth ~on_tick:dummy_on_tick >>= fun () -> (* Use Mock_feed *) 
  Alcotest.(check bool "Mock_kraken_ws_feed.start was called") true !Mock_state.start_called;
  Lwt.return_unit

let test_start_exec_calls_mock_with_auth _switch () =
  Mock_state.reset ();
  Mock_feed.start_executions test_config_with_auth ~on_execution:dummy_on_exec >>= fun () -> (* Use Mock_feed *) 
  Alcotest.(check bool "Mock_kraken_ws_feed.start_executions called (with auth)") true !Mock_state.start_executions_called;
  Lwt.return_unit

let test_start_exec_skips_mock_without_auth _switch () =
  Mock_state.reset ();
  Mock_feed.start_executions test_config_no_auth ~on_execution:dummy_on_exec >>= fun () -> (* Use Mock_feed *) 
  Alcotest.(check bool "Mock_kraken_ws_feed.start_executions NOT called (no auth)") false !Mock_state.start_executions_called;
  Lwt.return_unit

let test_callbacks_passed_down _switch () =
  Mock_state.reset ();
  let tick_received = ref false in
  let exec_received = ref false in
  let on_tick_test _ = tick_received := true; Lwt.return_unit in
  let on_exec_test _ = exec_received := true; Lwt.return_unit in

  (* Call the Mock_feed functions *) 
  Mock_feed.start test_config_with_auth ~on_tick:on_tick_test >>= fun () ->
  Mock_feed.start_executions test_config_with_auth ~on_execution:on_exec_test >>= fun () ->

  (* Manually invoke the callbacks stored by the mock *) 
  let sample_exec = [ Core.Ack { order_id = "o"; client_id = "c"; state = Open; ts = 0L } ] in (* List of events *) 

  (* Create a dummy tick for the type check, even though we don't use its value *) 
  let dummy_tick_for_callback = 
    let bid = Primitives.Price.of_string_exn ~scale:1 "1" in
    let ask = Primitives.Price.of_string_exn ~scale:1 "1" in
    { Event.src="test"; symbol="X"; bid; ask; current_price = Primitives.Price.midpoint bid ask; ts=0L }
  in

  !(Mock_state.last_on_tick_callback) dummy_tick_for_callback >>= fun () ->
  !(Mock_state.last_on_exec_callback) sample_exec >>= fun () -> (* Pass list *) 

  Alcotest.(check bool "on_tick callback was invoked via mock") true !tick_received;
  Alcotest.(check bool "on_execution callback was invoked via mock") true !exec_received;
  Lwt.return_unit


(* --- Test Suite --- *)
let suite = [
  "Feed Logic", [
    test_case "start calls underlying start" `Quick test_start_calls_mock;
    test_case "start_exec calls underlying start (with auth)" `Quick test_start_exec_calls_mock_with_auth;
    test_case "start_exec skips underlying start (no auth)" `Quick test_start_exec_skips_mock_without_auth;
    test_case "callbacks are passed down" `Quick test_callbacks_passed_down;
  ];
]

(* --- Runner --- *)
let () =
  Lwt_main.run (Alcotest_lwt.run "Feed" suite)
