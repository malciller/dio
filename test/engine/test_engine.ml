(* test/engine/test_engine.ml *)

        (* brings Engine.Ringbuffer into scope *)

open Dio_types
open Lwt.Infix

let test_buffer_push_pop _switch () =
  let buffer = Ringbuffer.create 4 in

  (* two example ticks *)
  let tick1 = 
    let bid = Primitives.Price.of_string_exn ~scale:2 "100.00" in
    let ask = Primitives.Price.of_string_exn ~scale:2 "100.10" in
    { Event.src="t"; symbol="BTC/USD"; 
      bid; ask; current_price = Primitives.Price.midpoint bid ask; ts=0L;
      ask_qty = 0.0; bid_qty = 0.0; change = 0.0; change_pct = 0.0;
      high = 0.0; last_price = 0.0; low = 0.0; volume = 0.0; vwap = 0.0 }
  in
  let tick2 =
    let bid = Primitives.Price.of_string_exn ~scale:2 "50.00" in
    let ask = Primitives.Price.of_string_exn ~scale:2 "50.05" in
    { Event.src="t"; symbol="ETH/USD"; 
      bid; ask; current_price = Primitives.Price.midpoint bid ask; ts=1L;
      ask_qty = 0.0; bid_qty = 0.0; change = 0.0; change_pct = 0.0;
      high = 0.0; last_price = 0.0; low = 0.0; volume = 0.0; vwap = 0.0 }
  in

  let%lwt () = Ringbuffer.push buffer tick1 in
  let%lwt () = Ringbuffer.push buffer tick2 in
  
  Alcotest.(check int) "len=2" 2 (Ringbuffer.length buffer);

  (* Use try_pop since we know items are available *)
  Ringbuffer.try_pop buffer >>= fun t1_opt ->
  (match t1_opt with
  | Some t1 -> 
      Alcotest.(check string) "pop1" "BTC/USD" t1.symbol;
      Lwt.return_unit
  | None -> 
      Alcotest.fail "Expected first tick"
  ) >>= fun () ->
  
  Ringbuffer.try_pop buffer >>= fun t2_opt ->
  (match t2_opt with
  | Some t2 ->
      Alcotest.(check string) "pop2" "ETH/USD" t2.symbol;
      Lwt.return_unit
  | None -> 
      Alcotest.fail "Expected second tick"
  ) >>= fun () ->

  Alcotest.(check bool) "empty" true (Ringbuffer.is_empty buffer);
  Lwt.return_unit

let () =
  Lwt_main.run
    (Alcotest_lwt.run "Engine"
       [ "Engine - Create Ringbuffer",
         [ Alcotest_lwt.test_case "push-pop" `Quick test_buffer_push_pop ] ])
