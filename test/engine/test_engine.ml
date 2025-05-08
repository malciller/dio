(* test/engine/test_engine.ml *)

        (* brings Engine.Ringbuffer into scope *)

open Dio_types


let test_buffer_push_pop _switch () =
  let buffer = Ringbuffer.create 4 in

  (* two example ticks *)
  let tick1 = 
    let bid = Primitives.Price.of_string_exn ~scale:2 "100.00" in
    let ask = Primitives.Price.of_string_exn ~scale:2 "100.10" in
    { Event.src="t"; symbol="BTC/USD"; 
      bid; ask; current_price = Primitives.Price.midpoint bid ask; ts=0L }
  in
  let tick2 =
    let bid = Primitives.Price.of_string_exn ~scale:2 "50.00" in
    let ask = Primitives.Price.of_string_exn ~scale:2 "50.05" in
    { Event.src="t"; symbol="ETH/USD"; 
      bid; ask; current_price = Primitives.Price.midpoint bid ask; ts=1L }
  in

  Alcotest.(check bool) "push1" true (Ringbuffer.push buffer tick1);
  Alcotest.(check bool) "push2" true (Ringbuffer.push buffer tick2);
  Alcotest.(check int ) "len=2" 2    (Ringbuffer.length buffer);

  (match Ringbuffer.pop_opt buffer with
   | Some t -> Alcotest.(check string) "pop1" "BTC/USD" t.symbol
   | None   -> Alcotest.fail "expected tick1");

  (match Ringbuffer.pop_opt buffer with
   | Some t -> Alcotest.(check string) "pop2" "ETH/USD" t.symbol
   | None   -> Alcotest.fail "expected tick2");

  Alcotest.(check bool) "empty" true (Ringbuffer.is_empty buffer);
  Lwt.return_unit

let () =
  Lwt_main.run
    (Alcotest_lwt.run "Engine"
       [ "Engine - Create Ringbuffer",
         [ Alcotest_lwt.test_case "push-pop" `Quick test_buffer_push_pop ] ])
