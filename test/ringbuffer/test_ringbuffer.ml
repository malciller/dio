open Alcotest
open Ringbuffer               (* dio.ringbuffer *)

(* ------------------------------------------------------------------ *)
let test_fifo () =
  let q = RingBuffer.create 4 in
  List.iter (fun x -> assert (RingBuffer.push q x)) [ 1; 2; 3 ];
  let popped =
    List.filter_map (fun () -> RingBuffer.pop_opt q) [ (); (); () ]
  in
  check (list int) "fifo order" [ 1; 2; 3 ] popped

let test_overflow () =
  let q = RingBuffer.create 2 in
  assert (RingBuffer.push q 1);
  assert (RingBuffer.push q 2);
  assert (not (RingBuffer.push q 3))       (* correctly refuses when full *)

(* ------------------------------------------------------------------ *)
let () =
  run "RingBuffer"
    [ ("basic",                      
       [ test_case "fifo"      `Quick test_fifo
       ; test_case "overflow"  `Quick test_overflow
       ] )
    ]
