open Alcotest
open Ringbuffer

(* ------------------------------------------------------------------ *)
let test_fifo () =
  let q = create 4 in
  List.iter (fun x -> assert (push q x)) [ 1; 2; 3 ];
  let popped =
    List.filter_map (fun () -> pop_opt q) [ (); (); () ]
  in
  check (list int) "fifo order" [ 1; 2; 3 ] popped

let test_overflow () =
  let q = Ringbuffer.create 2 in
  assert (Ringbuffer.push q 1);
  assert (Ringbuffer.push q 2);
  assert (not (Ringbuffer.push q 3))       (* correctly refuses when full *)

(* ------------------------------------------------------------------ *)
let () =
  run "RingBuffer"
    [ ("basic",                      
       [ test_case "fifo"      `Quick test_fifo
       ; test_case "overflow"  `Quick test_overflow
       ] )
    ]
