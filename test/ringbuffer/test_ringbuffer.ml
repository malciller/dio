open Alcotest_lwt
open Dio_types

(* ------------------------------------------------------------------ *)
let test_fifo _switch () =
  let q = Ringbuffer.create 4 in
  let%lwt () = Lwt_list.iter_s (Ringbuffer.push q) [ 1; 2; 3 ] in
  let%lwt p1 = Ringbuffer.pop q in
  let%lwt p2 = Ringbuffer.pop q in
  let%lwt p3 = Ringbuffer.pop q in
  let popped = [ p1; p2; p3 ] in
  Alcotest.(check (list int)) "fifo order" [ 1; 2; 3 ] popped;
  Lwt.return_unit

let test_overflow _switch () =
  let q = Ringbuffer.create 2 in
  let%lwt () = Ringbuffer.push q 1 in
  let%lwt () = Ringbuffer.push q 2 in
  
  (* The push operation will now wait, so we test this by checking
     if it times out, proving it's waiting for a pop. *)
  let push_promise = Ringbuffer.push q 3 in
  
  let timeout_promise = Lwt_unix.sleep 0.1 in

  let%lwt () = Lwt.choose [ push_promise; timeout_promise ] in
  
  Alcotest.(check bool) "push is still pending" true (Lwt.is_sleeping push_promise);

  (* Clean up by cancelling the pending push *)
  Lwt.cancel push_promise;
  Lwt.return_unit

(* ------------------------------------------------------------------ *)
let () =
  Lwt_main.run
    (run "RingBuffer"
      [ ("basic",                      
          [ test_case "fifo"      `Quick test_fifo
          ; test_case "overflow"  `Quick test_overflow
          ] )
      ])
