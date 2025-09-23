open Alcotest_lwt
open Dio_types
open Lwt.Infix

(* ------------------------------------------------------------------ *)
let test_fifo _switch () =
  let q = Ringbuffer.create 4 in
  let results = ref [] in
  let result_promise, result_resolver = Lwt.wait () in
  
  (* Create event-driven consumer that collects results *)
  let processor item =
    results := item :: !results;
    if List.length !results = 3 then
      Lwt.wakeup result_resolver (List.rev !results);
    Lwt.return_unit
  in
  
  Ringbuffer.create_consumer q ~name:"test_consumer" ~processor;
  
  (* Push items and wait for processing *)
  Lwt_list.iter_s (Ringbuffer.push q) [ 1; 2; 3 ] >>= fun () ->
  result_promise >>= fun popped ->
  
  Alcotest.(check (list int)) "fifo order" [ 1; 2; 3 ] popped;
  Lwt.return_unit

let test_overflow _switch () =
  let q = Ringbuffer.create 2 in
  Ringbuffer.push q 1 >>= fun () ->
  Ringbuffer.push q 2 >>= fun () ->
  
  (* The push operation will now wait, so we test this by checking
     if it times out, proving it's waiting for a pop. *)
  let push_promise = Ringbuffer.push q 3 in
  
  let timeout_promise = Lwt_unix.sleep 0.1 in

  Lwt.choose [ push_promise; timeout_promise ] >>= fun () ->
  
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
