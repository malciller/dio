open Alcotest_lwt
open Lwt.Infix
open Dio_types

let test_order_completion_message _switch () =
  let test_message = "Dio Webhook Test - SUCCESS" in
  Discord_webhook.send_message test_message >>= fun () ->
  let queue = Discord_webhook.get_message_queue_for_test () in
  Ringbuffer.pop queue >>= fun message_from_queue ->
  Alcotest.(check string "message should be in queue" test_message message_from_queue);
  Lwt.return_unit

let suite =
  [
    ( "Discord_webhook",
      [ test_case "order completion message" `Quick test_order_completion_message
      ] );
  ]

let () = Lwt_main.run (run "Discord_webhook" suite)
