open Alcotest_lwt
open Lwt.Infix
open Dio_types

let test_order_completion_message _switch () =
  let test_payload : Discord_webhook.fill_notification_payload = {
    side = Core.Buy;
    asset_name = "BTC";
    qty_str = "0.001";
    value_str = "USD 65,000.00";
    order_id = "TEST-ORDER-123";
    symbol = "BTC/USD";
  } in
  Discord_webhook.send_message test_payload >>= fun () ->
  let queue = Discord_webhook.get_message_queue_for_test () in
  Ringbuffer.pop queue >>= fun payload_from_queue ->
  Alcotest.(check string "order_id should match" test_payload.order_id payload_from_queue.order_id);
  Lwt.return_unit

let suite =
  [
    ( "Discord_webhook",
      [ test_case "order completion message" `Quick test_order_completion_message
      ] );
  ]

let () = Lwt_main.run (run "Discord_webhook" suite)
