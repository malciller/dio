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
  let queue = Discord_webhook.get_message_queue_for_test () in
  let result_promise, result_resolver = Lwt.wait () in
  
  (* Create event-driven consumer to capture the message *)
  let processor payload_from_queue =
    let unwrapped_payload = match payload_from_queue with
      | Discord_webhook.Fill p -> p
      | Discord_webhook.Balance _ -> Alcotest.fail "Expected Fill payload but got Balance payload" in
    Lwt.wakeup result_resolver unwrapped_payload;
    Lwt.return_unit
  in
  
  Ringbuffer.create_consumer queue ~name:"test_discord_consumer" ~processor;
  
  Discord_webhook.send_message (Discord_webhook.Fill test_payload) >>= fun () ->
  result_promise >>= fun unwrapped_payload ->
  Alcotest.(check string "order_id should match" test_payload.order_id unwrapped_payload.order_id);
  Lwt.return_unit

let suite =
  [
    ( "Discord_webhook",
      [ test_case "order completion message" `Quick test_order_completion_message
      ] );
  ]

let () = Lwt_main.run (run "Discord_webhook" suite)
