open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Lwt_log_core
open Dio_types
open Dio_types.Core

module Notification = struct
  type fill_notification_payload = {
    side: side;
    asset_name: string;
    qty_str: string;
    value_str: string;
    order_id: string;
    symbol: string;
  }
end

type fill_notification_payload = Notification.fill_notification_payload

let section = Section.make "notification.discord"
let message_queue = Ringbuffer.create 100
let logged_no_url = ref false

let make_order_embed (payload: Notification.fill_notification_payload) =
  let side_str = match payload.side with | Core.Buy -> "buy" | Core.Sell -> "sell" in
  let color =
    match side_str with
    | "buy"  -> 0x2ECC71  (* green *)
    | "sell" -> 0xE74C3C  (* red *)
    | _      -> 0x3498DB  (* blue *)
  in
  let order_url = Printf.sprintf "https://pro.kraken.com/app/trade/%s" payload.symbol in
  `Assoc [
    ("username", `String "Dio");
    ("content", `String (Printf.sprintf "Kraken"));
    ("embeds", `List [
       `Assoc [
         ("title", `String (Printf.sprintf "%s %s" (String.uppercase_ascii side_str) payload.asset_name));
         ("description", `String "Order filled successfully");
         ("color", `Int color);
         ("fields", `List [
            `Assoc [("name", `String "Quantity"); ("value", `String payload.qty_str); ("inline", `Bool true)];
            `Assoc [("name", `String "Value"); ("value", `String payload.value_str); ("inline", `Bool true)]
          ]);
         ("footer", `Assoc [("text", `String "Diophant Solutions Trading Engine")]);
         ("timestamp", `String (Ptime.to_rfc3339 (Ptime_clock.now ())))
       ]
     ]);
    ("components", `List [
       `Assoc [
         ("type", `Int 1);
         ("components", `List [
           `Assoc [
             ("type", `Int 2);
             ("style", `Int 5);
             ("label", `String "View Order");
             ("url", `String order_url)
           ]
         ])
       ]
     ])
  ]

let send_message (payload: Notification.fill_notification_payload) =
  Ringbuffer.push message_queue payload

let get_message_queue_for_test () = message_queue

let rec worker () =
  Ringbuffer.pop message_queue >>= fun payload ->
  (match Sys.getenv_opt "DISCORD_WEBHOOK_URL" with
  | None -> 
      if not !logged_no_url then (
        logged_no_url := true;
        warning ~section "DISCORD_WEBHOOK_URL not set, not sending notification."
      ) else Lwt.return_unit
  | Some webhook_url ->
    Lwt.catch 
      (fun () ->
        let uri = Uri.of_string webhook_url in
        let headers = Header.init () |> fun h -> Header.add h "Content-Type" "application/json" in
        let json = make_order_embed payload in
        let body_str = Yojson.Safe.to_string json in
        let body = Cohttp_lwt.Body.of_string body_str in
        Client.post ~headers ~body uri >>= fun (resp, body) ->
        let status = Response.status resp in
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        if Code.is_success (Code.code_of_status status) then
          info_f ~section "Discord notification sent for order %s" payload.order_id
        else
          error_f ~section "Failed to send Discord notification: %s - %s" (Code.string_of_status status) body_str
      )
      (fun exn ->
        error_f ~section "Exception sending Discord notification: %s" (Printexc.to_string exn)
      )) >>= fun () ->
  worker ()

let start () =
  worker ()
