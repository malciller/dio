open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Lwt_log_core
open Dio_types
open Dio_types.Core

let get_current_time () =
  Ptime.to_rfc3339 (Ptime_clock.now ())

type fill_notification_payload = {
  side: side;
  asset_name: string;
  qty_str: string;
  value_str: string;
  order_id: string;
  symbol: string;
}

type balance_notification_payload = {
  balances: (string * float) list;
}

type notification_payload =
  | Fill of fill_notification_payload
  | Balance of balance_notification_payload

let section = Section.make "notification.discord"
let message_queue : notification_payload Ringbuffer.t = Ringbuffer.create 100
let logged_no_url = ref false

let make_order_embed (payload: fill_notification_payload) =
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
    ("content", `String (Printf.sprintf "Kraken : %s : %s" (String.uppercase_ascii side_str) (String.uppercase_ascii payload.asset_name)));
    ("embeds", `List [
       `Assoc [
         ("title", `String (Printf.sprintf "%s %s" (String.uppercase_ascii side_str) payload.asset_name));
         ("description", `String "Order filled successfully");
         ("color", `Int color);
         ("fields", `List [
            `Assoc [("name", `String "Quantity"); ("value", `String payload.qty_str); ("inline", `Bool true)];
            `Assoc [("name", `String "Value"); ("value", `String payload.value_str); ("inline", `Bool true)]
          ]);
         ("footer", `Assoc [("text", `String "Dio Trading Engine")]);
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

let make_balance_embed (payload: balance_notification_payload) =
  (* Filter out zero or very small balances and exclude stock assets *)
  let excluded_assets = ["QQQ.EQ"; "SCHD.EQ"; "VTI.EQ"] in
  let significant_balances = payload.balances
    |> List.filter (fun (asset, balance) ->
         balance >= 0.00001 && not (List.mem asset excluded_assets)
       ) in

  if significant_balances = [] then
    (* If no significant balances, send a simple message *)
    `Assoc [
      ("username", `String "Dio");
      ("content", `String "Kraken : Balances");
      ("embeds", `List [
         `Assoc [
           ("title", `String "Exchange Balance");
           ("description", `String "No significant balances found");
           ("color", `Int 0x3498DB);
           ("footer", `Assoc [("text", `String "Dio Trading Engine")]);
           ("timestamp", `String (Ptime.to_rfc3339 (Ptime_clock.now ())))
         ]
       ])
    ]
  else
    let fields = List.map (fun (asset, balance) ->
      let value_str =
        if balance < 0.00001 then "< 0.00001"
        else if balance < 0.01 then Printf.sprintf "%.6f" balance
        else Printf.sprintf "%.2f" balance in
      `Assoc [("name", `String asset); ("value", `String value_str); ("inline", `Bool true)]
    ) significant_balances in

    `Assoc [
      ("username", `String "Dio");
      ("content", `String "Kraken : Balances");
      ("embeds", `List [
         `Assoc [
           ("title", `String "Exchange Balance");
           ("description", `String "Current portfolio balances across all assets");
           ("color", `Int 0x3498DB);  (* blue *)
           ("fields", `List fields);
           ("footer", `Assoc [("text", `String "Dio Trading Engine")]);
           ("timestamp", `String (Ptime.to_rfc3339 (Ptime_clock.now ())))
         ]
       ])
    ]

let make_message_json (payload: notification_payload) =
  match payload with
  | Fill fill_payload -> make_order_embed fill_payload
  | Balance balance_payload -> make_balance_embed balance_payload

let send_message (payload: notification_payload) =
  Ringbuffer.push message_queue payload

let get_message_queue_for_test () = message_queue

let send_balance_notification (balance_fetcher: Config.engine_config -> (string, float) Hashtbl.t Lwt.t) (cfg: Config.engine_config) =
  match Sys.getenv_opt "DISCORD_WEBHOOK_URL" with
  | None ->
      if not !logged_no_url then (
        logged_no_url := true;
        warning ~section "DISCORD_WEBHOOK_URL not set, skipping balance notification."
      ) else Lwt.return_unit
  | Some _ ->
    Lwt.catch
      (fun () ->
        balance_fetcher cfg >>= fun balances ->
        let balance_list = Hashtbl.fold (fun asset balance acc -> (asset, balance) :: acc) balances [] in
        let sorted_balances = List.sort (fun (a1, _) (a2, _) -> String.compare a1 a2) balance_list in
        (* Debug: Log what balances we're trying to send *)
        let balance_str = String.concat ", " (List.map (fun (asset, balance) -> Printf.sprintf "%s: %.6f" asset balance) sorted_balances) in
        info_f ~section "Sending balances: %s" balance_str >>= fun () ->
        let payload = Balance { balances = sorted_balances } in
        send_message payload >>= fun () ->
        info_f ~section "Balance notification queued for Discord"
      )
      (fun exn ->
        error_f ~section "Failed to send balance notification: %s" (Printexc.to_string exn)
      )

let balance_scheduler (balance_fetcher: Config.engine_config -> (string, float) Hashtbl.t Lwt.t) (cfg: Config.engine_config) =
  (* Calculate seconds until next quarter hour (00, 15, 30, 45 minutes) *)
  let now = Unix.gettimeofday () in
  let tm = Unix.localtime now in
  let current_minute = tm.Unix.tm_min in
  let current_second = tm.Unix.tm_sec in

  (* Find next quarter hour *)
  let next_quarter_minute =
    if current_minute < 15 then 15
    else if current_minute < 30 then 30
    else if current_minute < 45 then 45
    else 60  (* Next hour's :00 *)
  in

  let seconds_until_next_quarter =
    if next_quarter_minute = 60 then
      (* Next hour's :00 *)
      (60 - current_minute) * 60 - current_second
    else
      (* This hour's quarter hour *)
      (next_quarter_minute - current_minute) * 60 - current_second
  in

  let initial_wait = float_of_int seconds_until_next_quarter in

  info_f ~section "Next balance notification in %.0f seconds (at quarter hour)" initial_wait >>= fun () ->

  (* Sleep until next quarter hour, then send notification *)
  Lwt_unix.sleep initial_wait >>= fun () ->
  send_balance_notification balance_fetcher cfg >>= fun () ->

  (* Then schedule every 15 minutes thereafter *)
  let rec schedule_next () =
    let fifteen_minutes = 15.0 *. 60.0 in
    Lwt_unix.sleep fifteen_minutes >>= fun () ->
    send_balance_notification balance_fetcher cfg >>= fun () ->
    schedule_next ()
  in
  schedule_next ()

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
        let json = make_message_json payload in
        let body_str = Yojson.Safe.to_string json in
        let body = Cohttp_lwt.Body.of_string body_str in
        Client.post ~headers ~body uri >>= fun (resp, body) ->
        let status = Response.status resp in
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        if Code.is_success (Code.code_of_status status) then
          let log_info = match payload with
            | Fill p -> Printf.sprintf "for order %s" p.order_id
            | Balance _ -> "for balance update" in
          info_f ~section "Discord notification sent %s" log_info >>= fun () ->
          Lwt.return_unit
        else
          error_f ~section "Failed to send Discord notification: %s - %s" (Code.string_of_status status) body_str >>= fun () ->
          Lwt.return_unit
      )
      (fun exn ->
        error_f ~section "Exception sending Discord notification: %s" (Printexc.to_string exn) >>= fun () ->
        Lwt.return_unit
      )) >>= fun () ->
  worker ()

let start (balance_fetcher: Config.engine_config -> (string, float) Hashtbl.t Lwt.t) (cfg: Config.engine_config) =
  Lwt.join [
    worker ();
    balance_scheduler balance_fetcher cfg
  ]
