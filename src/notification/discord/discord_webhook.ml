open Lwt.Infix
open Cohttp
open Cohttp_lwt_unix
open Lwt_log_core

let section = Section.make "notification.discord"

let logged_no_url = ref false

let send_message message =
  match Sys.getenv_opt "DISCORD_WEBHOOK_URL" with
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
        let json = `Assoc [("content", `String message)] in
        let body_str = Yojson.Safe.to_string json in
        let body = Cohttp_lwt.Body.of_string body_str in
        Client.post ~headers ~body uri >>= fun (resp, body) ->
        let status = Response.status resp in
        Cohttp_lwt.Body.to_string body >>= fun body_str ->
        if Code.is_success (Code.code_of_status status) then
          info_f ~section "Discord notification sent: %s" message
        else
          error_f ~section "Failed to send Discord notification: %s - %s" (Code.string_of_status status) body_str
      )
      (fun exn ->
        error_f ~section "Exception sending Discord notification: %s" (Printexc.to_string exn)
      )
