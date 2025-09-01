(* src/exchange/kraken/kraken_generate_auth_token.ml *)
open Lwt.Infix
open Cohttp_lwt_unix
open Yojson.Safe
open Lwt_log_core (* Added Lwt_log_core *)

let endpoint = "https://api.kraken.com"

let section = Section.make "kraken.auth_token" (* Defined section *)

let load_env_file () : unit Lwt.t =
  Lwt.catch
    (fun () ->
       Dotenv.export ~path:".env" ();
       Lwt.return_unit)
    (fun exn ->
       warning_f ~section "Error loading .env file: %s" (Printexc.to_string exn) >>= fun () ->
       Lwt.fail exn)

let get_api_credentials_from_env () : (string * string) Lwt.t =
  load_env_file () >>= fun () ->
  let get_env_var var_name =
    match Sys.getenv_opt var_name with
    | Some value -> Lwt.return value
    | None ->
        error_f ~section "Missing environment variable: %s" var_name >>= fun () ->
        Lwt.fail (Failure (Printf.sprintf "Missing environment variable: %s" var_name))
  in
  get_env_var "KRAKEN_API_KEY" >>= fun api_key ->
  get_env_var "KRAKEN_API_SECRET" >>= fun api_secret ->
  Lwt.return (api_key, api_secret)

let get_token () : string Lwt.t =
  get_api_credentials_from_env () >>= fun (api_key, api_secret) ->
  let path = "/0/private/GetWebSocketsToken" in
  Kraken_common_types.nonce () >>= fun nonce ->
  let body_str = "nonce=" ^ nonce in
  let signature = Kraken_common_types.sign ~secret:api_secret ~path ~body:body_str ~nonce in
  let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) [
    ("API-Key", api_key);
    ("API-Sign", signature);
    ("Content-Type", "application/x-www-form-urlencoded");
  ] in
  let request_body = Cohttp_lwt.Body.of_string body_str in
  Client.post ~headers ~body:request_body (Uri.of_string (endpoint ^ path)) >>= fun (resp, resp_body) ->

  (* Check HTTP status code first *)
  let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  if status_code <> 200 then (
    resp_body |> Cohttp_lwt.Body.to_string >>= fun body_str ->
    error_f ~section "Kraken API HTTP Error %d: %s" status_code body_str >>= fun () ->
    Lwt.fail_with (Printf.sprintf "Kraken API request failed with HTTP status %d" status_code)
  ) else (
    resp_body |> Cohttp_lwt.Body.to_string >>= fun body_str ->
    let json = from_string body_str in

    (* Check for API errors in JSON before accessing result *)
    match Util.member "error" json with
    | `List (_ :: _ as errors) -> (* Check if error list is non-empty *)
        let error_msg =
          errors
          |> List.map to_string
          |> String.concat "; "
        in
        error_f ~section "Kraken API Error Response (JSON): %s" error_msg >>= fun () ->
        Lwt.fail_with ("Kraken API error: " ^ error_msg)
    | `Null | `List [] -> (* No error or empty error list, proceed *)
        (match Util.member "result" json with
         | `Null ->
            error_f ~section "Kraken API Unexpected Response (no result field)" >>= fun () ->
            Lwt.fail_with "Kraken API error: Missing 'result' field in response"
         | result_json ->
            match Util.member "token" result_json with
            | `String token ->
                Lwt.return token
            | _ ->
                error_f ~section "Kraken API Unexpected Token Format (token field is not a string)" >>= fun () ->
                Lwt.fail_with "Kraken API error: Token field is not a string"
        )
    | other_error -> (* Unexpected error format *)
        error_f ~section "Kraken API Unexpected Error Format: %s" (to_string other_error) >>= fun () ->
        Lwt.fail_with ("Kraken API error: Unexpected format in 'error' field: " ^ (to_string other_error))
  )