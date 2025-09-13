(* src/exchange/kraken/kraken_api.ml *)

open Lwt.Infix
open Cohttp_lwt_unix
open Yojson.Safe
open Lwt_log_core
open Dio_types

let section = Section.make "kraken.balances"

let endpoint = "https://api.kraken.com"

let make_request (cfg : Config.engine_config) ~path ~params =
  Kraken_common_types.nonce () >>= fun nonce ->
  let params_with_nonce = ("nonce", nonce) :: params in
  let body_str = Uri.encoded_of_query (List.map (fun (k, v) -> (k, [v])) params_with_nonce) in
  let signature = Kraken_common_types.sign ~secret:cfg.kraken_api_secret ~path ~body:body_str ~nonce in
  let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) [
    ("API-Key", cfg.kraken_api_key);
    ("API-Sign", signature);
    ("Content-Type", "application/x-www-form-urlencoded");
  ] in
  let request_body = Cohttp_lwt.Body.of_string body_str in
  Client.post ~headers ~body:request_body (Uri.of_string (endpoint ^ path)) >>= fun (resp, resp_body) ->
  let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  resp_body |> Cohttp_lwt.Body.to_string >>= fun body_str ->
  if status_code <> 200 then (
    error_f ~section "Kraken API HTTP Error %d: %s" status_code body_str >>= fun () ->
    Lwt.fail_with (Printf.sprintf "Kraken API request failed with HTTP status %d" status_code)
  ) else (
    Lwt.return (from_string body_str)
  )

let get_account_balance (cfg : Config.engine_config) : (string, float) Hashtbl.t Lwt.t =
  let path = "/0/private/Balance" in
  make_request cfg ~path ~params:[] >>= fun json ->
  let open Yojson.Safe.Util in
  let result = json |> member "result" in
  let balances = Hashtbl.create 16 in
  if result <> `Null then (
    result |> to_assoc |> List.iter (fun (asset, balance_json) ->
      let balance = balance_json |> to_string |> float_of_string in
      Hashtbl.add balances asset balance
    )
  );
  Lwt.return balances
