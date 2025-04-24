(* src/exchange/kraken/token.ml *)
open Lwt.Infix
open Cohttp_lwt_unix
open Yojson.Safe
open Common

let endpoint = "https://api.kraken.com"

let load_env_file () =
  try
    let () = Dotenv.export () in
    ()
  with Sys_error msg -> (* Catch standard file system errors *) 
    failwith (Printf.sprintf "Error loading .env file: %s" msg)

let get_api_credentials_from_env () =
  let () = load_env_file () in
  let get_env_var var_name =
    match Stdlib.Sys.getenv_opt var_name with
    | Some value -> value
    | None -> failwith (Printf.sprintf "Missing environment variable: %s" var_name)
  in
  let api_key = get_env_var "KRAKEN_API_KEY" in
  let api_secret = get_env_var "KRAKEN_API_SECRET" in
  (api_key, api_secret)

let get_token () =
  let api_key, api_secret = get_api_credentials_from_env () in
  let path = "/0/private/GetWebSocketsToken" in
  let nonce = nonce () in
  let body_str = "nonce=" ^ nonce in (* Form-encoded body *) 
  let signature = sign ~secret:api_secret ~path ~body:body_str ~nonce in (* Sign form body *) 
  let headers = Cohttp.Header.add_list (Cohttp.Header.init ()) [
    ("API-Key", api_key);
    ("API-Sign", signature);
    ("Content-Type", "application/x-www-form-urlencoded"); (* Set correct Content-Type *) 
  ] in
  let request_body = Cohttp_lwt.Body.of_string body_str in (* Create Cohttp body *) 
  Client.post ~headers ~body:request_body (Uri.of_string (endpoint ^ path)) >>= fun (resp, resp_body) -> 
  
  (* Check HTTP status code first *) 
  let status_code = Cohttp.Response.status resp |> Cohttp.Code.code_of_status in
  if status_code <> 200 then (
    resp_body |> Cohttp_lwt.Body.to_string >>= fun _body_str -> (* Read body but don't print *) 
    Printf.eprintf "Kraken API HTTP Error %d\n%!" status_code;
    Lwt.fail_with (Printf.sprintf "Kraken API request failed with HTTP status %d" status_code)
  ) else (
    (* Status is 200, proceed to parse body *) 
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
        Printf.eprintf "Kraken API Error Response (JSON): %s\n%!" error_msg; (* Print parsed error, not full body *) 
        Lwt.fail_with ("Kraken API error: " ^ error_msg)
    | `Null | `List [] -> (* No error or empty error list, proceed *) 
        (match Util.member "result" json with 
         | `Null -> 
            Printf.eprintf "Kraken API Unexpected Response (no result field)\n%!"; 
            Lwt.fail_with "Kraken API error: Missing 'result' field in response"
         | result_json -> 
            match Util.member "token" result_json with
            | `String token ->
                Lwt.return token
            | _ ->
                Printf.eprintf "Kraken API Unexpected Token Format (token field is not a string)\n%!"; 
                Lwt.fail_with "Kraken API error: Token field is not a string"
        )
    | other_error -> (* Unexpected error format *) 
        Printf.eprintf "Kraken API Unexpected Error Format: %s\n%!" (to_string other_error); (* Print parsed error field, not full body *) 
        Lwt.fail_with ("Kraken API error: Unexpected format in 'error' field: " ^ (to_string other_error))
  )