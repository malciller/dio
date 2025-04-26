open Lwt.Infix
open Types.Core
open Types.Config

(* Set up logging *)
let setup_logging () =
  Lwt_log.add_rule "*" Lwt_log_core.Error; (* Default to Error level *)
  Lwt_log.add_rule "engine.strategy" Lwt_log_core.Info; (* Allow Info for strategy *) 
  Lwt_log.add_rule "engine.router" Lwt_log_core.Info;   (* Allow Info for router *) 
  Lwt_log.add_rule "kraken_ws_feed" Lwt_log_core.Debug; (* Allow Debug for ws_feed *) 
  Lwt_log_core.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

(* Read and parse config file *)
let read_config config_path =
  try
    let json = Yojson.Safe.from_file config_path in
    let runtime_cfg = runtime_cfg_of_yojson_exn json in
    (* Convert runtime_cfg to Core.config *)
    let symbols = List.map (fun asset -> asset.symbol) runtime_cfg.assets in
    Ok {
      ws_host = "ws.kraken.com";
      ws_port = 443;
      ws_path = "/v2";
      symbols;
      auth_token = None; (* Will be set later from .env *)
    }
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "Invalid JSON in config file: %s" msg)
  | Sys_error msg -> Error (Printf.sprintf "Failed to read config file: %s" msg)
  | exn -> Error (Printf.sprintf "Unexpected error reading config: %s" (Printexc.to_string exn))

let main () =
  (* Initialize Logging FIRST *)
  setup_logging ();

  (* Initialize the default RNG for crypto operations (TLS) using the Unix backend *)
  Mirage_crypto_rng_unix.use_default ();

  (* Load .env file to get the API token *)
  (try Dotenv.export ~path:"src/exchange/kraken/.env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!");

  (* Start the engine with all components *)
  Lwt_main.run (
    Lwt.catch
      (fun () ->
        (* Read config file *)
        match read_config "config.json" with
        | Error msg ->
            Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.config") msg >>= fun () ->
            Lwt.fail_with msg
        | Ok cfg ->
            (* Retrieve Auth Token using Kraken.Token.get_token, handle potential errors *)
            (Lwt.catch 
              (fun () -> Kraken.Token.get_token () >>= fun token -> Lwt.return_some token)
              (fun exn -> 
                Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.auth") 
                  (Printf.sprintf "Failed to retrieve auth token: %s" (Printexc.to_string exn)) >>= fun () ->
                Lwt.return_none
              )
            ) >>= fun auth_token_opt ->

            (* Update config with auth token *)
            let cfg = { cfg with auth_token = auth_token_opt } in
            (* Removed non-error related log *)
            Lwt.return_unit >>= fun () ->

            (* Create strategy and router modules *)
            let strategy : Types.Core.strategy = {
              (* Signature: start: config -> tick_buffer -> cmd_buffer -> exec_buffer -> unit Lwt.t *)
              start = Strategy.start (* Use Strategy.start directly *)
            } in
            let router : Types.Core.router = {
              (* Signature: start: config -> cmd_buffer -> exec_buffer -> unit Lwt.t *)
              start = Router.start (* Use Router.start directly *)
            } in

            (* Start the engine with all components *)
            Engine.run ~strategy ~router cfg (* No callbacks needed *)
      )
      (fun exn ->
        (* Log errors from starting the engine *)
        Printf.eprintf "Error in engine: %s\n%!" (Printexc.to_string exn);
        Lwt.return_unit
      )
  )

let () = main ()
