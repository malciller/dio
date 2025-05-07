open Lwt.Infix
open Conduit_lwt_unix
open Types

(* Set up logging *)
let setup_logging () =
  Lwt_log.add_rule "*" Lwt_log_core.Error;
  Lwt_log.add_rule "*" Lwt_log_core.Warning; 
  Lwt_log.add_rule "engine.router" Lwt_log_core.Info;   
  Lwt_log.add_rule "engine.router" Lwt_log_core.Debug;   
  Lwt_log.add_rule "kraken_ws_exec" Lwt_log_core.Info;   
  Lwt_log.add_rule "kraken_ws_exec" Lwt_log_core.Debug;
  Lwt_log.add_rule "engine.strategy" Lwt_log_core.Info; 
  Lwt_log.add_rule "engine.supervisor" Lwt_log_core.Info;        
  (* Allow Info for router *) 
  Lwt_log_core.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

(* Read and parse config file *)
let read_config config_path : (Config.runtime_cfg * Core.config, string) result = (* Return both configs *)
  try
    let json = Yojson.Safe.from_file config_path in
    let runtime_cfg = Config.runtime_cfg_of_yojson_exn json in
    (* Convert runtime_cfg to Core.config *)
    let symbols = List.map (fun asset -> asset.Config.symbol) runtime_cfg.assets in
    let core_cfg = { (* Rename to core_cfg for clarity *)
      Core.ws_host = "ws.kraken.com";
      Core.ws_port = 443;
      Core.ws_path = "/v2";
      Core.symbols;
      Core.auth_token = None; (* Will be set later from .env *)
    } in
    Ok (runtime_cfg, core_cfg) (* Return tuple *)
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
  (try Dotenv.export ~path:".env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!"); 
  let key_check = match Sys.getenv_opt "KRAKEN_API_KEY" with Some _ -> "FOUND" | None -> "MISSING" in
  let secret_check = match Sys.getenv_opt "KRAKEN_API_SECRET" with Some _ -> "FOUND" | None -> "MISSING" in
  Printf.eprintf "[DEBUG] Post Dotenv.export: KRAKEN_API_KEY status: %s, KRAKEN_API_SECRET status: %s\n%!" key_check secret_check;
  (* =========================== *)

  (* Start the engine with all components *)
  Lwt_main.run (
    init () >>= fun _ctx ->
    Lwt.catch
      (fun () ->
        (* Read config file *)
        match read_config "kraken_grid_config.json" with
        | Error msg ->
            Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.config") msg >>= fun () ->
            Lwt.fail_with msg
        | Ok (runtime_cfg, core_cfg) -> (* Destructure the tuple *)
            (* Retrieve Auth Token using Kraken.Token.get_token, handle potential errors *)
            (Lwt.catch 
              (fun () -> Kraken.Token.get_token () >>= fun token -> Lwt.return_some token)
              (fun exn -> 
                let backtrace = Printexc.get_backtrace () in 
                Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.auth") 
                  (Printf.sprintf "Failed to retrieve auth token: %s\nBacktrace:\n%s" 
                     (Printexc.to_string exn) 
                     backtrace) 
                >>= fun () ->
                Lwt.return_none
              )
            ) >>= fun auth_token_opt ->

            (* Update core_cfg with auth token *)
            let core_cfg = { core_cfg with auth_token = auth_token_opt } in
            Lwt.return_unit >>= fun () ->

            (* Create strategy and router modules *)
            let grid_strategy : Types.Core.grid_strategy = { 
              start = Grid_strategy.start 
            } in
            let router : Types.Core.router = {
              start = Router.start 
            } in


            Engine.run ~grid_strategy ~router runtime_cfg core_cfg 
      )
      (fun exn ->
        (* Log errors from starting the engine *)
        Printf.eprintf "Error in engine: %s\n%!" (Printexc.to_string exn);
        Lwt.return_unit
      )
  )

let () = main ()
