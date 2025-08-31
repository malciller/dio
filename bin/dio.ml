open Lwt.Infix
open Conduit_lwt_unix
open Dio_types
open Dashboard 
open Engine

let mode_dash = ref false

(* Set up logging *)
let setup_logging () =
  (* These base rules apply to all logs before specific section rules are checked.
     They will direct to whichever logger becomes the default. *)
  Lwt_log.add_rule "*" Lwt_log_core.Error;
  Lwt_log.add_rule "*" Lwt_log_core.Warning; 

  let default_logger =
    if !mode_dash then
      (* Dashboard mode: logs with timestamps *)
      Lwt_log_core.make
        ~output:(fun section level messages ->
          if List.length messages > 0 then
            let first_message_string = List.hd messages in
            let timestamp = 
              let tm = Unix.localtime (Unix.time ()) in
              Printf.sprintf "%02d:%02d:%02d" 
                tm.Unix.tm_hour 
                tm.Unix.tm_min 
                tm.Unix.tm_sec
            in
            let formatted_message = 
              Printf.sprintf "[%s][%s|%s] %s" 
                timestamp
                (Lwt_log_core.Section.name section) 
                (Lwt_log_core.string_of_level level) 
                first_message_string
            in
            Stats.add_dashboard_log formatted_message;
            Lwt.return_unit
          else
            Lwt.return_unit
        )
        ~close:(fun () -> Lwt.return_unit)
    else
      (* Normal mode: logs go to stdout *)
      Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()
  in

  Lwt_log_core.default := default_logger;

  (* Specific rules for log levels and sections.*) 
  Lwt_log.add_rule "engine.*" Lwt_log_core.Info;
  Lwt_log.add_rule "engine.strategy.orderbook" Lwt_log_core.Info;
  ()

let read_config config_path : (Config.runtime_cfg * Config.engine_config, string) result = (* Return both configs *)
  try
    let json = Yojson.Safe.from_file config_path in
    let runtime_cfg : Config.runtime_cfg = Config.runtime_cfg_of_yojson_exn json in
    
    (* --- Begin Validation --- *)
    match Config.validate_runtime_cfg runtime_cfg with
    | Error msg -> Error (Printf.sprintf "Configuration validation failed:\n%s" msg)
    | Ok () ->

        let grid_symbols = List.filter_map (fun (asset: Config.asset_cfg) -> 
          match asset.strategy with 
          | Config.Grid -> Some asset.symbol
          | Config.Orderbook -> None
        ) runtime_cfg.assets in
        
        let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) -> 
          match asset.strategy with 
          | Config.Orderbook -> Some asset.symbol
          | Config.Grid -> None
        ) runtime_cfg.assets in
        
        let all_symbols = List.map (fun (asset: Config.asset_cfg) -> asset.symbol) runtime_cfg.assets in
        
        Printf.eprintf "[CONFIG] Grid strategy symbols: [%s]\n%!" (String.concat ", " grid_symbols);
        Printf.eprintf "[CONFIG] Orderbook strategy symbols: [%s]\n%!" (String.concat ", " orderbook_symbols);
        
        let api_key = 
          match Sys.getenv_opt "KRAKEN_API_KEY" with 
          | Some key -> key 
          | None -> 
              Printf.eprintf "FATAL: KRAKEN_API_KEY environment variable not set.\n%!"; 
              exit 1
        in
        let api_secret = 
          match Sys.getenv_opt "KRAKEN_API_SECRET" with
          | Some secret -> secret
          | None -> 
              Printf.eprintf "FATAL: KRAKEN_API_SECRET environment variable not set.\n%!";
              exit 1
        in

        let engine_cfg : Config.engine_config = {
          ws_host = "ws.kraken.com";
          ws_port = 443;
          ws_path = "/v2";
          symbols = all_symbols;  
          auth_token = None; 
          kraken_api_key = api_key;
          kraken_api_secret = api_secret;
        } in
        Ok (runtime_cfg, engine_cfg)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "Invalid JSON in config file: %s" msg)
  | Sys_error msg -> Error (Printf.sprintf "Failed to read config file: %s" msg)
  | exn -> Error (Printf.sprintf "Unexpected error reading config: %s" (Printexc.to_string exn))

(* Placeholder for any other existing command line arguments application might have later *)
let your_other_args = []

let specs = [
  ("--dashboard", Arg.Set mode_dash, " Run Dashboard")
] @ your_other_args

let start_engine_logic () : unit Lwt.t = 
  init () >>= fun _ctx ->
  Lwt.catch
    (fun () ->
      match read_config "_config.json" with
      | Error msg ->
          Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.config") msg >>= fun () ->
          Lwt.fail_with msg
      | Ok (runtime_cfg, core_cfg) ->
          (Lwt.catch 
            (fun () -> Kraken.Kraken_generate_auth_token.get_token () >>= fun token -> Lwt.return_some token)
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

          let core_cfg = { core_cfg with auth_token = auth_token_opt } in
          Lwt.return_unit >>= fun () ->

          let grid_strategy : Core.grid_strategy = { 
            start = Kraken_suicide_grid.start 
          } in
          let orderbook_strategy : Core.orderbook_strategy = {
            start = Kraken_top_level_mm.start
          } in
          let router : Core.router = {
            start = Router.start 
          } in
          Engine.run ~grid_strategy ~orderbook_strategy ~router runtime_cfg core_cfg
    )
    (fun exn ->
      Printf.eprintf "Error in engine: %s\n%!" (Printexc.to_string exn);
      Lwt.return_unit
    )

let main () =
  Mirage_crypto_rng_unix.use_default ();

  (try Dotenv.export ~path:".env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!"); 
  let key_check = match Sys.getenv_opt "KRAKEN_API_KEY" with Some _ -> "FOUND" | None -> "MISSING" in
  let secret_check = match Sys.getenv_opt "KRAKEN_API_SECRET" with Some _ -> "FOUND" | None -> "MISSING" in
  Printf.eprintf "[DEBUG] Post Dotenv.export: KRAKEN_API_KEY status: %s, KRAKEN_API_SECRET status: %s\n%!" key_check secret_check;

  Arg.parse specs (fun anon_arg -> Printf.eprintf "Warning: Ignoring anonymous argument: %s\n" anon_arg) "dio options";

  setup_logging ();

  if !mode_dash then begin
    let quit_promise, resolve_quit = Lwt.wait () in

    let cleanup_initiated = ref false in

    let term_instance_ref = ref None in
    let final_engine_promise_ref = ref (Lwt.return_unit) in 

    let dashboard_on_quit () =
      if not !cleanup_initiated then (
        Printf.eprintf "\\n[!] Dashboard requested exit. Signaling main loop...\\n%!";
        Lwt.wakeup_later resolve_quit ();
      );
      Lwt.return_unit
    in

    let term = Dashboard.start ~on_quit:dashboard_on_quit () in
    term_instance_ref := Some term;

    final_engine_promise_ref := start_engine_logic (); 
    Lwt.async (fun () -> 
      Lwt.catch (fun () -> !final_engine_promise_ref) (fun ex -> 
        Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.main") 
          (Printf.sprintf "Engine task failed: %s" (Printexc.to_string ex))
        >>= fun () -> 
          if not !cleanup_initiated then Lwt.wakeup_later resolve_quit (); 
          Lwt.return_unit
      ) 
    );

    let _sighandler_id = Lwt_unix.on_signal Sys.sigint (fun _signum ->
        if not !cleanup_initiated then (
          Printf.eprintf "\\n[!] SIGINT received, initiating exit...\\n%!";
          Lwt.wakeup_later resolve_quit (); 
        )
      )
    in

    Lwt_main.run (
      quit_promise >>= fun () ->
      if not !cleanup_initiated then (
        cleanup_initiated := true; 
        Printf.eprintf "\\n[!] Main loop exiting, performing final cleanup...\\n%!";
        Lwt.cancel !final_engine_promise_ref; 
        (match !term_instance_ref with
        | Some ti -> Notty_lwt.Term.release ti
        | None -> Lwt.return_unit)
        >>= fun () -> 
          Printf.eprintf "[!] Cleanup complete. Exiting application.\n%!";
          Lwt.return_unit
      ) else (
        Printf.eprintf "[!] Cleanup already handled or in progress. Exiting application.\n%!";
        Lwt.return_unit
      )
    );

  end else

    Lwt_main.run (start_engine_logic ())

let () = main ()
