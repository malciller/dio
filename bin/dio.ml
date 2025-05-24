open Lwt.Infix
open Conduit_lwt_unix
open Dio_types
open Dashboard (* For Dashboard and Stats, and potentially Engine if it's moved into Dio lib *)
open Engine

(* Move this declaration to the top *)
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

  (* Specific rules for log levels and sections.
     These will now use the 'default_logger' configured above *) 
  Lwt_log.add_rule "engine.*" Lwt_log_core.Info;
  Lwt_log.add_rule "kraken_ws_exec" Lwt_log_core.Info;
  Lwt_log.add_rule "database.price_logger" Lwt_log_core.Info

(* Read and parse config file *)
let read_config config_path : (Config.runtime_cfg * Config.engine_config, string) result = (* Return both configs *)
  try
    let json = Yojson.Safe.from_file config_path in
    let runtime_cfg : Config.runtime_cfg = Config.runtime_cfg_of_yojson_exn json in
    
    (* Extract symbols for engine_config *)
    (* asset is of type Config.asset_cfg, which has a 'symbol: Primitives.symbol' field. Primitives.symbol is string *)
    let engine_symbols : string list = List.map (fun (asset: Config.asset_cfg) -> asset.symbol) runtime_cfg.assets in
    
    (* Retrieve API key and secret from environment variables *)
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

    (* Get DB_URI from environment variable, with a default for convenience *)
    let db_uri = 
      match Sys.getenv_opt "DB_URI" with
      | Some uri -> uri
      | None -> "sqlite3:./dio_prices.db" (* Default SQLite URI if not set *)
    in

    let engine_cfg : Config.engine_config = {
      ws_host = "ws.kraken.com";
      ws_port = 443;
      ws_path = "/v2";
      symbols = engine_symbols;
      auth_token = None; (* Will be set later from Exchange.Kraken.Token *)
      kraken_api_key = api_key;
      kraken_api_secret = api_secret;
      db_uri = db_uri;  (* Changed from db_path to db_uri *)
    } in
    Ok (runtime_cfg, engine_cfg)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "Invalid JSON in config file: %s" msg)
  | Sys_error msg -> Error (Printf.sprintf "Failed to read config file: %s" msg)
  | exn -> Error (Printf.sprintf "Unexpected error reading config: %s" (Printexc.to_string exn))

(* Placeholder for any other existing command line arguments application might have *)
let your_other_args = []

let specs = [
  ("--dashboard", Arg.Set mode_dash, " Run Dashboard")
] @ your_other_args

let start_engine_logic () : unit Lwt.t = 
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
          let grid_strategy : Core.grid_strategy = { 
            start = Grid_strategy.start 
          } in
          let router : Core.router = {
            start = Router.start 
          } in
          Engine.run ~grid_strategy ~router runtime_cfg core_cfg 
    )
    (fun exn ->
      (* Log errors from starting the engine *)
      Printf.eprintf "Error in engine: %s\n%!" (Printexc.to_string exn);
      Lwt.return_unit
    )

let main () =
  (* Initialize the default RNG for crypto operations (TLS) using the Unix backend *)
  Mirage_crypto_rng_unix.use_default ();

  (* Load .env file to get the API token *)
  (try Dotenv.export ~path:".env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!"); 
  let key_check = match Sys.getenv_opt "KRAKEN_API_KEY" with Some _ -> "FOUND" | None -> "MISSING" in
  let secret_check = match Sys.getenv_opt "KRAKEN_API_SECRET" with Some _ -> "FOUND" | None -> "MISSING" in
  Printf.eprintf "[DEBUG] Post Dotenv.export: KRAKEN_API_KEY status: %s, KRAKEN_API_SECRET status: %s\n%!" key_check secret_check;
  (* =========================== *)

  Arg.parse specs (fun anon_arg -> Printf.eprintf "Warning: Ignoring anonymous argument: %s\n" anon_arg) "dio options";

  (* Initialize Logging AFTER Arg.parse so mode_dash is set *)
  setup_logging ();

  if !mode_dash then begin
    let quit_promise, resolve_quit = Lwt.wait () in

    let cleanup_initiated = ref false in

    let term_instance_ref = ref None in
    let final_engine_promise_ref = ref (Lwt.return_unit) in 

    let dashboard_on_quit () =
      if not !cleanup_initiated then (
        Printf.eprintf "\\n[!] Dashboard requested exit. Signaling main loop...\\n%!";
        Lwt.wakeup_later resolve_quit (); (* Signal the main loop to start cleanup *)
      );
      Lwt.return_unit
    in

    let term = Dashboard.start ~on_quit:dashboard_on_quit () in
    term_instance_ref := Some term;

    final_engine_promise_ref := start_engine_logic (); (* Start the engine *)
    Lwt.async (fun () -> 
      Lwt.catch (fun () -> !final_engine_promise_ref) (fun ex -> 
        Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.main") 
          (Printf.sprintf "Engine task failed: %s" (Printexc.to_string ex))
        >>= fun () -> 
          if not !cleanup_initiated then Lwt.wakeup_later resolve_quit (); (* Also trigger cleanup on engine fail *)
          Lwt.return_unit
      ) 
    );

    (* Set up Lwt signal handler for Ctrl+C (SIGINT) *)
    let _sighandler_id = Lwt_unix.on_signal Sys.sigint (fun _signum ->
        if not !cleanup_initiated then (
          Printf.eprintf "\\n[!] SIGINT received, initiating exit...\\n%!";
          Lwt.wakeup_later resolve_quit (); (* Signal the main loop to start cleanup *)
        )
      )
    in

    (* Main loop waits for quit_promise. Cleanup is performed after it resolves. *)
    Lwt_main.run (
      quit_promise >>= fun () ->
      (* Actual cleanup sequence *)
      if not !cleanup_initiated then (
        cleanup_initiated := true; (* Mark cleanup as started here *)
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
    (* Run only the engine logic in normal mode *)
    Lwt_main.run (start_engine_logic ())

let () = main ()
