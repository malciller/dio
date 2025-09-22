(**
 * Main entry point for the Dio trading application.
 *
 * This module initializes the application, sets up logging, reads configuration,
 * and orchestrates the trading engine with support for both dashboard and headless modes.
 *)

open Lwt.Infix
open Conduit_lwt_unix
open Dio_types
open Lwt_log_core

(** Flag to determine if running in dashboard mode *)
let mode_dash = ref false

(** Main application logging section *)
let section = Section.make "dio.main"
(** Configuration-specific logging section *)
let config_section = Section.make "dio.config"

(**
 * Configure logging system with appropriate output handlers.
 *
 * In dashboard mode, routes logs through the dashboard interface.
 * In normal mode, prints formatted logs to stdout.
 *)
let setup_logging () =
  (** Set base log levels for all sections *)
  Lwt_log.add_rule "*" Error;
  Lwt_log.add_rule "*" Warning;
  (*Lwt_log.add_rule "*" Info;  *)
  (*Lwt_log.add_rule "*" Debug;  *)

  (** Create logger based on execution mode *)
  let default_logger =
    if !mode_dash then
      (** Dashboard mode: route logs to dashboard interface *)
      make
        ~output:(fun section level messages ->
          if Section.name section = "dashboard" then
            (** Prevent recursion from dashboard logging *)
            Lwt.return_unit
          else if List.length messages > 0 then
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
                (Section.name section)
                (string_of_level level)
                first_message_string
            in
            Dashboard.Stats.add_dashboard_log formatted_message;
            Lwt.return_unit
          else
            Lwt.return_unit
        )
        ~close:(fun () -> Lwt.return_unit)
      else
        (** Normal mode: print formatted logs to stdout *)
        make
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
                  (Section.name section)
                  (string_of_level level)
                  first_message_string
              in
              Lwt_io.printf "%s\n" formatted_message
            else
              Lwt.return_unit
          )
          ~close:(fun () -> Lwt.return_unit)
            in
  default := default_logger;

  (** Additional section-specific log level rules can be added here *)
    (*Lwt_log.add_rule "engine.strategy.kraken.VMM" Debug; 
    Lwt_log.add_rule "engine.strategy.kraken.GMM" Info;*) 

  
  ()

(**
 * Read and validate application configuration from JSON file.
 *
 * @param config_path Path to the configuration JSON file
 * @return Result containing (runtime_config, engine_config) tuple or error message
 *
 * Performs validation, updates global state with strategy assignments,
 * and constructs engine configuration with API credentials from environment.
 *)
let read_config config_path : (Config.runtime_cfg * Config.engine_config, string) result =
  try
    let json = Yojson.Safe.from_file config_path in
    let runtime_cfg : Config.runtime_cfg = Config.runtime_cfg_of_yojson_exn json in

    (** Validate configuration structure and constraints *)
    match Config.validate_runtime_cfg runtime_cfg with
    | Error msg -> Error (Printf.sprintf "Configuration validation failed:\n%s" msg)
    | Ok () ->

        (** Classify symbols by trading strategy *)
        let grid_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
          match asset.strategy with
          | Config.Grid -> Some asset.symbol
          | Config.GMM -> None
          | Config.VMM -> None
        ) runtime_cfg.assets in

        let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
          match asset.strategy with
          | Config.GMM -> Some asset.symbol
          | Config.Grid -> None
          | Config.VMM -> None
        ) runtime_cfg.assets in

        let vmm_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
          match asset.strategy with
          | Config.VMM -> Some asset.symbol
          | _ -> None
        ) runtime_cfg.assets in

        let all_symbols = List.map (fun (asset: Config.asset_cfg) -> asset.symbol) runtime_cfg.assets in

        info_f ~section:config_section "[CONFIG] Grid strategy symbols: [%s]" (String.concat ", " grid_symbols) |> Lwt.ignore_result;
        info_f ~section:config_section "[CONFIG] Orderbook strategy symbols: [%s]" (String.concat ", " orderbook_symbols) |> Lwt.ignore_result;
        info_f ~section:config_section "[CONFIG] VMM strategy symbols: [%s]" (String.concat ", " vmm_symbols) |> Lwt.ignore_result;

        (** Update global state with strategy assignments for each symbol *)
        List.iter (fun (asset: Config.asset_cfg) ->
          let strategy = match asset.strategy with
            | Config.Grid -> State.Grid
            | Config.GMM -> State.Orderbook
            | Config.VMM -> State.VMM
          in
          State.update_global_strategy_assignment asset.symbol strategy
        ) runtime_cfg.assets;

        let open Dio_types.State in
        set_symbols all_symbols;

        (** Retrieve required API credentials from environment variables *)
        let api_key =
          match Sys.getenv_opt "KRAKEN_API_KEY" with
          | Some key -> key
          | None ->
              Lwt_main.run (error_f ~section:config_section "FATAL: KRAKEN_API_KEY environment variable not set.");
              exit 1
        in
        let api_secret =
          match Sys.getenv_opt "KRAKEN_API_SECRET" with
          | Some secret -> secret
          | None ->
              Lwt_main.run (error_f ~section:config_section "FATAL: KRAKEN_API_SECRET environment variable not set.");
              exit 1
        in

        (** Construct engine configuration with Kraken API details *)
        let engine_cfg : Config.engine_config = {
          ws_host = "ws.kraken.com";
          ws_port = 443;
          ws_path = "/v2";
          symbols = all_symbols;
          auth_token = None; (** Will be set later after token retrieval *)
          kraken_api_key = api_key;
          kraken_api_secret = api_secret;
        } in
        Ok (runtime_cfg, engine_cfg)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "Invalid JSON in config file: %s" msg)
  | Sys_error msg -> Error (Printf.sprintf "Failed to read config file: %s" msg)
  | exn -> Error (Printf.sprintf "Unexpected error reading config: %s" (Printexc.to_string exn))

(** Placeholder for future command line arguments *)
let your_other_args = []

(** Command line argument specifications *)
let specs = [
  ("--dashboard", Arg.Set mode_dash, " Run Dashboard")
] @ your_other_args

(**
 * Initialize and start the trading engine with all components.
 *
 * Orchestrates the complete startup sequence:
 * 1. Load and validate configuration
 * 2. Retrieve Kraken authentication token
 * 3. Initialize balance WebSocket feeds
 * 4. Configure trading strategies and router
 * 5. Start the main engine loop
 *)
let start_engine_logic () : unit Lwt.t =
  init () >>= fun _ctx ->
  Lwt.catch
    (fun () ->
      (** Load configuration from default path *)
      match read_config "_config.json" with
      | Error msg ->
          error ~section:config_section msg >>= fun () ->
          Lwt.fail_with msg
      | Ok (runtime_cfg, core_cfg) ->
          (** Attempt to retrieve authentication token for private API access *)
          (Lwt.catch
            (fun () -> Kraken.Kraken_generate_auth_token.get_token () >>= fun token -> Lwt.return_some token)
            (fun exn ->
              let backtrace = Printexc.get_backtrace () in
              error_f ~section:(Section.make "engine.auth")
                "Failed to retrieve auth token: %s\nBacktrace:\n%s"
                   (Printexc.to_string exn)
                   backtrace
                >>= fun () ->
                Lwt.return_none
            )
          ) >>= fun auth_token_opt ->

          let core_cfg = { core_cfg with auth_token = auth_token_opt } in
          Lwt.return_unit >>= fun () ->

          (** Initialize balance monitoring if authentication succeeded *)
          (match auth_token_opt with
          | Some token -> Kraken.Kraken_balances.initialize_ws_balances_feed core_cfg token
          | None -> ());
          Lwt.return_unit >>= fun () ->

          (** Configure trading strategy implementations *)
          let grid_strategy : Core.grid_strategy = {
            start = Kraken_suicide_grid.start
          } in
          let orderbook_strategy : Core.orderbook_strategy = {
            start = Kraken_greedy_mm.start
          } in
          let vmm_strategy : Core.vmm_strategy = {
            start = Kraken_valley_mm.start
          } in
          let arbitrage_strategy : Core.arbitrage_strategy = {
            start = Kraken_arbitrage.start
          } in
          (** Configure event routing system *)
          let router : Core.router = {
            start = Router.start
          } in
          (** Start the main engine with all configured components *)
          Engine.run ~grid_strategy ~orderbook_strategy ~vmm_strategy ~arbitrage_strategy ~router runtime_cfg core_cfg
    )
    (fun exn ->
      error_f ~section "Error in engine: %s" (Printexc.to_string exn) >>= fun () ->
      Lwt.return_unit
    )

(**
 * Application entry point handling initialization and mode selection.
 *
 * Performs environment setup, loads configuration from .env file,
 * validates required environment variables, and starts either dashboard
 * or headless trading mode based on command line arguments.
 *)
let main () =
  (** Initialize cryptographic random number generator *)
  Mirage_crypto_rng_unix.use_default ();

  (** Load environment variables from .env file if present *)
  (try Dotenv.export ~path:".env" () with _ -> Lwt_main.run (warning_f ~section "Failed to load .env file."));

  (** Verify required environment variables are set *)
  let key_check = match Sys.getenv_opt "KRAKEN_API_KEY" with Some _ -> "FOUND" | None -> "MISSING" in
  let secret_check = match Sys.getenv_opt "KRAKEN_API_SECRET" with Some _ -> "FOUND" | None -> "MISSING" in
  let discord_check = match Sys.getenv_opt "DISCORD_WEBHOOK_URL" with Some _ -> "FOUND" | None -> "MISSING" in
  Lwt_main.run (info_f ~section "Post Dotenv.export: KRAKEN_API_KEY: %s, KRAKEN_API_SECRET: %s, DISCORD_WEBHOOK_URL: %s" key_check secret_check discord_check);

  (** Parse command line arguments *)
  Arg.parse specs (fun anon_arg -> Lwt_main.run (warning_f ~section "Ignoring anonymous argument: %s" anon_arg)) "dio options";

  (** Configure logging system based on selected mode *)
  setup_logging ();

  (** Dashboard mode: Interactive terminal interface with graceful shutdown *)
  if !mode_dash then begin
    (** Promise for coordinating shutdown across components *)
    let quit_promise, resolve_quit = Lwt.wait () in

    (** Prevent duplicate cleanup operations *)
    let cleanup_initiated = ref false in

    (** References to track running components for cleanup *)
    let term_instance_ref = ref None in
    let final_engine_promise_ref = ref (Lwt.return_unit) in

    (** Callback for dashboard-initiated shutdown *)
    let dashboard_on_quit () =
      if not !cleanup_initiated then (
        Lwt_main.run (info_f ~section "Dashboard requested exit. Signaling main loop...");
        Lwt.wakeup_later resolve_quit ();
      );
      Lwt.return_unit
    in

    (** Start dashboard interface *)
    let term = Dashboard.start ~on_quit:dashboard_on_quit () in
    term_instance_ref := Some term;

    (** Start trading engine in background *)
    final_engine_promise_ref := start_engine_logic ();
    Lwt.async (fun () ->
      Lwt.catch (fun () -> !final_engine_promise_ref) (fun ex ->
        error_f ~section:(Section.make "engine.main")
          "Engine task failed: %s" (Printexc.to_string ex)
        >>= fun () ->
          if not !cleanup_initiated then Lwt.wakeup_later resolve_quit ();
          Lwt.return_unit
      )
    );

    (** Handle SIGINT for graceful shutdown *)
    let _sighandler_id = Lwt_unix.on_signal Sys.sigint (fun _signum ->
        if not !cleanup_initiated then (
          Lwt_main.run (info_f ~section "SIGINT received, initiating exit...");
          Lwt.wakeup_later resolve_quit ();
        )
      )
    in

    (** Main event loop: wait for shutdown signal then cleanup *)
    Lwt_main.run (
      quit_promise >>= fun () ->
      if not !cleanup_initiated then (
        cleanup_initiated := true;
        Lwt_main.run (info_f ~section "Main loop exiting, performing final cleanup...");
        Lwt.cancel !final_engine_promise_ref;
        (match !term_instance_ref with
        | Some ti -> Notty_lwt.Term.release ti
        | None -> Lwt.return_unit)
        >>= fun () ->
          Lwt_main.run (info_f ~section "Cleanup complete. Exiting application.");
          Lwt.return_unit
      ) else (
        Lwt_main.run (info_f ~section "Cleanup already handled or in progress. Exiting application.");
        Lwt.return_unit
      )
    );

  end else

    (** Headless mode: Run trading engine without dashboard interface *)
    Lwt_main.run (start_engine_logic ())

(** Application entry point *)
let () = main ()
