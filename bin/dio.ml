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

(** Flag to enable production telemetry optimizations (1% sampling, disabled hot paths) *)
let production_mode = ref false

(** Main application logging section *)
let section = Section.make "dio.main"
(** Configuration-specific logging section *)
let config_section = Section.make "dio.config"

(**
 * Truncate overly large log messages to prevent runaway logs.
 *
 * Sanitizes backslashes and truncates messages exceeding max_len.
 * Applied globally to all log output.
 *)
let truncate_log_message ?(max_len = 2048) (message : string) : string =
  let len = String.length message in
  let sanitized =
    if String.exists (fun c -> c = '\\') message then (
      let buf = Bytes.create len in
      String.iteri (fun i c ->
        match c with
        | '\\' -> Bytes.set buf i ' '
        | _ -> Bytes.set buf i c
      ) message;
      Bytes.unsafe_to_string buf
    ) else
      message
  in
  if len <= max_len then
    sanitized
  else
    let truncated = String.sub sanitized 0 max_len in
    Printf.sprintf "%s... [truncated %d bytes]" truncated (len - max_len)

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
  Lwt_log.add_rule "*" Info;  
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
            (** Truncate message to prevent runaway logs *)
            let truncated_message = truncate_log_message first_message_string in
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
                truncated_message
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
              (** Truncate message to prevent runaway logs *)
              let truncated_message = truncate_log_message first_message_string in
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
                  truncated_message
              in
              Lwt_io.printf "%s\n" formatted_message
            else
              Lwt.return_unit
          )
          ~close:(fun () -> Lwt.return_unit)
            in
  default := default_logger;

  (** Additional section-specific log level rules can be added here *)
    (*Lwt_log.add_rule "engine.strategy.kraken.VMM" Debug; ;*)
    (*Lwt_log.add_rule "engine.strategy.kraken.GMM" Debug;*)

  
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
  ("--dashboard", Arg.Set mode_dash, " Run Dashboard");
  ("--production", Arg.Set production_mode, " Enable production telemetry optimizations (1% sampling, disabled hot paths)");
] @ your_other_args

(**
 * Initialize and start the trading engine with all components.
 *
 * Orchestrates the complete startup sequence:
 * 1. Load and validate configuration
 * 2. Retrieve Kraken authentication token
 * 3. Initialize balance WebSocket feeds and fee cache
 * 4. Configure trading strategies and router
 * 5. Start the main engine loop
 *)
let start_engine_logic () : unit Lwt.t =
  init () >>= fun _ctx ->
  
  (** Start periodic telemetry reporting *)
  Lwt.async (fun () ->
    let rec telemetry_loop () =
      Lwt_unix.sleep 30.0 >>= fun () ->
      Telemetry.get_all_stats () >>= fun stats ->
      List.iter (fun (name, s) ->
        if s.Dio_types.Telemetry_types.count > 0 then
          info_f ~section:(Section.make "telemetry")
            "%s: count=%d avg=%.2fms p95=%.2fms p99=%.2fms"
            name s.Dio_types.Telemetry_types.count (s.Dio_types.Telemetry_types.mean *. 1000.0) (s.Dio_types.Telemetry_types.p95 *. 1000.0) (s.Dio_types.Telemetry_types.p99 *. 1000.0)
          |> Lwt.ignore_result
      ) stats;
      telemetry_loop ()
    in
    telemetry_loop ()
  );
  
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

          (** Initialize balance monitoring if authentication succeeded *)
          (match auth_token_opt with
          | Some token -> Kraken.Kraken_balances.initialize_ws_balances_feed core_cfg token
          | None -> ());

          let fee_pairs =
            runtime_cfg.assets
            |> List.map (fun (asset : Config.asset_cfg) -> asset.symbol)
            |> List.sort_uniq String.compare
          in
          let normalized_pairs = List.map String.uppercase_ascii fee_pairs in

          debug_f ~section:config_section "Prefetching maker/taker fees for %d symbol(s)" (List.length normalized_pairs) >>= fun () ->
          Kraken.Kraken_fee_cache.ensure_pairs core_cfg normalized_pairs >>= fun () ->
          let combined_pairs = List.combine fee_pairs normalized_pairs in
          Lwt_list.iter_s (fun (pair, symbol) ->
            let maker_opt = Kraken.Kraken_fee_cache.get_fee_rate symbol ~is_maker:true in
            let taker_opt = Kraken.Kraken_fee_cache.get_fee_rate symbol ~is_maker:false in
            debug_f ~section:config_section
              "[Fee Cache] %s (normalized %s) maker=%s taker=%s"
              pair symbol
              (match maker_opt with Some fee -> Printf.sprintf "%.6f" fee | None -> "missing")
              (match taker_opt with Some fee -> Printf.sprintf "%.6f" fee | None -> "missing")
          ) combined_pairs >>= fun () ->

          (** Configure trading strategy implementations *)
          let grid_strategy : Core.grid_strategy = {
            start = Kraken_suicide_grid.start
          } in
          let orderbook_strategy : Core.orderbook_strategy = {
            start = (fun runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer ->
              Kraken_mm.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer ~strategy_type:Config.GMM)
          } in
          let vmm_strategy : Core.vmm_strategy = {
            start = (fun runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer ->
              Kraken_mm.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer ~strategy_type:Config.VMM)
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

  (** Initialize telemetry system *)
  Lwt_main.run (
    info ~section "Initializing telemetry system" >>= fun () ->
    (if !production_mode then (
      info ~section "Enabling production telemetry mode (1% sampling, disabled hot paths)" >>= fun () ->
      Telemetry.set_config Dio_types.Telemetry_types.production_config;
      Lwt.return_unit
    ) else (
      info ~section "Using default telemetry configuration" >>= fun () ->
      Lwt.return_unit
    )) >>= fun () ->
    Telemetry.init ()
  );

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
        Lwt.async (fun () ->
          info_f ~section "Dashboard requested exit. Signaling main loop..."
        );
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
          Lwt.async (fun () -> info_f ~section "SIGINT received, initiating exit...");
          Lwt.wakeup_later resolve_quit ();
        )
      )
    in

    (** Main event loop: wait for shutdown signal then cleanup *)
    Lwt_main.run (
      quit_promise >>= fun () ->
      if not !cleanup_initiated then (
        cleanup_initiated := true;
        (match !term_instance_ref with
        | Some ti ->
            Notty_lwt.Term.release ti >>= fun () ->
            Lwt.return_unit
        | None ->
            Lwt.return_unit
        ) >>= fun () ->
        Lwt.return_unit
      ) else (
        Lwt.return_unit
      )
    );

  end else

    (** Headless mode: Run trading engine without dashboard interface *)
    Lwt_main.run (start_engine_logic ())

(** Application entry point *)
let () = main ()
