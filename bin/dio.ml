open Lwt.Infix
open Conduit_lwt_unix
open Types
open Dio_lib (* For Pacdash and Stats, and potentially Engine if it's moved into Dio lib *)

(* Set up logging *)
let setup_logging () =
  Lwt_log.add_rule "*" Lwt_log_core.Error;
  Lwt_log.add_rule "*" Lwt_log_core.Warning; 
  (* Lwt_log.add_rule "engine.router" Lwt_log_core.Info;   
  Lwt_log.add_rule "engine.router" Lwt_log_core.Debug;   
  Lwt_log.add_rule "kraken_ws_feed" Lwt_log_core.Debug;
  Lwt_log.add_rule "kraken_ws_exec" Lwt_log_core.Info;   
  Lwt_log.add_rule "kraken_ws_exec" Lwt_log_core.Debug;
  Lwt_log.add_rule "engine.strategy" Lwt_log_core.Info; 
  Lwt_log.add_rule "engine.strategy.grid_verify" Lwt_log_core.Info;
  Lwt_log.add_rule "engine.supervisor" Lwt_log_core.Info;        
  Lwt_log.add_rule "pacdash" Lwt_log_core.Info; *)
  (* Allow Info for router *) 
  Lwt_log_core.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

(* Read and parse config file *)
let read_config config_path : (Config.runtime_cfg * Config.engine_config, string) result = (* Return both configs *)
  try
    let json = Yojson.Safe.from_file config_path in
    let runtime_cfg = Config.runtime_cfg_of_yojson_exn json in
    (* Convert runtime_cfg to Core.config *)
    let symbols = List.map (fun asset -> asset.Config.symbol) runtime_cfg.assets in
    let core_cfg = { (* Rename to core_cfg for clarity *)
      Config.ws_host = "ws.kraken.com";
      Config.ws_port = 443;
      Config.ws_path = "/v2";
      Config.symbols;
      Config.auth_token = None; (* Will be set later from .env *)
    } in
    Ok (runtime_cfg, core_cfg) (* Return tuple *)
  with
  | Yojson.Json_error msg -> Error (Printf.sprintf "Invalid JSON in config file: %s" msg)
  | Sys_error msg -> Error (Printf.sprintf "Failed to read config file: %s" msg)
  | exn -> Error (Printf.sprintf "Unexpected error reading config: %s" (Printexc.to_string exn))

let mode_dash = ref false

(* Placeholder for any other existing command line arguments your application might have *)
let your_other_args = []

let specs = [
  ("--pacdash", Arg.Set mode_dash, " Run Pac-Man-style dashboard")
] @ your_other_args

(* Renamed function to reflect it starts the core logic and returns a promise *)
let start_engine_logic () : unit Lwt.t = 
  (* This was the original Lwt_main.run block, now returns the promise *)
  init () >>= fun _ctx -> (* Assuming init() is defined elsewhere or should be part of this block *)
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

  Arg.parse specs (fun anon_arg -> Printf.eprintf "Warning: Ignoring anonymous argument: %s\n" anon_arg) "dio options";

  if !mode_dash then begin
    (* Removed Lwt_log_core.default := Lwt_log_core.null; to allow logging in dash mode *)
    let term_instance = Pacdash.start () in
    let quit_promise, resolve_quit = Lwt.wait () in

    (* Start the engine logic and store the promise *)
    let engine_promise = start_engine_logic () in
    Lwt.async (fun () -> 
      Lwt.catch (fun () -> engine_promise) (fun ex -> 
        Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.main") 
          (Printf.sprintf "Engine task failed: %s" (Printexc.to_string ex))
      ) 
    );

    (* Define a common cleanup function *)
    let cleanup_initiated = ref false in
    let cleanup_and_exit () =
      if not !cleanup_initiated then (
        cleanup_initiated := true;
        Printf.eprintf "\\n[!] Exit requested, cleaning up...\\n%!";
        Lwt.cancel engine_promise; 
        (* Release terminal asynchronously *)
        Lwt.async (fun () ->
            Lwt.catch
              (fun () -> Notty_lwt.Term.release term_instance)
              (fun _ -> Lwt.return_unit) (* Ignore errors during release *)
        );
        (* Resolve the main loop promise to exit (schedule for later) *)
        Lwt.wakeup_later resolve_quit ();
      )
    in

    (* Set up Lwt signal handler for Ctrl+C (SIGINT) *)
    let _sighandler_id = Lwt_unix.on_signal Sys.sigint (fun _signum ->
        cleanup_and_exit ()
      )
    in

    (* Also listen for any keyboard input to exit *)
    Lwt.async (fun () ->
      let rec wait_for_key () =
        match%lwt Lwt_stream.get (Notty_lwt.Term.events term_instance) with
        | Some (`Key _ | `Mouse _ | `Paste _) -> (* Any key, mouse, or paste event triggers exit *)
            Printf.eprintf "\\n[!] Input event received, initiating exit...\\n%!";
            cleanup_and_exit (); 
            Lwt.return_unit (* Stop listening *)
        | Some (`Resize _) -> 
            (* Optional: Handle resize if needed, or just continue listening *)
            wait_for_key () (* Continue listening for other input *)
        | None -> 
            (* Stream closed, might mean terminal was released *)
            Printf.eprintf "\\n[!] Event stream closed, stopping input loop.\\n%!";
            if not !cleanup_initiated then Lwt.wakeup_later resolve_quit (); (* Exit if not already doing so *)
            Lwt.return_unit
      in
      wait_for_key ()
    );

    (* Run main loop, waiting for quit_promise (resolved by cleanup function) *)
    Lwt_main.run quit_promise;

  end else
    (* Run only the engine logic in normal mode *)
    Lwt_main.run (start_engine_logic ())

let () = main ()
