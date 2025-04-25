open Lwt.Infix
open Types.Core

(* Set up logging *)
let setup_logging () =
  Lwt_log.add_rule "*" Lwt_log_core.Error; (* Default to Error level *)
  Lwt_log.add_rule "engine.strategy" Lwt_log_core.Info; (* Allow Info for strategy *) 
  Lwt_log.add_rule "engine.router" Lwt_log_core.Info;   (* Allow Info for router *) 
  Lwt_log_core.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()


let main () =
  (* Initialize Logging FIRST *)
  setup_logging ();

  (* Initialize the default RNG for crypto operations (TLS) using the Unix backend *)
  Mirage_crypto_rng_unix.use_default ();

  (* Load .env file to get the API token *)
  (try Dotenv.export ~path:"src/exchange/kraken/.env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!");

  Printf.printf "Starting market and execution feeds...\n%!";

  (* Start the engine with all components *)
  Lwt_main.run (
    Lwt.catch
      (fun () ->
        (* Retrieve Auth Token using Kraken.Token.get_token, handle potential errors *)
        (Lwt.catch 
          (fun () -> Kraken.Token.get_token () >>= fun token -> Lwt.return_some token)
          (fun exn -> 
            Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.auth") 
              (Printf.sprintf "Failed to retrieve auth token: %s" (Printexc.to_string exn)) >>= fun () ->
            Lwt.return_none
          )
        ) >>= fun auth_token_opt ->

        (* Create Kraken WS Feed Configuration *)
        let cfg : Types.Core.config = {
          ws_host = "ws.kraken.com";
          ws_port = 443;
          ws_path = "/v2"; (* Base path, connect function handles auth/public specific paths *)
          symbols = ["BTC/USD"; "ETH/USD"]; (* Hardcoded symbols *)
          auth_token = auth_token_opt; (* Use token option from caught get_token *)
        } in

        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.config") 
          (Printf.sprintf "Using symbols: %s" (String.concat ", " cfg.symbols)) >>= fun () ->

        (* Create strategy and router modules *)
        let strategy : Types.Core.strategy = {
          (* Signature: start: config -> tick_buffer -> cmd_buffer -> unit Lwt.t *)
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
