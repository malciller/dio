(* src/engine/feed.ml *)
open Lwt.Infix  (* for >>= *)
open Types (* To access Event.tick and market_event type for on_tick/on_execution *)
open Types.Core (* For market_event *)

(* Import the Kraken feed module *)
module Kraken_feed = Kraken.Ws_feed

(* Define the configuration type, mirroring Kraken.Ws_feed.config *)
type config = {
  ws_host: string;
  ws_port: int;
  ws_path: string;
  symbols: string list;
  auth_token: string option;
}

(* Convert our config to Kraken_feed.config *)
let to_kraken_config (cfg : config) : Kraken_feed.config = {
  ws_host = cfg.ws_host;
  ws_port = cfg.ws_port;
  ws_path = cfg.ws_path;
  symbols = cfg.symbols;
  auth_token = cfg.auth_token;
}

(* Section for logging *)
let section = Lwt_log_core.Section.make "engine.feed"

(* Start the public data feed (e.g., Ticker) *)
let start cfg ~on_tick:(on_tick : Event.tick -> unit Lwt.t) =
  Lwt_log_core.info ~section "Starting Kraken WebSocket feed..." >>= fun () ->
  Lwt.catch
    (fun () -> Kraken_feed.start (to_kraken_config cfg) ~on_tick)
    (fun exn ->
      Lwt_log_core.error_f ~section "Error starting public feed: %s" (Printexc.to_string exn) >>= fun () ->
      Lwt.fail exn (* Re-raise the exception after logging *)
    )

(* Start the private data feed (e.g., Executions) *)
let start_executions cfg ~on_execution:(on_execution : market_event list -> unit Lwt.t) =
  match cfg.auth_token with
  | Some _ ->
      Lwt_log_core.info ~section "Auth token found, starting Kraken execution feed..." >>= fun () ->
      Lwt.catch
        (fun () -> Kraken_feed.start_executions (to_kraken_config cfg) ~on_execution)
        (fun exn ->
          Lwt_log_core.error_f ~section "Error starting execution feed: %s" (Printexc.to_string exn) >>= fun () ->
          Lwt.fail exn (* Re-raise the exception after logging *)
        )
  | None ->
      Lwt_log_core.warning ~section "Auth token not found or failed to retrieve, skipping execution feed." >>= fun () ->
      Lwt.return_unit (* Return a resolved promise if no token *)