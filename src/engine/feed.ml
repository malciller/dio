(* src/engine/feed.ml *)
open Lwt.Infix  (* for >>= *)
open Types
open Types.Core (* Contains config, market_event, order_cmd etc. *)


(* 1. Define the WebSocket Interface using Types.Core.config *)
module type WS = sig
  type config = Types.Core.config (* Use the central config type *)

  val start : config -> on_tick:(Event.tick -> unit Lwt.t) -> unit Lwt.t
  val start_executions : config -> on_execution:(market_event list -> unit Lwt.t) -> unit Lwt.t
  val get_open_buy_orders : unit -> (string, Kraken.Ws_feed.order) Hashtbl.t
end

(* 2. Implement the Functor *)
module Make (W : WS) = struct
  let section = Lwt_log_core.Section.make "engine.feed"

  let start (cfg : Types.Core.config) ~on_tick =
    Lwt.catch
      (fun () -> W.start cfg ~on_tick) (* W.start expects Core.config *)
      (fun exn ->
        Lwt_log_core.error_f ~section "Error starting public feed: %s" (Printexc.to_string exn) >>= fun () ->
        Lwt.fail exn)

  let start_executions (cfg : Types.Core.config) ~on_execution =
    match cfg.auth_token with
    | Some _ ->
        Lwt.catch
          (fun () -> W.start_executions cfg ~on_execution) (* W.start_executions expects Core.config *)
          (fun exn ->
            Lwt_log_core.error_f ~section "Error starting execution feed: %s" (Printexc.to_string exn) >>= fun () ->
            Lwt.fail exn)
    | None ->
        Lwt_log_core.warning ~section "Auth token not found, skipping execution feed." >>= fun () ->
        Lwt.return_unit
end

(* 3. Define the production implementation using the real Kraken Ws_feed *)
(* Kraken.Ws_feed now uses Types.Core.config, matching the WS signature *)
module Kraken_ws : WS with type config = Types.Core.config = struct
  type config = Types.Core.config (* Satisfy signature constraint *)

  (* Map functions, coercing return type *)
  let start cfg ~on_tick : unit Lwt.t =
    (Kraken.Ws_feed.start cfg ~on_tick : unit Lwt.t)

  let start_executions cfg ~on_execution : unit Lwt.t =
    (Kraken.Ws_feed.start_executions cfg ~on_execution : unit Lwt.t)

  let get_open_buy_orders = Kraken.Ws_feed.get_open_buy_orders
end

(* 4. Instantiate the production Feed module *)
module Prod = Make (Kraken_ws)