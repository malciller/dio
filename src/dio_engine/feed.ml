(* src/engine/feed.ml *)
open Lwt.Infix 
open Dio_types



(* Define the WebSocket Interface using Core.config *)
module type WS = sig
  type config = Config.engine_config 

  val start : ?runtime_cfg:Config.runtime_cfg -> config -> on_tick:(Event.tick -> unit Lwt.t) -> unit Lwt.t
  val start_executions : config -> on_execution:(Core.market_event list -> unit Lwt.t) -> unit Lwt.t
  val get_open_buy_orders : unit -> (string, Kraken.Kraken_common_types.order) Hashtbl.t
end

(* Implement the Functor *)
module Make (W : WS) = struct
  let section = Lwt_log_core.Section.make "engine.feed"

  let start ?runtime_cfg (cfg : Config.engine_config) ~on_tick =
    let rec retry_loop () =
      Lwt.catch
        (fun () -> W.start ?runtime_cfg cfg ~on_tick) 
        (fun exn ->
          Lwt_log_core.error_f ~section "Error starting public feed: %s. Retrying in 5s..." (Printexc.to_string exn) >>= fun () ->
          Lwt_unix.sleep 5.0 >>= fun () ->
          retry_loop ())
    in
    retry_loop ()

  let start_executions (cfg : Config.engine_config) ~on_execution =
    match cfg.auth_token with
    | Some _ ->
        let rec retry_loop () =
          Lwt.catch
            (fun () -> W.start_executions cfg ~on_execution) 
            (fun exn ->
              Lwt_log_core.error_f ~section "Error starting execution feed: %s. Retrying in 5s..." (Printexc.to_string exn) >>= fun () ->
              Lwt_unix.sleep 5.0 >>= fun () ->
              retry_loop ())
        in
        retry_loop ()
    | None ->
        Lwt_log_core.warning ~section "Auth token not found, skipping execution feed." >>= fun () ->
        Lwt.return_unit
end

(* Define the production implementation using the real Kraken_incoming_data *)
module Kraken_ws : WS with type config = Config.engine_config = struct
  type config = Config.engine_config 

  let start ?runtime_cfg cfg ~on_tick : unit Lwt.t =
    (Kraken.Kraken_incoming_data.start ?runtime_cfg cfg ~on_tick : unit Lwt.t)

  let start_executions cfg ~on_execution : unit Lwt.t =
    (Kraken.Kraken_incoming_data.start_executions cfg ~on_execution : unit Lwt.t)

  let get_open_buy_orders = Kraken.Kraken_incoming_data.get_all_open_orders
end

module Prod = Make (Kraken_ws)