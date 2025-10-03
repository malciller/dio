(** Feed module: Manages WebSocket connections for market data and order executions.

    This module provides a unified interface for streaming market data and trade
    executions from exchanges, with built-in retry logic and error handling. *)

open Lwt.Infix
open Dio_types

(** WebSocket interface for exchange connectivity.
    Defines the contract for streaming market data and order events. *)
module type WS = sig
  type config = Config.engine_config

  (** Start public market data feed with tick callbacks.
      @param runtime_cfg Optional runtime configuration overrides
      @param config Engine configuration containing exchange details
      @param on_tick Callback for processing market ticks *)
  val start : ?runtime_cfg:Config.runtime_cfg -> config -> on_tick:(Event.tick -> unit Lwt.t) -> unit Lwt.t

  (** Start authenticated execution feed for order updates.
      @param config Engine configuration with auth credentials
      @param on_execution Callback for processing order execution events *)
  val start_executions : config -> on_execution:(Core.market_event list -> unit Lwt.t) -> unit Lwt.t

  (** Retrieve current open buy orders.
      @return Hashtable mapping order IDs to order details *)
  val get_open_buy_orders : unit -> (string, Kraken.Kraken_common_types.order) Hashtbl.t
end

(** Functor that wraps WebSocket implementations with retry logic.
    Provides resilient connection management for exchange feeds. *)
module Make (W : WS) = struct
  let section = Lwt_log_core.Section.make "engine.feed"
  
  
  (* Lightweight performance profiler to reduce telemetry overhead *)
  let profile_operation name operation =
    let start_time = Unix.gettimeofday () in
    operation () >>= fun result ->
    let duration = Unix.gettimeofday () -. start_time in
    (* Only record telemetry for operations that take significant time *)
    if duration > 0.0001 then (
      Lwt.async (fun () -> Telemetry.record_timer ["feed"] (name ^ "_duration") duration)
    );
    Lwt.return result

  (** Start market data feed with automatic reconnection on failures. *)
  let start ?runtime_cfg (cfg : Config.engine_config) ~on_tick =
    (* Removed tick rate tracking variables - no longer needed *)
    
    let instrumented_on_tick tick =
      (* Use the profile_operation utility for detailed timing *)
      profile_operation "tick_processing" (fun () ->
        on_tick tick
      )
    in
    
    let rec retry_loop () =
      let retry_start_time = Unix.gettimeofday () in
      Lwt.catch
        (fun () -> 
          W.start ?runtime_cfg cfg ~on_tick:instrumented_on_tick)
        (fun exn ->
          let retry_duration = Unix.gettimeofday () -. retry_start_time in
          Lwt.async (fun () -> Telemetry.record_timer ["feed"] "connection_retry_duration" retry_duration);
          Lwt_log_core.error_f ~section "Error starting public feed: %s. Retrying in 5s..." (Printexc.to_string exn) >>= fun () ->
          Lwt_unix.sleep 5.0 >>= fun () ->
          retry_loop ())
    in
    (* Removed tick rate metric - it's a derived rate, not a duration measurement *)
    retry_loop ()

  (** Start execution feed with automatic reconnection.
      Requires authentication token; logs warning and skips if missing. *)
  let start_executions (cfg : Config.engine_config) ~on_execution =
    
    let instrumented_on_execution executions =
      (* Use profiling for execution processing with detailed metrics *)
      profile_operation "execution_processing" (fun () ->
        on_execution executions
      ) >>= fun () ->
      
      Lwt.return_unit
    in
    
    match cfg.auth_token with
    | Some _ ->
        let rec retry_loop () =
          let exec_retry_start_time = Unix.gettimeofday () in
          Lwt.catch
            (fun () -> 
              W.start_executions cfg ~on_execution:instrumented_on_execution)
            (fun exn ->
              let exec_retry_duration = Unix.gettimeofday () -. exec_retry_start_time in
              Lwt.async (fun () -> Telemetry.record_timer ["feed"] "exec_connection_retry_duration" exec_retry_duration);
              Lwt_log_core.error_f ~section "Error starting execution feed: %s. Retrying in 5s..." (Printexc.to_string exn) >>= fun () ->
              Lwt_unix.sleep 5.0 >>= fun () ->
              retry_loop ())
        in
        retry_loop ()
    | None ->
        Lwt_log_core.warning ~section "Auth token not found, skipping execution feed." >>= fun () ->
        Lwt.return_unit
end

(** Production WebSocket implementation using Kraken exchange.
    Provides concrete implementation of WS interface for live trading. *)
module Kraken_ws : WS with type config = Config.engine_config = struct
  type config = Config.engine_config

  let start ?runtime_cfg cfg ~on_tick : unit Lwt.t =
    (Kraken.Kraken_incoming_data.start_market_data ?runtime_cfg cfg ~on_tick : unit Lwt.t)

  let start_executions cfg ~on_execution : unit Lwt.t =
    (Kraken.Kraken_incoming_data.start_executions cfg ~on_execution : unit Lwt.t)

  let get_open_buy_orders = Kraken.Kraken_incoming_data.get_all_open_orders
end

(** Production feed instance using Kraken WebSocket implementation. *)
module Prod = Make (Kraken_ws)