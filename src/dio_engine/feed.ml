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

  (** Start market data feed with automatic reconnection on failures. *)
  let start ?runtime_cfg (cfg : Config.engine_config) ~on_tick =
    let tick_counter = ref 0 in
    let last_tick_count_for_rate = ref 0 in
    let last_rate_sample_time = ref (Unix.gettimeofday ()) in
    
    Lwt.async (fun () -> Telemetry.record_counter ["feed"] "connection_initialized" 1);

    let instrumented_on_tick tick =
      let start_time = Unix.gettimeofday () in
      incr tick_counter;
      Lwt.async (fun () ->
        Telemetry.record_counter ["feed"] "ticks_processed" 1 >>= fun () ->
        let processing_time = Unix.gettimeofday () -. start_time in
        Telemetry.record_timer ["feed"] "tick_processing_duration" processing_time
      );
      on_tick tick
    in
    
    let rec retry_loop () =
      Lwt.catch
        (fun () -> 
          Lwt.async (fun () -> Telemetry.record_counter ["feed"] "connection_attempts" 1);
          W.start ?runtime_cfg cfg ~on_tick:instrumented_on_tick)
        (fun exn ->
          Lwt.async (fun () -> Telemetry.record_counter ["feed"] "connection_failures" 1);
          Lwt_log_core.error_f ~section "Error starting public feed: %s. Retrying in 5s..." (Printexc.to_string exn) >>= fun () ->
          Lwt_unix.sleep 5.0 >>= fun () ->
          retry_loop ())
    in
    (* Background sampler for tick rate (ticks per second) *)
    Lwt.async (fun () ->
      let rec loop () =
        Lwt_unix.sleep 1.0 >>= fun () ->
        let now = Unix.gettimeofday () in
        let dt = now -. !last_rate_sample_time in
        let delta = !tick_counter - !last_tick_count_for_rate in
        if dt > 0.0 then (
          let rate = (Float.of_int delta) /. dt in
          Lwt.async (fun () -> Telemetry.record_gauge ["feed"] "tick_rate" rate)
        );
        (* Also record as histogram for better statistical analysis *)
        if delta > 0 then (
          let rate = (Float.of_int delta) /. dt in
          Lwt.async (fun () ->
            let histogram_values = List.init delta (fun _ -> rate) in
            Telemetry.record_histogram ["feed"] "tick_rate_histogram" histogram_values
          )
        );
        last_rate_sample_time := now;
        last_tick_count_for_rate := !tick_counter;
        loop ()
      in
      loop ()
    );
    retry_loop ()

  (** Start execution feed with automatic reconnection.
      Requires authentication token; logs warning and skips if missing. *)
  let start_executions (cfg : Config.engine_config) ~on_execution =
    let exec_counter = ref 0 in
    let last_exec_count_for_rate = ref 0 in
    let last_exec_rate_sample_time = ref (Unix.gettimeofday ()) in
    
    Lwt.async (fun () -> Telemetry.record_counter ["feed"] "exec_connection_initialized" 1);

    let instrumented_on_execution executions =
      let count = List.length executions in
      let start_time = Unix.gettimeofday () in
      exec_counter := !exec_counter + count;
      Lwt.async (fun () ->
        Telemetry.record_counter ["feed"] "executions_processed" count >>= fun () ->
        let processing_time = Unix.gettimeofday () -. start_time in
        Telemetry.record_timer ["feed"] "execution_processing_duration" processing_time
      );
      on_execution executions
    in
    
    match cfg.auth_token with
    | Some _ ->
        let rec retry_loop () =
          Lwt.catch
            (fun () -> 
              Lwt.async (fun () -> Telemetry.record_counter ["feed"] "exec_connection_attempts" 1);
              W.start_executions cfg ~on_execution:instrumented_on_execution)
            (fun exn ->
              Lwt.async (fun () -> Telemetry.record_counter ["feed"] "exec_connection_failures" 1);
              Lwt_log_core.error_f ~section "Error starting execution feed: %s. Retrying in 5s..." (Printexc.to_string exn) >>= fun () ->
              Lwt_unix.sleep 5.0 >>= fun () ->
              retry_loop ())
        in
        (* Background sampler for execution event rate (events per second) *)
        Lwt.async (fun () ->
          let rec loop () =
            Lwt_unix.sleep 1.0 >>= fun () ->
            let now = Unix.gettimeofday () in
            let dt = now -. !last_exec_rate_sample_time in
            let delta = !exec_counter - !last_exec_count_for_rate in
            if dt > 0.0 then (
              let rate = (Float.of_int delta) /. dt in
              Lwt.async (fun () -> Telemetry.record_gauge ["feed"] "exec_rate" rate)
            );
            (* Also record as histogram for better statistical analysis *)
            if delta > 0 then (
              let rate = (Float.of_int delta) /. dt in
              Lwt.async (fun () ->
                let histogram_values = List.init delta (fun _ -> rate) in
                Telemetry.record_histogram ["feed"] "exec_rate_histogram" histogram_values
              )
            );
            last_exec_rate_sample_time := now;
            last_exec_count_for_rate := !exec_counter;
            loop ()
          in
          loop ()
        );
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
    (Kraken.Kraken_incoming_data.start ?runtime_cfg cfg ~on_tick : unit Lwt.t)

  let start_executions cfg ~on_execution : unit Lwt.t =
    (Kraken.Kraken_incoming_data.start_executions cfg ~on_execution : unit Lwt.t)

  let get_open_buy_orders = Kraken.Kraken_incoming_data.get_all_open_orders
end

(** Production feed instance using Kraken WebSocket implementation. *)
module Prod = Make (Kraken_ws)