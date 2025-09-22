(**
 * Trading engine core module - coordinates feed consumption, strategy execution, and order routing
 *)

open Lwt.Infix
open Dio_types
module Feed = Feed
module Kraken = Kraken
module Discord_webhook = Discord_webhook

(** Push market tick data to ring buffer for processing with timing *)
let push_tick_to_buffer tick_buffer tick =
  let start_time = Unix.gettimeofday () in
  Ringbuffer.push tick_buffer tick >>= fun () ->
  let duration = Unix.gettimeofday () -. start_time in
  (* Always record timing for tick buffer operations - critical path *)
  Lwt.async (fun () -> 
    Telemetry.record_timer ["engine"] "tick_buffer_push_duration" duration
    (* Removed tick_buffer_depth_at_push gauge - non-duration metric *)
  );
  Lwt.return_unit

(** Push execution events to ring buffer with timing *)
let push_execs_to_buffer exec_buffer events =
  let start_time = Unix.gettimeofday () in
  let count = List.length events in
  Lwt_list.iter_s (Ringbuffer.push exec_buffer) events >>= fun () ->
  let duration = Unix.gettimeofday () -. start_time in
  (* Always record execution buffer timing *)
  Lwt.async (fun () -> 
    Telemetry.record_timer ["engine"] "exec_buffer_push_duration" duration >>= fun () ->
    if count > 0 then 
      Telemetry.record_timer ["engine"] "exec_buffer_per_item_push_duration" (duration /. Float.of_int count)
    else 
      Lwt.return_unit
  );
  Lwt.return_unit

(**
 * Initialize and start market data feeds (ticks and executions) with ring buffer callbacks
 * @param runtime_cfg Runtime configuration parameters
 * @param core_cfg Engine-specific configuration
 * @param tick_buffer Buffer for market tick data
 * @param exec_buffer Buffer for execution events
 * @return Lwt promise that resolves when feeds terminate
 *)
let start_feed (runtime_cfg: Config.runtime_cfg) (core_cfg: Config.engine_config) (tick_buffer: Event.tick Ringbuffer.t) (exec_buffer: Core.market_event Ringbuffer.t) =
  let feed_promise = Feed.Prod.start ~runtime_cfg core_cfg ~on_tick:(push_tick_to_buffer tick_buffer) in
  let executions_promise = Feed.Prod.start_executions core_cfg ~on_execution:(push_execs_to_buffer exec_buffer) in
  Lwt.join [feed_promise; executions_promise]

(**
 * Main engine entry point - initializes buffers and starts supervised trading system
 * @param grid_strategy Grid trading strategy implementation
 * @param orderbook_strategy Orderbook management strategy
 * @param arbitrage_strategy Arbitrage detection and execution logic
 * @param router Order routing and exchange connectivity
 * @param runtime_cfg Runtime configuration parameters
 * @param core_cfg Engine-specific configuration
 * @return Lwt promise representing the complete trading session
 *)
let run ~grid_strategy ~orderbook_strategy ~vmm_strategy ~arbitrage_strategy ~router (runtime_cfg: Config.runtime_cfg) (core_cfg: Config.engine_config) =
  (* Create the ring buffers with configurable capacity and telemetry *)
  let buffer_cap = runtime_cfg.queues_cap in
  let tick_buffer = Ringbuffer.create ~name:"tick_buffer" buffer_cap in
  let exec_buffer = Ringbuffer.create ~name:"exec_buffer" buffer_cap in
  let cmd_buffer  = Ringbuffer.create ~name:"cmd_buffer" buffer_cap in

  (* Initialize ringbuffer telemetry interface *)
  Ringbuffer.TelemetryInterface.set_functions
    Telemetry.record_timer
    Telemetry.record_counter
    Telemetry.record_gauge;

  (* Initialize telemetry for ringbuffers in other modules *)
  Kraken.Kraken_balances.RingbufferTelemetryInterface.set_functions
    Telemetry.record_timer
    Telemetry.record_counter
    Telemetry.record_gauge;

  Kraken.Kraken_outgoing_data.RingbufferTelemetryInterface.set_functions
    Telemetry.record_timer
    Telemetry.record_counter
    Telemetry.record_gauge;

  Router.RingbufferTelemetryInterface.set_functions
    Telemetry.record_timer
    Telemetry.record_counter
    Telemetry.record_gauge;

  Discord_webhook.RingbufferTelemetryInterface.set_functions
    Telemetry.record_timer
    Telemetry.record_counter
    Telemetry.record_gauge;

  (* Start all components via the supervisor *)
  Supervisor.start
    ~feed_initializer_fn:(fun () -> start_feed runtime_cfg core_cfg tick_buffer exec_buffer)
    ~grid_strategy
    ~orderbook_strategy
    ~vmm_strategy
    ~arbitrage_strategy
    ~router
    ~tick_buffer
    ~exec_buffer
    ~cmd_buffer
    runtime_cfg
    core_cfg
