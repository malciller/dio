(* src/engine/engine.ml *)

open Dio_types 
module Feed = Feed
module Kraken = Kraken

(* Asynchronously push a single tick onto the buffer *)
let push_tick_to_buffer tick_buffer tick =
  Ringbuffer.push tick_buffer tick

(* Asynchronously push a list of execution events onto the buffer *)
let push_execs_to_buffer exec_buffer events =
  Lwt_list.iter_s (Ringbuffer.push exec_buffer) events

(* Adapter function that starts both feed streams using ring buffers *)
let start_feed (runtime_cfg: Config.runtime_cfg) (core_cfg: Config.engine_config) (tick_buffer: Event.tick Ringbuffer.t) (exec_buffer: Core.market_event Ringbuffer.t) =
  (* Use the helper functions as callbacks *) 
  let feed_promise = Feed.Prod.start ~runtime_cfg core_cfg ~on_tick:(push_tick_to_buffer tick_buffer) in
  let executions_promise = Feed.Prod.start_executions core_cfg ~on_execution:(push_execs_to_buffer exec_buffer) in
  Lwt.join [feed_promise; executions_promise]

(* Main run function that orchestrates all components *)
let run ~grid_strategy ~orderbook_strategy ~arbitrage_strategy ~router (runtime_cfg: Config.runtime_cfg) (core_cfg: Config.engine_config) =
  (* Create the ring buffers with configurable capacity *)
  let buffer_cap = runtime_cfg.queues_cap in
  let tick_buffer = Ringbuffer.create buffer_cap in
  let exec_buffer = Ringbuffer.create buffer_cap in
  let cmd_buffer  = Ringbuffer.create buffer_cap in

  (* Start all components via the supervisor *)
  Supervisor.start
    ~feed_initializer_fn:(fun () -> start_feed runtime_cfg core_cfg tick_buffer exec_buffer)
    ~grid_strategy
    ~orderbook_strategy
    ~arbitrage_strategy
    ~router
    ~tick_buffer
    ~exec_buffer
    ~cmd_buffer
    runtime_cfg
    core_cfg
