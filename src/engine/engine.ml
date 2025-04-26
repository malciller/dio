(* src/engine/engine.ml *)
open Types (* For Event.tick *)
open Types.Core (* For market_event, order_cmd, config, strategy, router *)
open Types.Config (* For runtime_cfg *)


module Feed = Feed
module Kraken = Kraken



(* Helper: Push a single tick onto the buffer, logging if full *)
(* Extracted for testability *) 
let push_tick_to_buffer tick_buffer tick = 
  if not (Ringbuffer.push tick_buffer tick) then
    Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.feed") "Tick buffer full! Dropping tick."
  else
    Lwt.return_unit

(* Helper: Push a list of execution events onto the buffer, logging if full *)
(* Extracted for testability *) 
let push_execs_to_buffer exec_buffer events =
  List.iter (fun event ->
    if not (Ringbuffer.push exec_buffer event) then
      Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.feed") "Execution buffer full! Dropping event." |> ignore
  ) events;
  Lwt.return_unit

(* Adapter function that starts both feed streams using ring buffers *)
(* This function only needs the core connection/symbol/token info *)
let start_feed (core_cfg: config) (tick_buffer: Event.tick Ringbuffer.t) (exec_buffer: market_event Ringbuffer.t) =
  (* Use the helper functions as callbacks *) 
  let feed_promise = Feed.Prod.start core_cfg ~on_tick:(push_tick_to_buffer tick_buffer) in
  let executions_promise = Feed.Prod.start_executions core_cfg ~on_execution:(push_execs_to_buffer exec_buffer) in
  Lwt.join [feed_promise; executions_promise]

(* Main run function that orchestrates all components *)
(* Updated signature to accept both runtime_cfg and core_cfg *)
let run ~strategy ~router (runtime_cfg: runtime_cfg) (core_cfg: config) =
  (* Create the ring buffers *)
  (* TODO: Potentially use runtime_cfg.queues_cap here? For now, keep fixed size. *)
  let tick_buffer = Ringbuffer.create 1024 in
  let exec_buffer = Ringbuffer.create 1024 in 
  let cmd_buffer  = Ringbuffer.create 1024 in

  (* Start all components via the supervisor *)
  (* Supervisor.start will also need its signature updated *)
  Supervisor.start 
    ~feed:(start_feed core_cfg tick_buffer exec_buffer) (* Pass core_cfg to feed *)
    ~strategy
    ~router
    ~tick_buffer
    ~exec_buffer
    ~cmd_buffer
    runtime_cfg (* Pass runtime_cfg to supervisor *)
    core_cfg (* Pass core_cfg to supervisor *)
