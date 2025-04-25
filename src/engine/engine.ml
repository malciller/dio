(* src/engine/engine.ml *)
open Lwt.Infix  (* for >>= *)
open Types (* For Event.tick *)
open Types.Core (* For market_event, order_cmd, config, strategy, router *)


module Feed = Feed
module Kraken = Kraken


(* Define the callbacks the engine needs (keep this here) *)
(* type callbacks = { ... } -- No longer used directly by run *)

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
let start_feed (cfg: config) (tick_buffer: Event.tick Ringbuffer.t) (exec_buffer: market_event Ringbuffer.t) =
  (* Use the helper functions as callbacks *) 
  let feed_promise = Feed.Prod.start cfg ~on_tick:(push_tick_to_buffer tick_buffer) in
  let executions_promise = Feed.Prod.start_executions cfg ~on_execution:(push_execs_to_buffer exec_buffer) in
  Lwt.join [feed_promise; executions_promise]

(* Main run function that orchestrates all components *)
let run ~strategy ~router (cfg: config) =
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine") "Starting engine..." >>= fun () ->
  
  (* Create the ring buffers *)
  let tick_buffer = Ringbuffer.create 1024 in
  let exec_buffer = Ringbuffer.create 1024 in 
  let cmd_buffer  = Ringbuffer.create 1024 in

  (* Start all components via the supervisor *)
  Supervisor.start 
    ~feed:(start_feed cfg tick_buffer exec_buffer)
    ~strategy
    ~router
    ~tick_buffer
    ~exec_buffer
    ~cmd_buffer
    cfg >>= fun () ->
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine") "Engine started successfully"
