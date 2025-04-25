(* src/engine/engine.ml *)
open Lwt.Infix  (* for >>= *)
open Types (* For Event.tick *)
open Types.Core (* For market_event, order_cmd, etc. *)
open Types_engine (* Contains config, strategy, router types *)

module Feed = Feed
module Kraken = Kraken


(* Define the callbacks the engine needs (keep this here) *)
type callbacks = {
  on_tick: Event.tick -> unit Lwt.t;
  on_execution: market_event list -> unit Lwt.t;
}

(* Adapter function that starts both feed streams using ring buffers *)
let start_feed (cfg: config) (tick_buffer: Event.tick Ringbuffer.t) (exec_buffer: market_event Ringbuffer.t) =
  let on_tick_buffer tick = 
    if not (Ringbuffer.push tick_buffer tick) then
      Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.feed") "Tick buffer full! Dropping tick."
    else
      Lwt.return_unit
  in
  let on_execution_buffer events =
    List.iter (fun event ->
      if not (Ringbuffer.push exec_buffer event) then
        Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.feed") "Execution buffer full! Dropping event." |> ignore
    ) events;
    Lwt.return_unit
  in
  let feed_promise = Feed.start cfg ~on_tick:on_tick_buffer in
  let executions_promise = Feed.start_executions cfg ~on_execution:on_execution_buffer in
  Lwt.join [feed_promise; executions_promise]

(* Main run function that orchestrates all components *)
let run ~strategy ~router (cfg: config) (* Remove callbacks *) =
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine") "Starting engine..." >>= fun () ->
  
  (* Create the ring buffers *)
  let tick_buffer = Ringbuffer.create 1024 in (* Adjust capacity as needed *)
  let exec_buffer = Ringbuffer.create 1024 in (* Adjust capacity as needed *)
  let cmd_buffer  = Ringbuffer.create 1024 in (* Adjust capacity as needed *)

  (* Start all components via the supervisor *)
  Supervisor.start 
    ~feed:(start_feed cfg tick_buffer exec_buffer) (* Pass buffers to feed starter *)
    ~strategy
    ~router
    ~tick_buffer (* Pass buffers to supervisor *)
    ~exec_buffer
    ~cmd_buffer
    cfg >>= fun () ->
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine") "Engine started successfully"
