(* src/engine/types_engine.ml *)
open Types (* Needed for Event.tick *)
open Types.Core (* Needed for order_cmd *)

(* Define the engine configuration structure HERE *)
type config = {
  ws_host: string;
  ws_port: int;
  ws_path: string;
  symbols: string list;
  auth_token: string option;
}

(* Type for streams *)
type 'a stream = unit -> 'a option
type tick_stream = Event.tick stream
type cmd_stream = order_cmd stream

(* Type for strategy component - Takes tick buffer, outputs to cmd buffer *)
type strategy = {
  start: config -> tick_buffer:Event.tick Ringbuffer.t -> cmd_buffer:order_cmd Ringbuffer.t -> unit Lwt.t;
}

(* Type for router component - Takes cmd buffer and exec buffer *)
type router = {
  start: config -> cmd_buffer:order_cmd Ringbuffer.t -> exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
} 