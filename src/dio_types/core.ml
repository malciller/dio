(* src/types/core.ml *)

open Primitives

type side  = Buy | Sell                 [@@deriving yojson]
type tif   = GTC | IOC | FOK            [@@deriving yojson]

type order_tag = [`Grid | `Manual | `Rebalance] [@@deriving yojson]

type order_cmd =
  | Add of {
      dst        : Event.exchange;    
      client_id  : string;
      symbol     : symbol;
      side       : side;
      price      : Price.t;
      qty        : Qty.t;
      tif        : tif;
      tags       : order_tag list;
    }
  | Amend of {
      dst : Event.exchange;
      order_id : string;
      symbol : symbol;
      new_price : Price.t;
      new_qty : Qty.t;
      ts: timestamp;
    }
  | Cancel of { dst : Event.exchange; order_id : string }
[@@deriving yojson { exn = true }]


type order_state = Open | Filled | Canceled | Rejected [@@deriving yojson]

type market_event =
  | Book  of { symbol : symbol ; bid : Price.t ; ask : Price.t ; ts : timestamp }
  | Trade of { symbol : symbol ; price : Price.t ; qty : Qty.t ; side : side ; ts : timestamp }
  | Fill  of { symbol : symbol ; order_id : string ; client_id : string ;
               price : Price.t ; qty : Qty.t ; side : side ; ts : timestamp }
  | Ack   of { order_id : string ; client_id : string ; state : order_state ; ts : timestamp }
  | Heartbeat of timestamp
[@@deriving yojson]


(* strategy component *) 
type grid_strategy = {
  start: Config.runtime_cfg -> Config.engine_config -> tick_buffer:Event.tick Ringbuffer.t -> cmd_buffer:order_cmd Ringbuffer.t -> exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
}

(* orderbook strategy component *)
type orderbook_strategy = {
  start: Config.runtime_cfg -> Config.engine_config -> tick_buffer:Event.tick Ringbuffer.t -> cmd_buffer:order_cmd Ringbuffer.t -> exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
}

(* router component *) 
type router = {
  start: Config.engine_config -> cmd_buffer:order_cmd Ringbuffer.t -> exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
}

(* order operations *)
type order_response = {
  success: bool;
  error: string option;
  result: Yojson.Safe.t option;
} [@@deriving yojson]

