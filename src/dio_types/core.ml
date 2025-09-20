(** Core type definitions for trading system *)

open Primitives

(** Trading side: Buy or Sell order *)
type side = Buy | Sell [@@deriving yojson]

(** Time in force: Good Till Cancelled, Immediate or Cancel, Fill or Kill *)
type tif = GTC | IOC | FOK [@@deriving yojson]

(** Order categorization tags for tracking order origins *)
type order_tag = [`Grid | `Manual | `Rebalance] [@@deriving yojson]

(** Order command types for managing order lifecycle *)
type order_cmd =
  (** Place new order with full specifications *)
  | Add of {
      dst        : Event.exchange;    (** Target exchange *)
      client_id  : string;            (** Client identifier *)
      symbol     : symbol;            (** Trading pair *)
      side       : side;              (** Buy or Sell *)
      price      : Price.t;           (** Order price *)
      qty        : Qty.t;             (** Order quantity *)
      tif        : tif;               (** Time in force *)
      tags       : order_tag list;    (** Order categorization *)
    }
  (** Modify existing order price and/or quantity *)
  | Amend of {
      dst : Event.exchange;       (** Target exchange *)
      order_id : string;          (** Order to modify *)
      symbol : symbol;            (** Trading pair *)
      new_price : Price.t;        (** New price *)
      new_qty : Qty.t;            (** New quantity *)
      ts: timestamp;              (** Timestamp of amendment *)
    }
  (** Cancel existing order *)
  | Cancel of {
      dst : Event.exchange;       (** Target exchange *)
      order_id : string;          (** Order to cancel *)
    }
[@@deriving yojson { exn = true }]


(** Order lifecycle states *)
type order_state = Open | Filled | Canceled | Rejected [@@deriving yojson]

(** Market data and order execution events *)
type market_event =
  (** Order book update with best bid/ask prices *)
  | Book of {
      symbol : symbol;      (** Trading pair *)
      bid : Price.t;        (** Best bid price *)
      ask : Price.t;        (** Best ask price *)
      ts : timestamp;       (** Event timestamp *)
    }
  (** Market trade execution *)
  | Trade of {
      symbol : symbol;      (** Trading pair *)
      price : Price.t;      (** Trade price *)
      qty : Qty.t;          (** Trade quantity *)
      side : side;          (** Trade side *)
      ts : timestamp;       (** Trade timestamp *)
    }
  (** Order fill confirmation *)
  | Fill of {
      symbol : symbol;      (** Trading pair *)
      order_id : string;    (** Filled order ID *)
      client_id : string;   (** Client identifier *)
      price : Price.t;      (** Fill price *)
      qty : Qty.t;          (** Fill quantity *)
      side : side;          (** Fill side *)
      ts : timestamp;       (** Fill timestamp *)
    }
  (** Order acknowledgement with current state *)
  | Ack of {
      order_id : string;    (** Acknowledged order ID *)
      client_id : string;   (** Client identifier *)
      state : order_state;  (** Current order state *)
      ts : timestamp;       (** Acknowledgement timestamp *)
    }
  (** System heartbeat for connection monitoring *)
  | Heartbeat of timestamp  (** Heartbeat timestamp *)
[@@deriving yojson]


(** Grid trading strategy interface *)
type grid_strategy = {
  start: Config.runtime_cfg -> Config.engine_config ->
         tick_buffer:Event.tick Ringbuffer.t ->
         cmd_buffer:order_cmd Ringbuffer.t ->
         exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
}

(** Order book analysis strategy interface *)
type orderbook_strategy = {
  start: Config.runtime_cfg -> Config.engine_config ->
         tick_buffer:Event.tick Ringbuffer.t ->
         cmd_buffer:order_cmd Ringbuffer.t ->
         exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
}

(** Cross-exchange arbitrage strategy interface *)
type arbitrage_strategy = {
  start: Config.runtime_cfg -> Config.engine_config ->
         tick_buffer:Event.tick Ringbuffer.t ->
         cmd_buffer:order_cmd Ringbuffer.t ->
         exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
}

(** Order routing and execution interface *)
type router = {
  start: Config.engine_config ->
         cmd_buffer:order_cmd Ringbuffer.t ->
         exec_buffer:market_event Ringbuffer.t -> unit Lwt.t;
}

(** Response structure for order operations *)
type order_response = {
  success: bool;                    (** Operation success flag *)
  error: string option;             (** Error message if operation failed *)
  result: Yojson.Safe.t option;     (** Operation result data *)
} [@@deriving yojson]

