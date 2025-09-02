(* src/types/event.ml *)

open Primitives

(** Which exchange produced this event *)
type exchange = string
[@@deriving yojson { exn = true }]

(** Unified market tick *)
type tick = {
  src    : exchange;    
  symbol : string;     
  bid    : Price.t;
  ask    : Price.t;
  current_price : Price.t; (* Midpoint of bid/ask *)
  ts     : timestamp;   (* µs since epoch *)
  ask_qty: float;
  bid_qty: float;
  change: float;
  change_pct: float;
  high: float;
  last_price: float; 
  low: float;
  volume: float;
  vwap: float;
}
[@@deriving yojson { exn = true }]

(** Unified fill report *)
type fill = {
  src       : exchange;
  symbol    : string;
  order_id  : string;
  side      : [ `Buy | `Sell ];
  qty       : Qty.t;
  price     : Price.t;
  ts        : timestamp;
}
[@@deriving yojson { exn = true }]
