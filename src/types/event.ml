(* src/types/event.ml *)
open Primitives

(** Which exchange produced this event *)
type exchange = string
[@@deriving yojson { exn = true }]

(** Unified market tick *)
type tick = {
  src    : exchange;    (* e.g. "kraken" *)
  symbol : string;      (* normalized, e.g. "BTC/USD" *)
  bid    : Price.t;
  ask    : Price.t;
  ts     : timestamp;   (* µs since epoch *)
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
