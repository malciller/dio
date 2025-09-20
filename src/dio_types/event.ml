(** Trading event types for market data and order execution *)

open Primitives

(** Exchange identifier as string (e.g., "kraken", "binance") *)
type exchange = string
[@@deriving yojson { exn = true }]

(** Real-time market data snapshot with current bid/ask and 24h stats *)
type tick = {
  src    : exchange;           (** Source exchange *)
  symbol : string;             (** Trading pair (e.g., "BTC/USD") *)
  bid    : Price.t;            (** Best bid price *)
  ask    : Price.t;            (** Best ask price *)
  current_price : Price.t;     (** Midpoint: (bid + ask) / 2 *)
  ts     : timestamp;          (** Event timestamp in microseconds since epoch *)
  ask_qty: float;              (** Quantity available at ask price *)
  bid_qty: float;              (** Quantity available at bid price *)
  change: float;               (** 24h absolute price change *)
  change_pct: float;           (** 24h percentage price change *)
  high: float;                 (** 24h highest price *)
  last_price: float;           (** Last traded price *)
  low: float;                  (** 24h lowest price *)
  volume: float;               (** 24h trading volume *)
  vwap: float;                 (** 24h volume-weighted average price *)
}
[@@deriving yojson { exn = true }]

(** Order execution confirmation with fill details *)
type fill = {
  src       : exchange;        (** Exchange where fill occurred *)
  symbol    : string;          (** Trading pair *)
  order_id  : string;          (** Unique order identifier *)
  side      : [ `Buy | `Sell ]; (** Trade direction *)
  qty       : Qty.t;           (** Filled quantity *)
  price     : Price.t;         (** Execution price *)
  ts        : timestamp;       (** Fill timestamp in microseconds since epoch *)
}
[@@deriving yojson { exn = true }]
