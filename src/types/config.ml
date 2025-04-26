(* src/types/config.ml *)
open Primitives

type asset_cfg = {
  symbol        : symbol;
  qty           : Qty.t;
  grid_interval : Decimal.t;
  sell_mult     : Decimal.t;  (* e.g. 0.999 to sell almost all *)
} [@@deriving yojson { exn = true }]   (* generates _to_yojson + _of_yojson_exn *)

type runtime_cfg = {
  assets      : asset_cfg list;
  debounce_ms : int;
  queues_cap  : int;
} [@@deriving yojson { exn = true }]
