(* src/types/config.ml *)
open Primitives

type asset_cfg = {
  symbol        : symbol;
  qty           : Qty.t;
  grid_interval : Fixed.t;
  sell_mult     : Fixed.t;  
} [@@deriving yojson { exn = true }]   

type runtime_cfg = {
  assets      : asset_cfg list;
  debounce_ms : int;
  queues_cap  : int;
} [@@deriving yojson { exn = true }]

type engine_config = {
  ws_host: string;
  ws_port: int;
  ws_path: string;
  symbols: string list;
  auth_token: string option;
  kraken_api_key : string;
  kraken_api_secret : string;
}

