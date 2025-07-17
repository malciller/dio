(* src/types/config.ml *)
open Primitives

type strategy_type = 
  | Grid
  | Orderbook

let strategy_type_to_yojson = function
  | Grid -> `String "Grid"
  | Orderbook -> `String "Orderbook"

let strategy_type_of_yojson = function
  | `String "Grid" -> Ok Grid
  | `String "Orderbook" -> Ok Orderbook
  | _ -> Error "Invalid strategy type"

let strategy_type_of_yojson_exn json =
  match strategy_type_of_yojson json with
  | Ok v -> v
  | Error msg -> failwith msg

type asset_cfg = {
  symbol        : symbol;
  qty           : Qty.t;
  grid_interval : Fixed.t;
  sell_mult     : Fixed.t;
  strategy      : strategy_type;
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

