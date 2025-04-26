(* Kraken WebSocket Protocol Types *)

(* Ensure necessary modules are available if types depend on them, e.g., Yojson.Safe *)
module Json = Yojson.Safe

(* Ticker Data Response *)
type ticker_data = {
  ask: float; [@key "ask"]
  bid: float; [@key "bid"]
  symbol: string; [@key "symbol"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type ticker_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: ticker_data list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Status Update *)
type status_data = {
  version: string; [@key "version"]
  system: string; [@key "system"]
  api_version: string; [@key "api_version"]
  connection_id: string; [@of_yojson (function
    | `Intlit s | `String s -> Ok s
    | `Int i -> Ok (Int64.to_string (Int64.of_int i))
    | _ -> Error "status_data.connection_id: expected int or string")]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type status_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: status_data list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Subscription Acknowledgment *)
type subscription_response = {
  method_: string; [@key "method"]
  req_id: int option; [@key "req_id"] [@yojson.option]
  result: Yojson.Safe.t option; [@key "result"] [@yojson.option]
  success: bool; [@key "success"]
  error: string option; [@key "error"] [@yojson.option]
  time_in: string; [@key "time_in"]
  time_out: string; [@key "time_out"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Heartbeat *)
type heartbeat_response = {
  channel: string; [@key "channel"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* START: Instrument Channel Types *)
(* Instrument Asset Data *)
type asset_data = {
  id: string; [@key "id"]
  precision: int; [@key "precision"]
  precision_display: int; [@key "precision_display"]
  status: string; [@key "status"]
  (* Add other fields if needed *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Instrument Pair Data *)
type pair_data = {
  symbol: string; [@key "symbol"]
  base: string; [@key "base"]
  quote: string; [@key "quote"]
  price_precision: int; [@key "price_precision"]
  qty_precision: int; [@key "qty_precision"]
  status: string; [@key "status"]
  (* Add other fields if needed *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Instrument Channel Data Container *)
type instrument_data = {
  assets: asset_data list; [@key "assets"]
  pairs: pair_data list; [@key "pairs"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Instrument Channel Response *)
type instrument_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: instrument_data; (* Note: data is an object here, not list *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]
(* END: Instrument Channel Types *)

(* Execution Report related types *)
type fee = {
  asset: string; [@key "asset"]
  qty: float; [@key "qty"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type contingent = {
  order_type: string option; [@key "order_type"] [@yojson.option]
  trigger_price: float option; [@key "trigger_price"] [@yojson.option]
  trigger_price_type: string option; [@key "trigger_price_type"] [@yojson.option]
  limit_price: float option; [@key "limit_price"] [@yojson.option]
  limit_price_type: string option; [@key "limit_price_type"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type execution_report = {
  order_id: string; [@key "order_id"]
  exec_type: string; [@key "exec_type"]
  order_status: string; [@key "order_status"]
  side: string option; [@key "side"] [@yojson.option]
  symbol: string option; [@key "symbol"] [@yojson.option]
  limit_price: float option; [@key "limit_price"] [@yojson.option]
  order_qty: float option; [@key "order_qty"] [@yojson.option]
  last_price: float option; [@key "last_price"] [@yojson.option]
  last_qty: float option; [@key "last_qty"] [@yojson.option]
  timestamp: string; [@key "timestamp"]
  fees: fee list option; [@key "fees"] [@yojson.option]
  contingent: Yojson.Safe.t option; [@key "contingent"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type executions_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: execution_report list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields] 