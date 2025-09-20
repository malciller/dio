(** Kraken Exchange WebSocket API Types and Utilities

    This module defines OCaml types for Kraken's WebSocket API v2, including
    subscription parameters, response messages, and order management structures.
    Provides nonce generation and request signing for authenticated operations.
*)

open Dio_types
open Lwt_log_core
open Lwt.Infix

let section = Section.make "kraken_nonce"

(** Channel subscription parameters for Kraken WebSocket feeds *)
type channel_params =
  | Ticker of {
      symbol: string list;
      snapshot: bool;
      event_trigger: string option;
    }
  | Executions of {
      snap_trades: bool;
      snap_orders: bool;
      order_status: bool;
      ratecounter: bool;
      token: string;
    }
  | Instrument of { 
      snapshot: bool;
    }
  | Book of {
      symbol: string list;
      depth: int;
      snapshot: bool;
    }
[@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** WebSocket subscription request message structure *)
type subscribe_message = {
  method_: string; [@key "method"]
  params: channel_params; [@key "params"]
  req_id: int option; [@key "req_id"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Market ticker data containing price and volume information *)
type ticker_data = {
  ask: float; [@key "ask"]
  bid: float; [@key "bid"]
  symbol: string; [@key "symbol"]
  ask_qty: float; [@key "ask_qty"]
  bid_qty: float; [@key "bid_qty"]
  change: float; [@key "change"]
  change_pct: float; [@key "change_pct"]
  high: float; [@key "high"]
  last: float; [@key "last"] 
  low: float; [@key "low"]
  volume: float; [@key "volume"]
  vwap: float; [@key "vwap"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** WebSocket message containing ticker data updates *)
type ticker_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: ticker_data list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Connection status information from Kraken WebSocket *)
type status_data = {
  version: string; [@key "version"]
  system: string; [@key "system"]
  api_version: string; [@key "api_version"]
  connection_id: string; [@of_yojson (function
    | `Intlit s | `String s -> Ok s
    | `Int i -> Ok (Int64.to_string (Int64.of_int i))
    | _ -> Error "status_data.connection_id: expected int or string")]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** WebSocket message containing connection status updates *)
type status_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: status_data list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Response to subscription requests indicating success/failure *)
type subscription_response = {
  method_: string; [@key "method"]
  req_id: int option; [@key "req_id"] [@yojson.option]
  result: Yojson.Safe.t option; [@key "result"] [@yojson.option]
  success: bool; [@key "success"]
  error: string option; [@key "error"] [@yojson.option]
  time_in: string; [@key "time_in"]
  time_out: string; [@key "time_out"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** WebSocket heartbeat message to maintain connection *)
type heartbeat_response = {
  channel: string; [@key "channel"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Trading asset information including precision and status *)
type asset_data = {
  id: string; [@key "id"]
  precision: int; [@key "precision"]
  precision_display: int; [@key "precision_display"]
  status: string; [@key "status"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Trading pair information with fee structure and precision settings *)
type pair_data = {
  symbol: string; [@key "symbol"]
  base: string; [@key "base"]
  quote: string; [@key "quote"]
  price_precision: int; [@key "price_precision"]
  qty_precision: int; [@key "qty_precision"]
  status: string; [@key "status"]
  maker_fee: float option; [@key "maker_fee"] [@yojson.optional] [@default None]
  taker_fee: float option; [@key "taker_fee"] [@yojson.optional] [@default None]
  fee_volume_currency: string option; [@key "fee_volume_currency"] [@yojson.optional] [@default None]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Container for instrument channel data including assets and trading pairs *)
type instrument_data = {
  assets: asset_data list; [@key "assets"]
  pairs: pair_data list; [@key "pairs"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** WebSocket message containing instrument data (assets and pairs) *)
type instrument_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: instrument_data; (* Note: data is an object here, not list *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Fee information for executed trades *)
type fee = {
  asset: string; [@key "asset"]
  qty: float; [@key "qty"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Contingent order parameters for conditional execution *)
type contingent = {
  order_type: string option; [@key "order_type"] [@yojson.option]
  trigger_price: float option; [@key "trigger_price"] [@yojson.option]
  trigger_price_type: string option; [@key "trigger_price_type"] [@yojson.option]
  limit_price: float option; [@key "limit_price"] [@yojson.option]
  limit_price_type: string option; [@key "limit_price_type"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Detailed report of order execution events *)
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

(** WebSocket message containing execution report updates *)
type executions_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: execution_report list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Parameters for placing new orders via Kraken WebSocket API v2 *)
type add_order_params = {
  order_type: string;
  side: string;
  order_qty: float;
  symbol: string;
  limit_price: float;
  time_in_force: string;
  post_only: bool;
  cl_ord_id: string;
} [@@deriving yojson]

(** Complete add order request message for WebSocket API *)
type add_order_request = {
  method_: string; [@key "method"]
  params: add_order_params;
  token: string;
  req_id: int option; [@yojson.option]
} [@@deriving yojson]

(** Parameters for amending existing orders via WebSocket API *)
type amend_order_params = {
  order_id: string;     
  order_qty: float;     
  limit_price: float;  
  post_only: bool;       
} [@@deriving yojson]

(** Complete amend order request message for WebSocket API *)
type amend_order_request = {
  method_: string; [@key "method"]
  params: amend_order_params;
  token: string;
  req_id: int option; [@yojson.option]
} [@@deriving yojson]

(** WebSocket connection state with request tracking and command queue *)
type state = {
  conn: Websocket_lwt_unix.conn;
  mutable next_req_id: int;
  mutable req_to_client: (int, string) Hashtbl.t;
  mutable response_promises: (int, Core.order_response Lwt.u) Hashtbl.t;
  cmd_queue: Core.order_cmd Queue.t;
  cmd_cond: unit Lwt_condition.t;
}

(** Internal representation of order state for tracking *)
type order = {
  order_id : string;
  client_id : string option; 
  order_symbol : string;
  side : Core.side option;
  status : Core.order_state;
  limit_price : float;
  qty: float;
}

(** Global mutable reference tracking the last used nonce for request authentication *)
let last_nonce =
  ref (Unix.gettimeofday () *. 1_000_000_000.0 |> Int64.of_float)

(** Mutex to synchronize nonce generation across concurrent requests *)
let nonce_mutex = Lwt_mutex.create ()

(** Generate monotonically increasing nonce for Kraken API authentication *)
let nonce () =
  Lwt_mutex.with_lock nonce_mutex (fun () ->
    let current_nanos = Unix.gettimeofday () *. 1_000_000_000.0 |> Int64.of_float in
    let old_last_nonce = !last_nonce in (* Capture old value *)
    let next_nonce = Int64.max current_nanos (Int64.add old_last_nonce 1L) in
    last_nonce := next_nonce;
    Lwt_log_core.debug ~section (Printf.sprintf "Generated Nonce: current_nanos=%Ld, old_last_nonce=%Ld, next_nonce=%Ld"
                                   current_nanos old_last_nonce next_nonce) >>= fun () ->
    Lwt.return (Int64.to_string !last_nonce)
  )

(** Generate HMAC-SHA512 signature for Kraken API authentication using secret key *)
let sign ~secret ~path ~body ~nonce =
  let payload = nonce ^ body in 
  let sha256_hash_raw = Digestif.SHA256.digest_string payload |> Digestif.SHA256.to_raw_string in
  let message_bytes = Bytes.cat (Bytes.of_string path) (Bytes.of_string sha256_hash_raw) in
  let decoded_secret = Base64.decode_exn secret in
  let hmac_hash_raw = Digestif.SHA512.hmac_bytes ~key:decoded_secret message_bytes |> Digestif.SHA512.to_raw_string in
  Base64.encode_string hmac_hash_raw

(** Extract a field from a JSON object by path/key *)
let parse_json_field json (path : string) =
  Yojson.Safe.Util.member path json
