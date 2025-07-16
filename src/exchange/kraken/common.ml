(* src/exchange/kraken/common.ml *)
open Dio_types


(* Channel-specific subscription parameters *)
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
  | Instrument of { (* New variant for instrument channel *)
      snapshot: bool;
    }
  | Book of {
      symbol: string list;
      depth: int;
      snapshot: bool;
    }
[@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Unified subscription message *)
type subscribe_message = {
  method_: string; [@key "method"]
  params: channel_params; [@key "params"]
  req_id: int option; [@key "req_id"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Ticker Data Response *)
type ticker_data = {
  ask: float; [@key "ask"]
  bid: float; [@key "bid"]
  symbol: string; [@key "symbol"]
  ask_qty: float; [@key "ask_qty"]
  bid_qty: float; [@key "bid_qty"]
  change: float; [@key "change"]
  change_pct: float; [@key "change_pct"]
  high: float; [@key "high"]
  last: float; [@key "last"] (* API uses 'last', will be mapped to 'last_price' in Event.tick *)
  low: float; [@key "low"]
  volume: float; [@key "volume"]
  vwap: float; [@key "vwap"]
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

(* Execution Report *)
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

(* WS_EXEC *)
(* Type definitions for Kraken WS v2 API messages *)
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

type add_order_request = {
  method_: string; [@key "method"]
  params: add_order_params;
  token: string;
  req_id: int option; [@yojson.option]
} [@@deriving yojson]

(* NEW: Types for Amend Order *) 
type amend_order_params = {
  order_id: string;      (* Kraken order ID *)
  order_qty: float;     (* New quantity *)
  limit_price: float;   (* New limit price *)
  post_only: bool;       (* Must be true *)
} [@@deriving yojson]

type amend_order_request = {
  method_: string; [@key "method"]
  params: amend_order_params;
  token: string;
  req_id: int option; [@yojson.option]
} [@@deriving yojson]

(* Connection state *)
type state = {
  conn: Websocket_lwt_unix.conn;
  mutable next_req_id: int;
  mutable req_to_client: (int, string) Hashtbl.t;
  mutable response_promises: (int, Core.order_response Lwt.u) Hashtbl.t;
  cmd_queue: Core.order_cmd Queue.t;
  cmd_cond: unit Lwt_condition.t;
}



type order = {
  order_id : string;
  client_id : string option; (* Mapped from userref *)
  order_symbol : string;
  side : Core.side option;
  status : Core.order_state;
  limit_price : float;
  qty: float; (* Mapped from vol *)
}

(* Global, mutable reference for the last used nonce.
   Initialize with current time in microseconds to ensure it's likely higher than
   any nonce from a previous session and provides good granularity.
   Using microseconds (Unix.time() *. 1_000_000.0) for initialization
   and incrementing ensures it's always increasing during a session.
*)
let last_nonce =
  ref (Unix.gettimeofday () *. 1_000_000.0 |> Int64.of_float)

let nonce () =
  last_nonce := Int64.add !last_nonce 1L; 
  Int64.to_string !last_nonce       

(* Kraken signing function *)
let sign ~secret ~path ~body ~nonce =
  let payload = nonce ^ body in 
  let sha256_hash_raw = Digestif.SHA256.digest_string payload |> Digestif.SHA256.to_raw_string in
  let message_bytes = Bytes.cat (Bytes.of_string path) (Bytes.of_string sha256_hash_raw) in
  let decoded_secret = Base64.decode_exn secret in
  let hmac_hash_raw = Digestif.SHA512.hmac_bytes ~key:decoded_secret message_bytes |> Digestif.SHA512.to_raw_string in
  Base64.encode_string hmac_hash_raw 

let parse_json_field json (path : string) =
  Yojson.Safe.Util.member path json