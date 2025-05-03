(* src/exchange/kraken/ws_feed.ml *)

open Lwt.Infix
open Websocket
open Types
open Lwt.Syntax
module Json = Yojson.Safe
module JsonUtil = Yojson.Safe.Util

(* Add at the top of the file, after module imports *)
let snapshot_processed, resolve_snapshot_processed = Lwt.task ()
let instruments_loaded, resolve_instruments_loaded = Lwt.task () (* New promise for instruments *)

(* Flags to track snapshot completion *)
let executions_snapshot_done = ref false
let open_orders_snapshot_done = ref false

(* Function to check and resolve the main snapshot promise *) 
let check_and_resolve_snapshot_promise () =
  let section = Lwt_log_core.Section.make "kraken_ws_feed" in (* Define section here *) 
  if !executions_snapshot_done && !open_orders_snapshot_done then (
    if Lwt.state snapshot_processed = Lwt.Sleep then (
      Lwt_log_core.info ~section "Both execution and openOrders snapshots processed, resolving main snapshot promise" >>= fun () ->
      Lwt.wakeup_later resolve_snapshot_processed ();
      Lwt.return_unit
    ) else Lwt.return_unit (* Already resolved *)
  ) else Lwt.return_unit (* Not all snapshots done yet *)

(* Expose a function to get the snapshot promise *)
let wait_for_snapshot () = snapshot_processed
(* Expose a function to get the instruments promise *)
let wait_for_instruments () = instruments_loaded

(* Storage for instrument precisions: symbol -> (price_precision, qty_precision) *)
let instrument_precisions : (string, (int * int)) Hashtbl.t = Hashtbl.create 16

(* Getter for instrument precisions *)
let get_precisions symbol : (int * int) option = Hashtbl.find_opt instrument_precisions symbol

(* NEW Getter specifically for price precision *)
let get_price_precision symbol : int option =
  match Hashtbl.find_opt instrument_precisions symbol with
  | Some (price_prec, _) -> Some price_prec
  | None -> None

(* Configuration type - REMOVED, using Core.config *)
(* Order Tracking definitions moved below the 'order' type definition *)

(* Type Definitions for Kraken WS v2 API *)

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

(* Utility Functions *)
let float_to_price ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Primitives.Price.of_string_exn ~scale s

let float_to_qty ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Primitives.Qty.of_string_exn ~scale s

let section = Lwt_log_core.Section.make "kraken_ws_feed"

let safe_string json key default = JsonUtil.(member key json |> to_string_option |> Option.value ~default)
let safe_float json key default = JsonUtil.(member key json |> to_float_option |> Option.value ~default)
let debug_log msg = Lwt_log_core.debug ~section msg

(* Order Side Parsing *)
let parse_order_side = function
  | "buy" -> Some Core.Buy
  | "sell" -> Some Core.Sell
  | _ -> None


type order = {
  order_id : string;
  client_id : string option; (* Mapped from userref *)
  order_symbol : string;
  side : Core.side option;
  status : Core.order_state;
  limit_price : float;
  qty: float; (* Mapped from vol *)
}

(* Order Tracking - Define Hashtables after 'order' type *)
let open_buy_orders : (string, order) Hashtbl.t = Hashtbl.create 16
let pending_orders : (string, order) Hashtbl.t = Hashtbl.create 16

let format_order_log order action =
  Printf.sprintf "[ORDER %s] ID: %s, Symbol: %s, Side: %s, Status: %s, Price: %.8f"
    action order.order_id order.order_symbol
    (match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown")
    (match order.status with (* Updated match for Core.order_state - removed redundant case *)
     | Core.Open -> "Open"
     | Core.Filled -> "Filled"
     | Core.Canceled -> "Canceled"
     | Core.Rejected -> "Rejected" 
     (* Add other Core.order_state variants if they exist and need specific strings *)
     (* | Core.Expired -> "Expired"  <- Assuming Expired exists *)
     (* | Core.Pending -> "Pending" <- Assuming Pending exists *)
    )
    order.limit_price

let log_open_orders () =
  let orders = Hashtbl.to_seq_values open_buy_orders |> List.of_seq in
  debug_log (Printf.sprintf "Open orders (%d):" (List.length orders)) >>= fun () ->
  Lwt_list.iter_s (fun (order: order) ->
    debug_log (format_order_log order "OPEN")
  ) orders

let handle_order_cancellation order_id symbol =
  (* Placeholder for potential cancellation logic specific to your application *)
  debug_log (Printf.sprintf "[ORDER CANCELLATION] Handling cancellation for %s %s" order_id symbol)

(* Conversion Helpers *)
let kraken_ts_to_core_ts s =
  (* Parse ISO 8601 / RFC 3339 timestamp manually since we don't have Ptime *)
  try
    (* Format: "2023-01-01T12:34:56.789Z" *)
    let len = String.length s in
    if len < 20 then raise (Invalid_argument "Timestamp too short");
    
    (* Extract date parts *)
    let year = int_of_string (String.sub s 0 4) in
    let month = int_of_string (String.sub s 5 2) in
    let day = int_of_string (String.sub s 8 2) in
    
    (* Extract time parts *)
    let hour = int_of_string (String.sub s 11 2) in
    let minute = int_of_string (String.sub s 14 2) in
    let sec = int_of_string (String.sub s 17 2) in
    
    (* Extract milliseconds if present *)
    let ms = 
      if len > 20 && s.[19] = '.' then
        let ms_str = String.sub s 20 (min (len - 21) 3) in
        float_of_string ("0." ^ ms_str)
      else 0.0
    in
    
    (* Convert to Unix timestamp *)
    let tm = Unix.{ tm_year = year - 1900; tm_mon = month - 1; tm_mday = day;
                    tm_hour = hour; tm_min = minute; tm_sec = sec;
                    tm_wday = 0; tm_yday = 0; tm_isdst = false } in
    let unix_time = Unix.mktime tm |> fst in
    (unix_time +. ms) *. 1_000_000. |> Int64.of_float
  with e ->
    Lwt_log_core.warning ~section (Printf.sprintf "Failed to parse timestamp: %s, using current time: %s" s (Printexc.to_string e)) |> ignore;
    Unix.gettimeofday () *. 1_000_000. |> Int64.of_float

let kraken_side_to_core_side = function
  | Some "buy" -> Some Core.Buy
  | Some "sell" -> Some Core.Sell
  | _ -> None

let kraken_status_to_core_state status : Core.order_state = (* Explicit return type *)
  match status with
  | "new" | "pending_new" | "amended" | "restated" | "status" -> Open
  | "filled" -> Filled
  | "canceled" | "expired" -> Canceled
  | "rejected" -> Rejected
  | _ ->
      Lwt_log_core.warning ~section (Printf.sprintf "Unhandled Kraken order status: %s, mapping to Rejected" status) |> ignore;
      Rejected

let execution_report_to_market_event (report : execution_report) : Core.market_event option =
  let order_id = report.order_id in
  let client_id = "kraken:" ^ order_id in
  let ts = kraken_ts_to_core_ts report.timestamp in
  let state = kraken_status_to_core_state report.order_status in
  let symbol_opt = report.symbol in

  match report.exec_type, report.last_qty, report.last_price, kraken_side_to_core_side report.side, symbol_opt with
  | ("trade" | "filled"), Some qty_f, Some price_f, Some side, Some symbol when qty_f > 0.0 ->
      begin try
        let price = float_to_price ~scale:8 price_f in
        let qty = float_to_qty ~scale:8 qty_f in
        Some (Core.Fill { symbol; order_id; client_id; price; qty; side; ts })
      with ex ->
        Lwt_log_core.error ~section (Printf.sprintf "Failed to convert Fill data for order %s: %s" order_id (Printexc.to_string ex)) |> ignore;
        Some (Core.Ack { order_id; client_id; state; ts })
      end
  | _ ->
      Some (Core.Ack { order_id; client_id; state; ts })

(* Connection Setup *)
let connect (cfg : Core.config) is_auth =
  let port = cfg.ws_port in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in

  if is_auth then
    (* Authenticated connection logic based on provided example *) 
    let host = "ws-auth.kraken.com" in
    let uri = Uri.of_string (Printf.sprintf "wss://%s:%d/v2" host port) in (* Use /v2 path *)
    let tls_config = `Hostname host, `IP (Ipaddr.of_string_exn "104.16.248.94"), `Port port in
    let endpoint = `TLS tls_config in
    Websocket_lwt_unix.connect ~ctx endpoint uri
  else 
    (* Public connection logic (existing) *) 
    let host = cfg.ws_host in
    let path = "/v2" in
    let uri = Uri.of_string (Printf.sprintf "wss://%s:%d%s" host port path) in
    Lwt_unix.getaddrinfo host (string_of_int port) [Unix.(AI_FAMILY PF_INET)] >>= fun addrs ->
    match addrs with
    | { Unix.ai_addr = Unix.ADDR_INET (ip_addr_from_dns, _); _ } :: _ ->
        let ip_to_use = Ipaddr.of_string_exn (Unix.string_of_inet_addr ip_addr_from_dns) in
        let tls_config = `Hostname host, `IP ip_to_use, `Port port in
        let endpoint = `TLS tls_config in
        Websocket_lwt_unix.connect ~ctx endpoint uri
    | _ -> Lwt.fail_with "Failed to resolve public host" 

(* Custom Yojson converter for channel_params *)
let custom_channel_params_to_yojson = function
  | Ticker { symbol; snapshot; event_trigger } ->
      `Assoc (
        [("channel", `String "ticker"); ("symbol", `List (List.map (fun s -> `String s) symbol)); ("snapshot", `Bool snapshot)] @
        (match event_trigger with
        | None -> []
        | Some trigger -> [("event_trigger", `String trigger)])
      )
  | Executions { snap_trades; snap_orders; order_status; ratecounter; token } ->
      `Assoc (
        [("channel", `String "executions"); ("snap_trades", `Bool snap_trades); ("snap_orders", `Bool snap_orders);
         ("order_status", `Bool order_status); ("ratecounter", `Bool ratecounter); ("token", `String token)]
      )
  | Instrument { snapshot } ->
      `Assoc [("channel", `String "instrument"); ("snapshot", `Bool snapshot)]

(* Custom Yojson converter for subscribe_message *)
let custom_subscribe_message_to_yojson (msg : subscribe_message) : Json.t =
  `Assoc (
    [("method", `String msg.method_); ("params", custom_channel_params_to_yojson msg.params)] @
    (match msg.req_id with None -> [] | Some id -> [("req_id", `Int id)])
  )

(* Subscription Messages *)
let make_subscribe_message ?req_id (cfg : Core.config) channel =
  let params = match channel with
    | `Ticker -> 
        Ticker {
          symbol = cfg.symbols;
          snapshot = true;
          event_trigger = Some "trades";
        }
    | `Executions ->
        Executions {
          snap_trades = false;
          snap_orders = true;
          order_status = true;
          ratecounter = false;
          token = Option.get cfg.auth_token;
        }
    | `Instrument ->
        Instrument {
          snapshot = true;
        }
  in
  let msg = {
    method_ = "subscribe";
    params;
    req_id;
  } in
  let content = custom_subscribe_message_to_yojson msg |> Json.to_string in
  Frame.create ~content ()


let handle_public_frame conn (cfg : Core.config) frame ~on_tick =
  let section = Lwt_log_core.Section.make "kraken_ws_feed" in
  match frame.Websocket.Frame.opcode with
  | Frame.Opcode.Text ->
      Lwt.catch
        (fun () ->
          let json = Json.from_string frame.content in
          (* Check for 'method' field to identify subscription responses *)
          match JsonUtil.(member "method" json |> to_string_option) with
          | Some "subscribe" ->
              let success = JsonUtil.(member "success" json |> to_bool_option |> Option.value ~default:false) in
              let req_id = JsonUtil.(member "req_id" json |> to_int_option |> Option.map string_of_int |> Option.value ~default:"N/A") in
              let error = JsonUtil.(member "error" json |> to_string_option) in
              if success then
                let channel = JsonUtil.(member "result" json |> member "channel" |> to_string_option |> Option.value ~default:"unknown") in
                Lwt_log_core.debug ~section (Printf.sprintf "Subscription successful (req_id=%s, channel=%s)" req_id channel)
              else
                let error_msg = Option.value error ~default:"unknown error" in
                Lwt_log_core.error ~section (Printf.sprintf "Subscription failed (req_id=%s): %s. Payload: %s" req_id error_msg frame.content)
          | _ ->
              (* Handle data messages by channel *)
              match JsonUtil.(member "channel" json |> to_string_option) with
              | Some "ticker" ->
                  begin match ticker_response_of_yojson json with
                  | Ok { type_ = ("snapshot" | "update"); data = ticker_list; _ } ->
                      Lwt_list.iter_s
                        (fun (ticker : ticker_data) ->
                          let symbol = ticker.symbol in
                          let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
                          let bid_price = float_to_price ~scale:8 ticker.bid in
                          let ask_price = float_to_price ~scale:8 ticker.ask in
                          let current_price = Primitives.Price.midpoint bid_price ask_price in
                          let tick_event : Event.tick = {
                            src = "kraken";
                            symbol;
                            bid = bid_price;
                            ask = ask_price;
                            current_price;
                            ts;
                          } in
                          on_tick tick_event)
                        ticker_list
                  | Ok _ ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Unexpected ticker data format: %s" frame.content)
                  | Error err ->
                      Lwt_log_core.error ~section (Printf.sprintf "Failed to parse ticker: %s. Payload: %s" err frame.content)
                  end
              | Some "status" ->
                  begin match status_response_of_yojson json with
                  | Ok { data = [_status]; _ } ->
                      Lwt_log_core.debug ~section "Received valid status message"
                  | Ok _ ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Unexpected status data format: %s" frame.content)
                  | Error err ->
                      Lwt_log_core.error ~section (Printf.sprintf "Failed to parse status: %s. Payload: %s" err frame.content)
                  end
              | Some "heartbeat" ->
                  Lwt.return_unit
              | Some "instrument" ->
                  begin match instrument_response_of_yojson json with
                  | Ok { type_ = ("snapshot" | "update"); data = { pairs; _ }; _ } ->
                      Lwt_list.iter_s
                        (fun (pair : pair_data) ->
                          if List.mem pair.symbol cfg.symbols then
                            let () = Hashtbl.replace instrument_precisions pair.symbol (pair.price_precision, pair.qty_precision) in
                            Lwt_log_core.debug ~section
                              (Printf.sprintf "Stored precisions for %s: price=%d, qty=%d"
                                 pair.symbol pair.price_precision pair.qty_precision)
                          else
                            Lwt.return_unit)
                        pairs
                      >>= fun () ->
                      if Lwt.state instruments_loaded = Lwt.Sleep then
                        Lwt_log_core.info ~section "Instrument snapshot processed, resolving instruments promise" >>= fun () ->
                        Lwt.wakeup_later resolve_instruments_loaded ();
                        Lwt.return_unit
                      else
                        Lwt.return_unit
                  | Ok _ ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Unexpected instrument data format: %s" frame.content)
                  | Error err ->
                      Lwt_log_core.error ~section (Printf.sprintf "Failed to parse instrument data: %s. Payload: %s" err frame.content)
                  end
              | Some unknown_channel ->
                  Lwt_log_core.warning ~section
                    (Printf.sprintf "Received unhandled channel '%s': %s" unknown_channel frame.content)
              | None ->
                  Lwt_log_core.warning ~section
                    (Printf.sprintf "Missing or invalid channel in message: %s" frame.content)
        )
        (fun ex ->
          Lwt_log_core.error ~section
            (Printf.sprintf "Error processing text frame: %s. Payload: %s" (Printexc.to_string ex) frame.content)
          >>= fun () ->
          Lwt.return_unit)
  | Frame.Opcode.Ping ->
      Lwt_log_core.debug ~section "Received ping" >>= fun () ->
      Websocket_lwt_unix.write conn (Frame.create ~opcode:Frame.Opcode.Pong ())
  | Frame.Opcode.Pong ->
      Lwt_log_core.debug ~section "Received pong"
  | Frame.Opcode.Close ->
      Lwt_log_core.info ~section "Received close frame"
  | opcode ->
      Lwt_log_core.warning ~section
        (Printf.sprintf "Received unhandled opcode: %s" (Frame.Opcode.to_string opcode))



let handle_auth_frame conn (cfg: Core.config) frame ~on_execution =
  match frame.Websocket.Frame.opcode with
  | Frame.Opcode.Text ->
      let json = Json.from_string frame.content in
      Lwt.catch
        (fun () ->
          (* First, check if this is a subscription response by looking at the 'method' field *)
          match JsonUtil.(member "method" json |> to_string_option) with
          | Some "subscribe" ->
              (* Handle subscription response manually *)
              let success = JsonUtil.(member "success" json |> to_bool_option |> Option.value ~default:false) in
              let req_id_opt = JsonUtil.(member "req_id" json |> to_int_option) in
              let error_opt = JsonUtil.(member "error" json |> to_string_option) in
              let req_id_str = Option.map string_of_int req_id_opt |> Option.value ~default:"N/A" in

              (if not success then
                  begin match error_opt with
                  | Some err -> Lwt_log_core.error ~section (Printf.sprintf "Subscription failed (req_id=%s): %s" req_id_str err)
                  | None -> Lwt_log_core.error ~section (Printf.sprintf "Subscription failed (req_id=%s) with unknown error: %s" req_id_str frame.content)
                  end
               else
                 Lwt.return_unit
              ) >>= fun () ->
              Lwt.return_unit
          | _ ->
              (* Handle data messages by channel *)
              let channel_opt = JsonUtil.(member "channel" json |> to_string_option) in
              match channel_opt with
              | Some "executions" ->
                  let msg_type = JsonUtil.(member "type" json |> to_string_option) in
                  let data_json = JsonUtil.(member "data" json |> to_list) in
                  (* Process based on message type: snapshot or update *)
                  begin match msg_type with
                  | Some "snapshot" ->
                      (* Snapshot: Only update internal state, do NOT generate events *)
                      Lwt_list.iter_s (fun order_json ->
                          (* Manual Field Extraction *)
                          let order_id = safe_string order_json "order_id" "" in
                          let exec_type = safe_string order_json "exec_type" "" in
                          let order_status_str = safe_string order_json "order_status" "" in
                          let symbol_opt = JsonUtil.(member "symbol" order_json |> to_string_option) in
                          let side_str_opt = JsonUtil.(member "side" order_json |> to_string_option) in
                          let limit_price_opt = JsonUtil.(member "limit_price" order_json |> to_float_option) in
                          let last_price_opt = JsonUtil.(member "last_price" order_json |> to_float_option) in
                          let last_qty_opt = JsonUtil.(member "last_qty" order_json |> to_float_option) in
                          let _order_qty_opt = JsonUtil.(member "order_qty" order_json |> to_float_option) in (* Extract order_qty (unused here) *)
                          
                         
                          (* Internal State Update Logic *)
                          match exec_type with
                          | "canceled" ->
                              (match Hashtbl.find_opt open_buy_orders order_id with
                              | Some existing_order ->
                                  let symbol = existing_order.order_symbol in 
                                  debug_log (format_order_log existing_order "CANCELED (Snapshot)") >>= fun () ->
                                  handle_order_cancellation order_id symbol >>= fun () ->
                                  Hashtbl.remove open_buy_orders order_id;
                                  Hashtbl.remove pending_orders order_id; 
                                  log_open_orders ()
                              | None -> Lwt.return_unit)
                          | "filled" | "expired" ->
                              (match Hashtbl.find_opt open_buy_orders order_id with
                              | Some existing_order ->
                                  Hashtbl.remove open_buy_orders order_id;
                                  Hashtbl.remove pending_orders order_id;
                                  debug_log (format_order_log existing_order (exec_type ^ " (Snapshot)")) >>= fun () ->
                                  log_open_orders ()
                              | None -> Lwt.return_unit)
                          | _ ->
                              let symbol =
                                match exec_type, symbol_opt with
                                | "amended", _ ->
                                    (match Hashtbl.find_opt open_buy_orders order_id with 
                                    | Some o -> o.order_symbol 
                                    | None -> Option.value symbol_opt ~default:"")
                                | "new", _ ->
                                    (match Hashtbl.find_opt pending_orders order_id with 
                                    | Some o -> o.order_symbol 
                                    | None -> Option.value symbol_opt ~default:"")
                                | _, Some s -> s 
                                | _, None -> ""
                              in
                              let side_opt =
                                match exec_type with
                                | "amended" -> 
                                    (match Hashtbl.find_opt open_buy_orders order_id with 
                                    | Some o -> o.side 
                                    | None -> parse_order_side (Option.value side_str_opt ~default:""))
                                | "new" -> 
                                    (match Hashtbl.find_opt pending_orders order_id with 
                                    | Some o -> o.side 
                                    | None -> parse_order_side (Option.value side_str_opt ~default:""))
                                | _ -> parse_order_side (Option.value side_str_opt ~default:"")
                              in
                              match side_opt with
                              | Some Core.Buy when List.exists (fun s -> String.equal s symbol) cfg.symbols ->
                                  let status = kraken_status_to_core_state order_status_str in
                                  let limit_price = 
                                    match exec_type with
                                    | "new" -> 
                                        (match Hashtbl.find_opt pending_orders order_id with 
                                        | Some o -> o.limit_price 
                                        | None -> Option.value limit_price_opt ~default:0.0)
                                    | "amended" -> 
                                        (match Hashtbl.find_opt open_buy_orders order_id with 
                                        | Some o -> Option.value limit_price_opt ~default:o.limit_price 
                                        | None -> Option.value limit_price_opt ~default:0.0)
                                    | _ -> Option.value limit_price_opt ~default:0.0
                                  in
                                  let qty = (* Get quantity, prioritize existing if amending/new, else use snapshot *)
                                    match exec_type with
                                    | "new" ->
                                        (match Hashtbl.find_opt pending_orders order_id with
                                        | Some o -> o.qty (* Use qty from pending order *)
                                        | None -> Option.value _order_qty_opt ~default:0.0)
                                    | "amended" ->
                                        (match Hashtbl.find_opt open_buy_orders order_id with
                                        | Some o -> Option.value _order_qty_opt ~default:o.qty (* Use new qty, fallback to existing *)
                                        | None -> Option.value _order_qty_opt ~default:0.0)
                                    | _ -> Option.value _order_qty_opt ~default:0.0 (* Use snapshot qty for other types *)
                                  in
                                  let order = { 
                                    order_id; 
                                    client_id = None; (* FIXME: Parse userref? *) 
                                    order_symbol = symbol; 
                                    side = side_opt; 
                                    status; 
                                    limit_price;
                                    qty = qty; (* Use parsed/retrieved quantity *)
                                  } in
                                  let* log_msg_lwt = match exec_type with
                                    | "pending_new" -> 
                                        Hashtbl.replace pending_orders order_id order; 
                                        Lwt.return (format_order_log order "PENDING (Snapshot)")
                                    | "new" -> 
                                        Hashtbl.replace open_buy_orders order_id order; 
                                        Hashtbl.remove pending_orders order_id; 
                                        Lwt.return (format_order_log order "NEW (Snapshot)")
                                    | "trade" -> 
                                        let last_qty = Option.value last_qty_opt ~default:0.0 in 
                                        let last_price = Option.value last_price_opt ~default:0.0 in 
                                        Lwt.return (Printf.sprintf "[ORDER FILL (Snapshot)] %f %s at %.2f" last_qty order.order_symbol last_price)
                                    | "amended" ->
                                        Hashtbl.replace open_buy_orders order_id order;
                                        Lwt.return (format_order_log order "AMENDED (Snapshot)")
                                    | "restated" -> 
                                        Hashtbl.replace open_buy_orders order_id order; 
                                        Lwt.return (format_order_log order "STATUS (Snapshot)")
                                    | "status" -> 
                                        Hashtbl.replace open_buy_orders order_id order; 
                                        Lwt.return (format_order_log order "STATUS (Snapshot)")
                                    | _ -> 
                                        Lwt.return (format_order_log order ("UPDATE (Snapshot):" ^ exec_type))
                                  in
                                  debug_log log_msg_lwt >>= fun () ->
                                  if List.mem exec_type ["new"; "amended"; "restated"] then log_open_orders () else Lwt.return_unit
                              | _ -> Lwt.return_unit (* Skip non-buy/untracked or unknown side *)
                      ) data_json >>= fun () ->
                      (* Signal that snapshot is processed *)
                      Lwt_log_core.info ~section "Execution snapshot processed, resolving snapshot promise" >>= fun () ->
                      Lwt.wakeup_later resolve_snapshot_processed ();
                      Lwt.return_unit
                  | Some "update" ->
                      (* Update: Generate events AND update internal state *)
                      (* Step 1: Generate events *)
                      let market_events = List.filter_map (fun order_json ->
                          (* Manual Field Extraction *)
                          let order_id = safe_string order_json "order_id" "" in
                          let exec_type = safe_string order_json "exec_type" "" in
                          let order_status_str = safe_string order_json "order_status" "" in
                          let symbol_opt = JsonUtil.(member "symbol" order_json |> to_string_option) in
                          let side_str_opt = JsonUtil.(member "side" order_json |> to_string_option) in
                          let last_price_opt = JsonUtil.(member "last_price" order_json |> to_float_option) in
                          let last_qty_opt = JsonUtil.(member "last_qty" order_json |> to_float_option) in
                          let _order_qty_opt = JsonUtil.(member "order_qty" order_json |> to_float_option) in (* Extract order_qty (unused here) *)
                          let timestamp_str = safe_string order_json "timestamp" "" in
                          
                          (* Log extracted data for update item event generation *)
                          debug_log (Printf.sprintf "[EventGenUpdate] ID: %s, Type: %s, Status: %s, Symbol: %s, Side: %s, LastQty: %s, LastPx: %s"
                            order_id exec_type order_status_str 
                            (Option.value symbol_opt ~default:"N/A") 
                            (Option.value side_str_opt ~default:"N/A")
                            (Option.map string_of_float last_qty_opt |> Option.value ~default:"N/A")
                            (Option.map string_of_float last_price_opt |> Option.value ~default:"N/A")) |> Lwt.ignore_result;
                          
                          (* Market Event Generation Logic *)
                          let client_id = "kraken:" ^ order_id in
                          let ts = kraken_ts_to_core_ts timestamp_str in
                          let state = kraken_status_to_core_state order_status_str in
                          match exec_type, last_qty_opt, last_price_opt, kraken_side_to_core_side side_str_opt, symbol_opt with
                          | ("trade" | "filled"), Some qty_f, Some price_f, Some side, Some symbol when qty_f > 0.0 ->
                              (try 
                                 Some (Core.Fill { symbol; order_id; client_id; price=(float_to_price ~scale:8 price_f); qty=(float_to_qty ~scale:8 qty_f); side; ts }) 
                               with ex -> 
                                 Lwt_log_core.error ~section (Printf.sprintf "Failed converting Fill data for update %s: %s" order_id (Printexc.to_string ex)) |> Lwt.ignore_result; 
                                 Some (Core.Ack { order_id; client_id; state; ts }))
                          | ("canceled" | "expired" | "rejected"), _, _, _, _ ->
                              Some (Core.Ack { order_id; client_id; state; ts }) (* Generate Ack for terminal states *)
                          | _ -> None (* Ignore other exec_types for event generation *)
                      ) data_json in
                      
                      (* Step 2: Call on_execution if events were generated *)
                      let* () = 
                        if market_events <> [] then (
                          debug_log (Printf.sprintf "Calling on_execution with %d events from update" (List.length market_events)) >>= fun () ->
                          on_execution market_events
                        ) else (
                          debug_log (Printf.sprintf "No market events generated from update message.")
                        )
                      in
                      
                      (* Step 3: Update Internal State *)
                      Lwt_list.iter_s (fun order_json ->
                          (* Manual Field Extraction *)
                          let order_id = safe_string order_json "order_id" "" in
                          let exec_type = safe_string order_json "exec_type" "" in
                          let order_status_str = safe_string order_json "order_status" "" in
                          let symbol_opt = JsonUtil.(member "symbol" order_json |> to_string_option) in
                          let side_str_opt = JsonUtil.(member "side" order_json |> to_string_option) in
                          let limit_price_opt = JsonUtil.(member "limit_price" order_json |> to_float_option) in
                          let last_price_opt = JsonUtil.(member "last_price" order_json |> to_float_option) in
                          let last_qty_opt = JsonUtil.(member "last_qty" order_json |> to_float_option) in
                          let _order_qty_opt = JsonUtil.(member "order_qty" order_json |> to_float_option) in (* Extract order_qty (unused here) *)
                          
                          (* Log extracted data for state update from update item *)
                          debug_log (Printf.sprintf "[StateUpdateFromUpdate] ID: %s, Type: %s, Status: %s, Symbol: %s, Side: %s"
                            order_id exec_type order_status_str 
                            (Option.value symbol_opt ~default:"N/A") 
                            (Option.value side_str_opt ~default:"N/A")) >>= fun () ->
                          
                          (* Internal State Update Logic *)
                          match exec_type with
                          | "canceled" ->
                              (match Hashtbl.find_opt open_buy_orders order_id with
                              | Some existing_order ->
                                  let symbol = existing_order.order_symbol in 
                                  debug_log (format_order_log existing_order "CANCELED") >>= fun () ->
                                  handle_order_cancellation order_id symbol >>= fun () ->
                                  Hashtbl.remove open_buy_orders order_id;
                                  Hashtbl.remove pending_orders order_id; 
                                  log_open_orders ()
                              | None -> Lwt.return_unit)
                          | "filled" | "expired" ->
                              (match Hashtbl.find_opt open_buy_orders order_id with
                              | Some existing_order ->
                                  Hashtbl.remove open_buy_orders order_id;
                                  Hashtbl.remove pending_orders order_id;
                                  debug_log (format_order_log existing_order exec_type) >>= fun () ->
                                  log_open_orders ()
                              | None -> Lwt.return_unit)
                          | _ ->
                              let symbol =
                                match exec_type, symbol_opt with
                                | "amended", _ -> 
                                    (match Hashtbl.find_opt open_buy_orders order_id with 
                                    | Some o -> o.order_symbol 
                                    | None -> Option.value symbol_opt ~default:"")
                                | "new", _ -> 
                                    (match Hashtbl.find_opt pending_orders order_id with 
                                    | Some o -> o.order_symbol 
                                    | None -> Option.value symbol_opt ~default:"")
                                | _, Some s -> s 
                                | _, None -> ""
                              in
                              let side_opt =
                                match exec_type with
                                | "amended" -> 
                                    (match Hashtbl.find_opt open_buy_orders order_id with 
                                    | Some o -> o.side 
                                    | None -> parse_order_side (Option.value side_str_opt ~default:""))
                                | "new" -> 
                                    (match Hashtbl.find_opt pending_orders order_id with 
                                    | Some o -> o.side 
                                    | None -> parse_order_side (Option.value side_str_opt ~default:""))
                                | _ -> parse_order_side (Option.value side_str_opt ~default:"")
                              in
                              match side_opt with
                              | Some Buy when List.exists (fun s -> String.equal s symbol) cfg.symbols ->
                                  let status = kraken_status_to_core_state order_status_str in
                                  let limit_price = 
                                    match exec_type with
                                    | "new" -> 
                                        (match Hashtbl.find_opt pending_orders order_id with 
                                        | Some o -> o.limit_price 
                                        | None -> Option.value limit_price_opt ~default:0.0)
                                    | "amended" -> 
                                        (match Hashtbl.find_opt open_buy_orders order_id with 
                                        | Some o -> Option.value limit_price_opt ~default:o.limit_price 
                                        | None -> Option.value limit_price_opt ~default:0.0)
                                    | _ -> Option.value limit_price_opt ~default:0.0
                                  in
                                  let qty = (* Get quantity, prioritize existing if amending/new, else use update *)
                                    match exec_type with
                                    | "new" ->
                                        (match Hashtbl.find_opt pending_orders order_id with
                                        | Some o -> o.qty (* Use qty from pending order *)
                                        | None -> Option.value _order_qty_opt ~default:0.0)
                                    | "amended" ->
                                        (match Hashtbl.find_opt open_buy_orders order_id with
                                        | Some o -> Option.value _order_qty_opt ~default:o.qty (* Use new qty, fallback to existing *)
                                        | None -> Option.value _order_qty_opt ~default:0.0)
                                    | _ -> Option.value _order_qty_opt ~default:0.0 (* Use update qty for other types *)
                                  in
                                  let order = { 
                                    order_id; 
                                    client_id = None; (* FIXME: Parse userref? *) 
                                    order_symbol = symbol; 
                                    side = side_opt; 
                                    status; 
                                    limit_price;
                                    qty = qty; (* Use parsed/retrieved quantity *)
                                  } in
                                  let* log_msg_lwt = match exec_type with
                                    | "pending_new" -> 
                                        Hashtbl.replace pending_orders order_id order; 
                                        Lwt.return (format_order_log order "PENDING")
                                    | "new" -> 
                                        Hashtbl.replace open_buy_orders order_id order; 
                                        Hashtbl.remove pending_orders order_id; 
                                        Lwt.return (format_order_log order "NEW")
                                    | "trade" -> 
                                        let last_qty = Option.value last_qty_opt ~default:0.0 in 
                                        let last_price = Option.value last_price_opt ~default:0.0 in 
                                        Lwt.return (Printf.sprintf "[ORDER FILL] %f %s at %.2f" last_qty order.order_symbol last_price)
                                    | "amended" ->
                                        let existing_order_opt = Hashtbl.find_opt open_buy_orders order_id in
                                        debug_log (Printf.sprintf "[CACHE UPDATE] %s Before amendment - Order %s: %.8f" 
                                          symbol order_id (match existing_order_opt with None -> 0.0 | Some o -> o.limit_price)) >>= fun () ->
                                        Hashtbl.replace open_buy_orders order_id order;
                                        debug_log (Printf.sprintf "[CACHE UPDATE] After amendment - Order %s: %.8f" 
                                          order_id order.limit_price) >>= fun () ->
                                        (match Hashtbl.find_opt open_buy_orders order_id with 
                                        | Some vo -> debug_log (Printf.sprintf "[CACHE VERIFY] Order %s price %.8f" vo.order_id vo.limit_price) 
                                        | None -> debug_log (Printf.sprintf "[CACHE ERROR] Order %s not found after update" order_id)) >>= fun () ->
                                        Lwt.return (format_order_log order "AMENDED")
                                    | "restated" -> 
                                        Hashtbl.replace open_buy_orders order_id order; 
                                        let reason = safe_string order_json "reason" "unknown" in 
                                        Lwt.return (Printf.sprintf "[ORDER RESTATED] %s: %s" order_id reason)
                                    | "status" -> 
                                        Hashtbl.replace open_buy_orders order_id order; 
                                        Lwt.return (format_order_log order "STATUS")
                                    | _ -> 
                                        Lwt.return (format_order_log order ("UPDATE:" ^ exec_type))
                                  in
                                  debug_log log_msg_lwt >>= fun () ->
                                  if List.mem exec_type ["new"; "amended"; "restated"] then log_open_orders () else Lwt.return_unit
                              | _ -> Lwt.return_unit (* Skip non-buy/untracked or unknown side *)
                      ) data_json
                  | Some other_type ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Unknown execution message type: %s" other_type)
                  | None ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Message type is missing in execution message")
                  end
              | Some "status" ->
                  begin match status_response_of_yojson json with
                  | Ok { data = [_status]; _ } ->
                      Lwt.return_unit
                  | Ok _ ->
                      Lwt_log_core.warning ~section ("Unexpected status data format: " ^ frame.content)
                  | Error err ->
                      Lwt_log_core.error ~section (Printf.sprintf "Failed to parse status: %s" err)
                  end
              | Some "heartbeat" ->
                  Lwt.return_unit
              | Some "" | None ->
                  Lwt_log_core.warning ~section (Printf.sprintf "Auth message with empty or missing channel: %s" frame.content)
              | Some other_channel ->
                  Lwt_log_core.warning ~section (Printf.sprintf "Unhandled public channel: %s. Content: %s" other_channel frame.content)
        )
        (fun ex ->
          Lwt_log_core.error ~section
            (Printf.sprintf "Exception in auth handler: %s" (Printexc.to_string ex)) >>= fun () ->
          Lwt.return_unit
        )
  | Frame.Opcode.Ping ->
      Websocket_lwt_unix.write conn (Frame.create ~opcode:Frame.Opcode.Pong ())
  | Frame.Opcode.Close ->
      Lwt_log_core.info ~section "Received Close frame" >>= fun () ->
      Lwt.return_unit
  | Frame.Opcode.Pong ->
      Lwt_log_core.debug ~section "Received Pong frame" >>= fun () ->
      Lwt.return_unit
  | _ ->
      Lwt_log_core.warning ~section 
        (Printf.sprintf "Unhandled frame opcode: %s" 
           (Frame.Opcode.to_string frame.Websocket.Frame.opcode)) >>= fun () ->
      Lwt.return_unit

(* Getter for open orders (used by Strategy) - Updated Type *)
let get_open_buy_orders () : (string, order) Hashtbl.t = open_buy_orders

(* Main Feed Functions *)
let start (cfg : Core.config) ~on_tick =
  let rec loop conn =
    Websocket_lwt_unix.read conn >>= fun frame ->
    handle_public_frame conn cfg frame ~on_tick >>= fun () ->
    loop conn
  in
  connect cfg false >>= fun conn ->
  let subscribe_ticker_msg = make_subscribe_message ~req_id:1 cfg `Ticker in
  let subscribe_instrument_msg = make_subscribe_message ~req_id:3 cfg `Instrument in (* New subscription *)
  Websocket_lwt_unix.write conn subscribe_ticker_msg >>= fun () ->
  Websocket_lwt_unix.write conn subscribe_instrument_msg >>= fun () -> (* Send instrument subscription *)
  loop conn

let start_executions (cfg : Core.config) ~on_execution =
  let section = Lwt_log_core.Section.make "kraken_ws_auth" in
  match cfg.auth_token with
  | None -> Lwt.fail_with "Authentication token required for executions feed"
  | Some _ ->
      let rec loop conn =
        Websocket_lwt_unix.read conn >>= fun frame ->
        Lwt.catch 
          (fun () -> 
            handle_auth_frame conn cfg frame ~on_execution (* Pass cfg here *)
          )
          (fun ex -> 
            Lwt_log_core.error_f ~section "Error reading/handling auth frame: %s" (Printexc.to_string ex) >>= fun () ->
            Lwt.fail ex
          ) >>= fun () ->
        loop conn
      in
      Lwt.catch 
        (fun () -> 
          Lwt_log_core.info ~section "Attempting to connect to auth endpoint..." >>= fun () ->
          connect cfg true >>= fun conn ->
          Lwt_log_core.info ~section "Successfully connected to auth endpoint." >>= fun () ->
          let subscribe_msg = make_subscribe_message ~req_id:2 cfg `Executions in
          Lwt_log_core.info ~section "Subscribing to executions feed" >>= fun () ->
          Lwt_log_core.debug ~section (Printf.sprintf "Sending executions subscribe message: %s" subscribe_msg.content) >>= fun () ->
          Websocket_lwt_unix.write conn subscribe_msg >>= fun () ->
          Lwt_log_core.info ~section "Starting auth message loop..." >>= fun () ->
          loop conn
        )
        (fun ex -> 
          Lwt_log_core.error_f ~section "Failed to connect/subscribe to auth endpoint: %s" (Printexc.to_string ex) >>= fun () ->
          Lwt.fail ex
        )
