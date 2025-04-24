open Lwt.Infix
open Websocket

module Json = Yojson.Safe

(* Type Definitions for Kraken WS v2 API *)

(* Order types *)
type order_side = Buy | Sell | Unknown

type order_status = 
  | PendingNew
  | New
  | PartiallyFilled
  | Filled
  | Canceled
  | Expired
  | Rejected
  | Unknown

type order_info = {
  order_id: string;
  order_symbol: string;
  side: order_side;
  status: order_status;
  limit_price: float;
}

(* Subscription Request *)
type subscribe_params = {
  channel : string; [@key "channel"]
  symbol : string list; [@key "symbol"]
  snapshot : bool; [@key "snapshot"] (* Default: true *)
  event_trigger : string option; [@key "event_trigger"] [@yojson.option] (* Default: trades *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type subscribe_message = {
  method_ : string; [@key "method"]
  params : subscribe_params; [@key "params"]
  req_id : int option; [@key "req_id"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Ticker Data Response *)
type ticker_data = {
  ask : float; [@key "ask"]
  ask_qty : float; [@key "ask_qty"]
  bid : float; [@key "bid"]
  bid_qty : float; [@key "bid_qty"]
  change : float; [@key "change"]
  change_pct : float; [@key "change_pct"]
  high : float; [@key "high"]
  last : float; [@key "last"]
  low : float; [@key "low"]
  symbol : string; [@key "symbol"]
  volume : float; [@key "volume"]
  vwap : float; [@key "vwap"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type ticker_response = {
  channel : string; [@key "channel"]
  type_ : string; [@key "type"]
  data : ticker_data list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Custom JSON decoder for connection_id *)
let connection_id_of_yojson = function
  | `Intlit s -> Ok s
  | `String s -> Ok s
  | `Int i    -> Ok (Int64.to_string (Int64.of_int i))
  | _         -> Error "status_data.connection_id: expected int or string"

(* Status Update *) 
(* Represents the data part of a status update *) 
type status_data = { 
  version : string [@key "version"];
  system : string [@key "system"];
  api_version : string [@key "api_version"];
  connection_id : string [@of_yojson connection_id_of_yojson]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Represents the overall status update message *) 
type status_response = { 
  channel : string; [@key "channel"]
  type_ : string; [@key "type"]
  data : status_data list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Subscription Acknowledgment Result *) 
(* Renamed from subscription_result to avoid type conflicts *) 
type ack_result = {  
  channel : string; [@key "channel"]
  event_trigger : string option; [@key "event_trigger"] [@yojson.option]
  snapshot : bool; [@key "snapshot"]
  symbol : string; [@key "symbol"] (* Single symbol for the ack *) 
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Represents the overall subscription acknowledgment message *) 
type subscription_response = { 
  method_ : string; [@key "method"]
  req_id : int option; [@key "req_id"] [@yojson.option]
  result : ack_result option; [@key "result"] [@yojson.option] (* Use ack_result *) 
  success : bool; [@key "success"]
  error : string option; [@key "error"] [@yojson.option]
  time_in : string; [@key "time_in"]
  time_out : string; [@key "time_out"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Heartbeat *) 
type heartbeat_response = { 
  channel : string; [@key "channel"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* --- Start Executions Channel Types --- *)

(* Execution Subscription Parameters *)
type executions_subscribe_params = {
  channel : string; [@key "channel"] (* Value: "executions" *)
  snap_trades : bool option; [@key "snap_trades"] [@yojson.option] (* Default: false *)
  snap_orders : bool option; [@key "snap_orders"] [@yojson.option] (* Default: true *)
  order_status : bool option; [@key "order_status"] [@yojson.option] (* Default: true *)
  ratecounter : bool option; [@key "ratecounter"] [@yojson.option] (* Default: false *)
  token : string; [@key "token"] (* Required auth token *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Execution Subscription Request *)
type executions_subscribe_message = {
  method_ : string; [@key "method"] (* Value: "subscribe" *)
  exec_params_data : executions_subscribe_params; [@key "params"]
  req_id : int option; [@key "req_id"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Execution Subscription Acknowledgment Result (Similar structure to public ack) *)
type executions_ack_result = {
  channel : string; [@key "channel"]
  snap_trades : bool option; [@key "snap_trades"] [@yojson.option]
  snap_orders : bool option; [@key "snap_orders"] [@yojson.option]
  snapshot : bool option; [@key "snapshot"] [@yojson.option]         (* Present in ack, deprecated in favour of snap_trades/snap_orders *)
  maxratecount: int option; [@key "maxratecount"] [@yojson.option] (* Present in ack *)
  warnings : string list option; [@key "warnings"] [@yojson.option] (* Added based on docs/logs *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Execution Subscription Acknowledgment *)
type executions_subscribe_response = {
  method_ : string; [@key "method"]
  req_id : int option; [@key "req_id"] [@yojson.option]
  result : executions_ack_result option; [@key "result"] [@yojson.option]
  success : bool; [@key "success"]
  error : string option; [@key "error"] [@yojson.option]
  time_in : string; [@key "time_in"]
  time_out : string; [@key "time_out"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Fee Structure (within execution_report) *)
type fee = {
  asset : string; [@key "asset"]
  qty : float; [@key "qty"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Contingent Order Details (within execution_report) *)
type contingent = {
  order_type : string option; [@key "order_type"] [@yojson.option]
  trigger_price : float option; [@key "trigger_price"] [@yojson.option]
  trigger_price_type : string option; [@key "trigger_price_type"] [@yojson.option]
  limit_price : float option; [@key "limit_price"] [@yojson.option]
  limit_price_type : string option; [@key "limit_price_type"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type execution_report = {
  amended : bool option [@key "amended"] [@yojson.option];
  avg_price : float option [@key "avg_price"] [@yojson.option];
  cash_order_qty : float option [@key "cash_order_qty"] [@yojson.option];
  cl_ord_id : string option [@key "cl_ord_id"] [@yojson.option];
  contingent : contingent option [@key "contingent"] [@yojson.option];
  cost : float option [@key "cost"] [@yojson.option]; (* trade events only *)
  cum_cost : float option [@key "cum_cost"] [@yojson.option];
  cum_qty : float option [@key "cum_qty"] [@yojson.option];
  display_qty : float option [@key "display_qty"] [@yojson.option];
  display_qty_remain : float option [@key "display_qty_remain"] [@yojson.option];
  effective_time : string option [@key "effective_time"] [@yojson.option]; (* RFC3339 *)
  exec_id : string option [@key "exec_id"] [@yojson.option]; (* trade events only *)
  exec_type : string [@key "exec_type"]; (* Required *)
  expire_time : string option [@key "expire_time"] [@yojson.option]; (* RFC3339 *)
  ext_ord_id : string option [@key "ext_ord_id"] [@yojson.option]; (* UUID *)
  ext_exec_id : string option [@key "ext_exec_id"] [@yojson.option]; (* UUID *)
  fees : fee list option [@key "fees"] [@yojson.option]; (* trade events only *)
  fee_ccy_pref : string option [@key "fee_ccy_pref"] [@yojson.option];
  fee_usd_equiv : float option [@key "fee_usd_equiv"] [@yojson.option];
  limit_price : float option [@key "limit_price"] [@yojson.option];
  liquidated : bool option [@key "liquidated"] [@yojson.option];
  liquidity_ind : string option [@key "liquidity_ind"] [@yojson.option]; (* m or t *)
  last_price : float option [@key "last_price"] [@yojson.option]; (* trade events only *)
  last_qty : float option [@key "last_qty"] [@yojson.option]; (* trade events only *)
  margin : bool option [@key "margin"] [@yojson.option];
  margin_borrow : bool option [@key "margin_borrow"] [@yojson.option];
  no_mpp : bool option [@key "no_mpp"] [@yojson.option];
  ord_ref_id : string option [@key "ord_ref_id"] [@yojson.option];
  order_id : string [@key "order_id"]; (* Required *)
  order_qty : float option [@key "order_qty"] [@yojson.option];
  order_type : string option [@key "order_type"] [@yojson.option];
  order_status : string [@key "order_status"]; (* Required *)
  order_userref : int option [@key "order_userref"] [@yojson.option];
  post_only : bool option [@key "post_only"] [@yojson.option];
  position_status : string option [@key "position_status"] [@yojson.option];
  reason : string option [@key "reason"] [@yojson.option];
  reduce_only : bool option [@key "reduce_only"] [@yojson.option];
  sender_sub_id : string option [@key "sender_sub_id"] [@yojson.option];
  side : string option [@key "side"] [@yojson.option];
  symbol : string option [@key "symbol"] [@yojson.option];
  time_in_force : string option [@key "time_in_force"] [@yojson.option]; (* GTC, GTD, IOC *)
  timestamp : string [@key "timestamp"]; (* Required: RFC3339 *)
  trade_id : int option [@key "trade_id"] [@yojson.option];
  triggers : Yojson.Safe.t option [@key "triggers"] [@yojson.option]; (* Complex object, using generic json for now *)
  (* Deprecated fields *)
  cancel_reason : string option [@key "cancel_reason"] [@yojson.option]; (* deprecated *)
  stop_price : float option [@key "stop_price"] [@yojson.option]; (* deprecated *)
  trigger : string option [@key "trigger"] [@yojson.option]; (* deprecated *)
  triggered_price : float option [@key "triggered_price"] [@yojson.option]; (* deprecated *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Execution Snapshot / Update Response *)
type executions_response = {
  channel : string; [@key "channel"] (* Value: "executions" *)
  type_ : string; [@key "type"] (* snapshot or update *)
  data : execution_report list; [@key "data"]
  sequence : int option; [@key "sequence"] [@yojson.option] (* Undocumented, but might appear *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* --- End Executions Channel Types --- *)

(* Generic Response for initial parsing - keep simple for dispatch *) 
type generic_response = { 
  channel : string option; [@key "channel"] [@yojson.option]
  type_ : string option; [@key "type"] [@yojson.option]
  method_ : string option; [@key "method"] [@yojson.option]
  error : string option; [@key "error"] [@yojson.option]
  success : bool option; [@key "success"] [@yojson.option]
  req_id : int option; [@key "req_id"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

let float_to_price ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Types.Primitives.Price.of_string_exn ~scale s

let section = Lwt_log_core.Section.make "kraken_ws_feed"

let base_uri = Uri.of_string "wss://ws.kraken.com/v2"
let auth_base_uri = Uri.of_string "wss://ws-auth.kraken.com/v2"

let connect () =
  let host = "ws.kraken.com" in
  let port = 443 in
  let uri = base_uri in
  Lwt_log_core.debug ~section ("Connecting to " ^ (Uri.to_string uri)) >>= fun () ->
  let tls_config = `Hostname host, `IP (Ipaddr.of_string_exn "104.16.248.94"), `Port port in
  let endpoint = `TLS tls_config in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Websocket_lwt_unix.connect ~ctx endpoint uri

let connect_auth () =
  let host = "ws-auth.kraken.com" in
  let port = 443 in
  let uri = auth_base_uri in
  Lwt_log_core.debug ~section ("Connecting to authenticated endpoint: " ^ (Uri.to_string uri)) >>= fun () ->
  let tls_config = `Hostname host, `IP (Ipaddr.of_string_exn "104.16.248.94"), `Port port in
  let endpoint = `TLS tls_config in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Websocket_lwt_unix.connect ~ctx endpoint uri

let make_subscribe_message ?req_id symbols =
  let sub_params : subscribe_params = {
    channel = "ticker";
    symbol = symbols;
    snapshot = true;
    event_trigger = Some "trades";
  } in
  let msg = {
    method_ = "subscribe";
    params = sub_params;
    req_id = req_id;
  } in
  let content = subscribe_message_to_yojson msg |> Json.to_string in
  Frame.create ~content ()

(* Define a placeholder type for the execution callback *)
type on_execution = execution_report list -> unit Lwt.t

(* Function to create the executions subscribe message *)
let make_executions_subscribe_message ?req_id ~token ?(snap_trades=false) ?(snap_orders=true) ?(order_status=true) ?(ratecounter=false) () =
  (* Rename the local variable to avoid any potential shadowing confusion *)
  let exec_params : executions_subscribe_params = {
    channel = "executions";
    snap_trades = Some snap_trades;
    snap_orders = Some snap_orders;
    order_status = Some order_status;
    ratecounter = Some ratecounter;
    token = token; (* Auth token is required *)
  } in
  let msg = {
    method_ = "subscribe";
    exec_params_data = exec_params; (* Use the renamed field and variable *)
    req_id = req_id;
  } in
  let content = executions_subscribe_message_to_yojson msg |> Json.to_string in
  Frame.create ~content ()

(* Helper function to handle subscription acknowledgments *)
let handle_subscribe_ack json =
  match generic_response_of_yojson json with
  | Ok { method_ = Some "subscribe"; success = Some true; _ } ->
      Lwt_log_core.info ~section
        (Printf.sprintf "✅ subscribe[%s] succeeded"
           (Json.Util.(json |> member "result" |> member "channel" |> to_string)))
  | Ok { method_ = Some "subscribe"; success = Some false; error = Some e; _ } ->
      Lwt_log_core.error ~section ("❌ subscribe failed: " ^ e)
  | _ ->
      Lwt_log_core.warning ~section "subscribe-ACK not understood"

(* Refactored handler to dispatch based on message content *)
let handle_frame conn frame ~on_tick ~on_execution =
  Lwt_log_core.debug ~section ("Received frame: " ^ (Websocket.Frame.show frame)) >>= fun () ->
  match frame.opcode with
  | Frame.Opcode.Text ->
      let payload_str = frame.content in
      begin try
        let json = Json.from_string payload_str in
        Lwt_log_core.debug ~section ("Parsed JSON: " ^ (Json.to_string json)) >>= fun () ->

        match Json.Util.(member "channel" json |> to_string_option),
              Json.Util.(member "method" json |> to_string_option),
              Json.Util.(member "type" json |> to_string_option),
              Json.Util.(member "error" json |> to_string_option),
              Json.Util.(member "success" json |> to_bool_option)
        with
        (* Handle errors first *)
        | _, _, _, Some err_msg, _ ->
            Lwt_log_core.error ~section ("Received Kraken error: " ^ err_msg ^ ". Payload: " ^ payload_str)

        (* Subscription Acknowledgment *)
        | _, Some "subscribe", _, _, Some true -> (* Success case *)
            let result_json = Json.Util.member "result" json in
            begin match result_json with
            | `Null ->
                 Lwt_log_core.error ~section "❌ subscribe succeeded but missing 'result' field in response."
            | _ -> (* result_json is not null *)
                 let channel_in_result = Json.Util.to_string_option (Json.Util.member "channel" result_json) in
                 begin match channel_in_result with
                 | Some "ticker" -> (* Assume public subscription ack *)
                     begin match ack_result_of_yojson result_json with
                     | Ok ack_data ->
                          Lwt_log_core.info ~section (Printf.sprintf "✅ subscribe[ticker] succeeded (symbol=%s, snapshot=%b)"
                            ack_data.symbol ack_data.snapshot)
                     | Error err ->
                          Lwt_log_core.error ~section (Printf.sprintf "❌ subscribe[ticker] succeeded but failed to parse result: %s. Payload: %s" err (Json.to_string result_json))
                     end
                 | Some "executions" -> (* Assume executions subscription ack *)
                      begin match executions_ack_result_of_yojson result_json with
                      | Ok exec_ack_data ->
                           let warnings_str = match exec_ack_data.warnings with
                             | Some w when List.length w > 0 -> Printf.sprintf ", Warnings: [%s]" (String.concat "; " w)
                             | _ -> ""
                           in
                           Lwt_log_core.info ~section (Printf.sprintf "✅ subscribe[executions] succeeded (snap_trades=%s, snap_orders=%s, snapshot=%s, maxratecount=%s%s)"
                             (Option.value ~default:"N/A" (Option.map string_of_bool exec_ack_data.snap_trades))
                             (Option.value ~default:"N/A" (Option.map string_of_bool exec_ack_data.snap_orders))
                             (Option.value ~default:"N/A" (Option.map string_of_bool exec_ack_data.snapshot))
                             (Option.value ~default:"N/A" (Option.map string_of_int exec_ack_data.maxratecount))
                             warnings_str)
                      | Error err ->
                           Lwt_log_core.error ~section (Printf.sprintf "❌ subscribe[executions] succeeded but failed to parse result: %s. Payload: %s" err (Json.to_string result_json))
                      end
                 | Some other_channel ->
                      Lwt_log_core.warning ~section (Printf.sprintf "✅ subscribe[%s] succeeded but result parsing not implemented for this channel. Result: %s" other_channel (Json.to_string result_json))
                 | None ->
                      Lwt_log_core.error ~section (Printf.sprintf "❌ subscribe succeeded but 'channel' missing in result field. Result: %s" (Json.to_string result_json))
                 end
            end
        | _, Some "subscribe", _, _, Some false -> (* Failure case *)
             begin match Json.Util.to_string_option (Json.Util.member "error" json) with
             | Some err_msg -> Lwt_log_core.error ~section ("❌ subscribe failed: " ^ err_msg)
             | None -> Lwt_log_core.error ~section "❌ subscribe failed: unknown error"
             end

        (* Ticker Snapshot/Update *)
        | Some "ticker", _, Some type_, _, _ when type_ = "snapshot" || type_ = "update" ->
            begin match ticker_response_of_yojson json with
            | Ok ticker_resp ->
                begin match ticker_resp.data with
                | [ticker_item] ->
                    Lwt_log_core.debug ~section (Printf.sprintf "Received %s ticker for %s" ticker_resp.type_ ticker_item.symbol) >>= fun () ->
                    let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
                    let tick_event : Types.Event.tick = {
                      src = "kraken";
                      symbol = ticker_item.symbol;
                      bid = float_to_price ~scale:8 ticker_item.bid;
                      ask = float_to_price ~scale:8 ticker_item.ask;
                      ts = ts;
                    } in
                    on_tick tick_event
                | _ -> Lwt_log_core.warning ~section ("Received ticker message with unexpected data format: " ^ payload_str)
                end
            | Error err ->
                Lwt_log_core.warning ~section (Printf.sprintf "Failed to parse ticker response: %s. Payload: %s" err payload_str)
            end

        (* Executions Snapshot/Update *)
        | Some "executions", _, Some type_, _, _ when type_ = "snapshot" || type_ = "update" ->
            Lwt_log_core.info ~section ("Raw Executions Payload: " ^ payload_str) >>= fun () -> (* Log raw payload *)
            begin match executions_response_of_yojson json with
            | Ok exec_resp ->
                Lwt_log_core.info ~section (Printf.sprintf "Parsed %s executions (%d reports) for channel %s" exec_resp.type_ (List.length exec_resp.data) exec_resp.channel) >>= fun () ->
                (* Detailed logging per report can be helpful here too if needed *)
                List.iter (fun (report : execution_report) ->
                  let price_str = match report.last_price with
                    | Some p -> Printf.sprintf "price=%.8f" p
                    | None -> "price=N/A" in
                  let qty_str = match report.last_qty with
                    | Some q -> Printf.sprintf "qty=%.8f" q
                    | None -> "qty=N/A" in
                  let side_str = Option.value report.side ~default:"N/A" in
                  let symbol_str = Option.value report.symbol ~default:"N/A" in
                  let status_str = report.order_status in
                  let exec_type_str = report.exec_type in
                  Lwt_log_core.info ~section (Printf.sprintf "Execution: %s %s %s %s %s %s order_id=%s" 
                    symbol_str side_str exec_type_str status_str price_str qty_str
                    report.order_id) |> ignore
                ) exec_resp.data;
                on_execution exec_resp.data (* Call the on_execution callback *)
            | Error _ ->
                (* Try to parse the JSON manually if the automatic parsing fails *)
                try
                  let channel = Json.Util.to_string (Json.Util.member "channel" json) in
                  let type_ = Json.Util.to_string (Json.Util.member "type" json) in
                  let data = Json.Util.to_list (Json.Util.member "data" json) in
                  let reports = List.map (fun item ->
                    {
                      amended = Json.Util.to_bool_option (Json.Util.member "amended" item);
                      avg_price = Json.Util.to_number_option (Json.Util.member "avg_price" item);
                      cash_order_qty = Json.Util.to_number_option (Json.Util.member "cash_order_qty" item);
                      cl_ord_id = Json.Util.to_string_option (Json.Util.member "cl_ord_id" item);
                      contingent = None; (* Skip complex contingent parsing for now *)
                      cost = Json.Util.to_number_option (Json.Util.member "cost" item);
                      cum_cost = Json.Util.to_number_option (Json.Util.member "cum_cost" item);
                      cum_qty = Json.Util.to_number_option (Json.Util.member "cum_qty" item);
                      display_qty = Json.Util.to_number_option (Json.Util.member "display_qty" item);
                      display_qty_remain = Json.Util.to_number_option (Json.Util.member "display_qty_remain" item);
                      effective_time = Json.Util.to_string_option (Json.Util.member "effective_time" item);
                      exec_id = Json.Util.to_string_option (Json.Util.member "exec_id" item);
                      exec_type = Json.Util.to_string (Json.Util.member "exec_type" item);
                      expire_time = Json.Util.to_string_option (Json.Util.member "expire_time" item);
                      ext_ord_id = Json.Util.to_string_option (Json.Util.member "ext_ord_id" item);
                      ext_exec_id = Json.Util.to_string_option (Json.Util.member "ext_exec_id" item);
                      fees = None; (* Skip complex fees parsing for now *)
                      fee_ccy_pref = Json.Util.to_string_option (Json.Util.member "fee_ccy_pref" item);
                      fee_usd_equiv = Json.Util.to_number_option (Json.Util.member "fee_usd_equiv" item);
                      limit_price = Json.Util.to_number_option (Json.Util.member "limit_price" item);
                      liquidated = Json.Util.to_bool_option (Json.Util.member "liquidated" item);
                      liquidity_ind = Json.Util.to_string_option (Json.Util.member "liquidity_ind" item);
                      last_price = Json.Util.to_number_option (Json.Util.member "last_price" item);
                      last_qty = Json.Util.to_number_option (Json.Util.member "last_qty" item);
                      margin = Json.Util.to_bool_option (Json.Util.member "margin" item);
                      margin_borrow = Json.Util.to_bool_option (Json.Util.member "margin_borrow" item);
                      no_mpp = Json.Util.to_bool_option (Json.Util.member "no_mpp" item);
                      ord_ref_id = Json.Util.to_string_option (Json.Util.member "ord_ref_id" item);
                      order_id = Json.Util.to_string (Json.Util.member "order_id" item);
                      order_qty = Json.Util.to_number_option (Json.Util.member "order_qty" item);
                      order_type = Json.Util.to_string_option (Json.Util.member "order_type" item);
                      order_status = Json.Util.to_string (Json.Util.member "order_status" item);
                      order_userref = Json.Util.to_int_option (Json.Util.member "order_userref" item);
                      post_only = Json.Util.to_bool_option (Json.Util.member "post_only" item);
                      position_status = Json.Util.to_string_option (Json.Util.member "position_status" item);
                      reason = Json.Util.to_string_option (Json.Util.member "reason" item);
                      reduce_only = Json.Util.to_bool_option (Json.Util.member "reduce_only" item);
                      sender_sub_id = Json.Util.to_string_option (Json.Util.member "sender_sub_id" item);
                      side = Json.Util.to_string_option (Json.Util.member "side" item);
                      symbol = Json.Util.to_string_option (Json.Util.member "symbol" item);
                      time_in_force = Json.Util.to_string_option (Json.Util.member "time_in_force" item);
                      timestamp = Json.Util.to_string (Json.Util.member "timestamp" item);
                      trade_id = Json.Util.to_int_option (Json.Util.member "trade_id" item);
                      triggers = None; (* Skip complex triggers parsing for now *)
                      cancel_reason = Json.Util.to_string_option (Json.Util.member "cancel_reason" item);
                      stop_price = Json.Util.to_number_option (Json.Util.member "stop_price" item);
                      trigger = Json.Util.to_string_option (Json.Util.member "trigger" item);
                      triggered_price = Json.Util.to_number_option (Json.Util.member "triggered_price" item);
                    }
                  ) data in
                  Lwt_log_core.info ~section (Printf.sprintf "Manually parsed %s executions (%d reports) for channel %s" type_ (List.length reports) channel) >>= fun () ->
                  on_execution reports
                with exn ->
                  Lwt_log_core.warning ~section (Printf.sprintf "Failed to parse executions response: %s. Payload: %s" (Printexc.to_string exn) payload_str)
            end

        (* Status Update *)
        | Some "status", _, Some "update", _, _ ->
            begin match status_response_of_yojson json with
            | Ok status ->
                begin match status.data with
                | [status_data] -> Lwt_log_core.info ~section (Printf.sprintf "System Status: %s (Version: %s, API: %s, ConnID: %s)" status_data.system status_data.version status_data.api_version status_data.connection_id)
                | _ -> Lwt_log_core.warning ~section ("Received status message with unexpected data format: " ^ payload_str)
                end
            | Error err ->
                Lwt_log_core.warning ~section (Printf.sprintf "Failed to parse status response: %s. Payload: %s" err payload_str)
            end

        (* Heartbeat *)
        | Some "heartbeat", _, _, _, _ ->
            begin match heartbeat_response_of_yojson json with
            | Ok _ -> Lwt_log_core.debug ~section "Received Heartbeat"
            | Error err -> Lwt_log_core.warning ~section (Printf.sprintf "Failed to parse heartbeat response: %s. Payload: %s" err payload_str)
            end

        (* Unhandled message type *)
        | _, _, _, _, _ ->
            Lwt_log_core.warning ~section ("Received unhandled message type: " ^ payload_str)

      with
      | Yojson.Json_error msg ->
          Lwt_log_core.warning ~section (Printf.sprintf "Failed to parse JSON: %s. Payload: %s" msg payload_str)
      | exn ->
          Lwt_log_core.error ~section (Printf.sprintf "Exception processing frame: %s. Payload: %s" (Printexc.to_string exn) payload_str)
      end
  | Frame.Opcode.Ping ->
      Lwt_log_core.info ~section "Received Ping" >>= fun () ->
      let pong = Frame.create ~opcode:Frame.Opcode.Pong () in
      Lwt_log_core.debug ~section "Sending Pong" >>= fun () ->
      Websocket_lwt_unix.write conn pong
  | Frame.Opcode.Pong -> Lwt_log_core.info ~section "Received Pong" >>= fun () -> Lwt.return_unit
  | Frame.Opcode.Close -> Lwt_log_core.info ~section "Received Close" >>= fun () -> Lwt.return_unit
  | _ -> Lwt_log_core.warning ~section ("Received unhandled frame type: " ^ (Frame.Opcode.to_string frame.opcode)) >>= fun () -> Lwt.return_unit

(* Placeholder for tick events on the execution connection (should not receive any) *)
let dummy_on_tick (_tick : Types.Event.tick) : unit Lwt.t =
  Lwt_log_core.warning ~section "Received tick event on execution connection (unexpected)" >>= fun () ->
  Lwt.return_unit

(* Placeholder for execution events on the public connection (should not receive any) *)
let dummy_on_execution (_reports : execution_report list) : unit Lwt.t =
  Lwt_log_core.warning ~section "Received execution report on public connection (unexpected)" >>= fun () ->
  Lwt.return_unit

(* Start function for the public (ticker) feed *)
let start _cfg ~on_tick =
  let symbols_to_subscribe = ["BTC/USD"; "ETH/USD"] in
  let rec loop conn =
    Websocket_lwt_unix.read conn >>= fun frame ->
    (* Provide dummy_on_execution for the public connection handler *)
    handle_frame conn frame ~on_tick ~on_execution:dummy_on_execution >>= fun () ->
    loop conn
  in
  connect () >>= fun conn ->
  let subscribe_msg = make_subscribe_message ~req_id:1 symbols_to_subscribe in
  Lwt_log_core.info ~section ("Sending subscribe message: " ^ subscribe_msg.content) >>= fun () ->
  Websocket_lwt_unix.write conn subscribe_msg >>= fun () ->
  loop conn

(* Start function for the authenticated (executions) feed *)
let start_executions ~on_execution =
  Token.get_token () >>= fun token ->
  connect_auth () >>= fun conn ->
  let subscribe_msg = make_executions_subscribe_message ~req_id:2 ~token () in
  Lwt_log_core.info ~section ("Sending executions subscribe message: " ^ subscribe_msg.content) >>= fun () ->
  Websocket_lwt_unix.write conn subscribe_msg >>= fun () ->
  let rec loop_exec conn =
    Websocket_lwt_unix.read conn >>= fun frame ->
    handle_frame conn frame ~on_tick:dummy_on_tick ~on_execution >>= fun () ->
    loop_exec conn
  in
  loop_exec conn

(* Helper functions for safe JSON access *)
let safe_string json key default =
  try Json.Util.(member key json |> to_string)
  with _ -> default

let safe_float json key default =
  try Json.Util.(member key json |> to_number)
  with _ -> default

let safe_int json key default =
  try Json.Util.(member key json |> to_int)
  with _ -> default

let safe_bool json key default =
  try Json.Util.(member key json |> to_bool)
  with _ -> default

(* Order tracking state *)
let open_buy_orders : (string, Json.t) Hashtbl.t = Hashtbl.create 16
let pending_orders : (string, Json.t) Hashtbl.t = Hashtbl.create 16

(* Order status parsing *)
let parse_order_status = function
  | "pending_new" -> "pending_new"
  | "new" -> "new"
  | "partially_filled" -> "partially_filled"
  | "filled" -> "filled"
  | "canceled" -> "canceled"
  | "expired" -> "expired"
  | "rejected" -> "rejected"
  | _ -> "unknown"

(* Order side parsing *)
let parse_order_side = function
  | "buy" -> "buy"
  | "sell" -> "sell"
  | _ -> "unknown"

(* Logging helpers *)
let format_order_log order status =
  Printf.sprintf "[ORDER %s] %s %s at %.2f" 
    status
    (safe_string order "order_id" "")
    (safe_string order "symbol" "")
    (safe_float order "limit_price" 0.0)

let log_open_orders () =
  let orders = Hashtbl.fold (fun k v acc -> (k, v) :: acc) open_buy_orders [] in
  Lwt_log_core.debug ~section (Printf.sprintf "Open orders (%d):" (List.length orders)) >>= fun () ->
  Lwt_list.iter_s (fun (id, order) ->
    Lwt_log_core.debug ~section (Printf.sprintf "  %s: %s %.2f" 
      id 
      (safe_string order "symbol" "")
      (safe_float order "limit_price" 0.0))
  ) orders

(* Main execution message parser *)
let parse_execution_message json =
  Lwt.catch
    (fun () ->
      let channel = Json.Util.(member "channel" json |> to_string) in
      match channel with
      | "executions" ->
          let msg_type = Json.Util.(member "type" json |> to_string) in
          let data = Json.Util.(member "data" json |> to_list) in
          Lwt_log_core.debug_f ~section "[PRIVATE] Processing %s message with %d orders" 
            msg_type (List.length data) >>= fun () ->
          
          Lwt_list.iter_s (fun order_json ->
            let order_id = safe_string order_json "order_id" "" in
            let exec_type = safe_string order_json "exec_type" "" in
            let symbol = safe_string order_json "symbol" "" in
            
            match exec_type with
            | "canceled" ->
                (match Hashtbl.find_opt open_buy_orders order_id with
                 | Some existing_order ->
                     Lwt_log_core.debug ~section (format_order_log existing_order "CANCELED") >>= fun () ->
                     Hashtbl.remove open_buy_orders order_id;
                     Hashtbl.remove pending_orders order_id;
                     log_open_orders ()
                 | None -> Lwt.return_unit)
            | "filled" | "expired" ->
                (match Hashtbl.find_opt open_buy_orders order_id with
                 | Some existing_order ->
                     Hashtbl.remove open_buy_orders order_id;
                     Hashtbl.remove pending_orders order_id;
                     Lwt_log_core.debug ~section (format_order_log existing_order exec_type) >>= fun () ->
                     log_open_orders ()
                 | None -> Lwt.return_unit)
            | _ -> 
                (* Get symbol/side, preserving existing values for amendments *)
                let symbol = 
                  match exec_type with
                  | "amended" ->
                      (match Hashtbl.find_opt open_buy_orders order_id with
                       | Some existing_order -> safe_string existing_order "symbol" ""
                       | None -> symbol)
                  | "new" ->
                      (match Hashtbl.find_opt pending_orders order_id with
                       | Some pending_order -> safe_string pending_order "symbol" ""
                       | None -> symbol)
                  | _ -> symbol
                in
                
                let side = safe_string order_json "side" "" in
                
                (* Process non-terminal states *)
                match side with
                | "buy" ->
                    let status = safe_string order_json "order_status" "" in
                    begin match exec_type with
                    | "pending_new" ->
                        Hashtbl.replace pending_orders order_id order_json;
                        Lwt_log_core.debug ~section (Printf.sprintf "[ORDER %s -> %s] %s %s" 
                          exec_type status order_id symbol)
                    | "new" ->
                        Hashtbl.replace open_buy_orders order_id order_json;
                        Hashtbl.remove pending_orders order_id;
                        Lwt_log_core.debug ~section (Printf.sprintf "[ORDER %s -> %s] %s %s" 
                          exec_type status order_id symbol) >>= fun () ->
                        log_open_orders ()
                    | "trade" ->
                        let last_qty = safe_float order_json "last_qty" 0.0 in
                        let last_price = safe_float order_json "last_price" 0.0 in
                        Lwt_log_core.debug ~section 
                          (Printf.sprintf "[ORDER %s -> %s] %f %s at %.2f" 
                             exec_type status last_qty symbol last_price)
                    | "amended" ->
                        Hashtbl.replace open_buy_orders order_id order_json;
                        Lwt_log_core.debug ~section (Printf.sprintf "[ORDER %s -> %s] %s %s" 
                          exec_type status order_id symbol) >>= fun () ->
                        log_open_orders ()
                    | "restated" | "status" ->
                        Hashtbl.replace open_buy_orders order_id order_json;
                        let msg = if String.equal exec_type "restated" then
                          Printf.sprintf "[ORDER %s -> %s] %s: %s" 
                            exec_type status order_id (safe_string order_json "reason" "unknown")
                        else
                          Printf.sprintf "[ORDER %s -> %s] %s %s" 
                            exec_type status order_id symbol
                        in
                        Lwt_log_core.debug ~section msg >>= fun () ->
                        log_open_orders ()
                    | _ -> 
                        Lwt_log_core.debug ~section (Printf.sprintf "[ORDER %s -> %s] %s %s" 
                          exec_type status order_id symbol)
                    end
                | _ -> Lwt.return_unit  (* Skip non-buy orders *)
          ) data
      | _ -> 
          Lwt_log_core.debug ~section (Printf.sprintf "[DEBUG] Ignoring unknown channel: %s" channel)
    )
    (fun e ->
      Lwt_log_core.error ~section (Printf.sprintf "[PRIVATE] Error parsing execution: %s" 
        (Printexc.to_string e)))

