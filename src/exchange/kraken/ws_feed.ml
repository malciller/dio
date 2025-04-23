open Lwt.Infix
open Websocket

module Json = Yojson.Safe

(* Type Definitions for Kraken WS v2 API *)

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

(* Status Update *) 
(* Represents the data part of a status update *) 
type status_data = { 
  version : string; [@key "version"]
  system : string; [@key "system"]
  api_version : string; [@key "api_version"]
  connection_id : int64; [@key "connection_id"]
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

let connect () =
  let host = "ws.kraken.com" in
  let port = 443 in
  let uri = base_uri in
  Lwt_log_core.debug ~section ("Connecting to " ^ (Uri.to_string uri)) >>= fun () ->
  let tls_config = `Hostname host, `IP (Ipaddr.of_string_exn "104.16.248.94"), `Port port in
  let endpoint = `TLS tls_config in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  Websocket_lwt_unix.connect ~ctx endpoint uri

let make_subscribe_message ?req_id symbols =
  let params : subscribe_params = {
    channel = "ticker";
    symbol = symbols;
    snapshot = true;
    event_trigger = Some "trades";
  } in
  let msg = {
    method_ = "subscribe";
    params = params;
    req_id = req_id;
  } in
  let content = subscribe_message_to_yojson msg |> Json.to_string in
  Frame.create ~content ()

(* Refactored handler to dispatch based on message content *) 
let handle_frame conn frame ~on_tick = 
  Lwt_log_core.debug ~section ("Received frame: " ^ (Websocket.Frame.show frame)) >>= fun () -> 
  match frame.opcode with 
  | Frame.Opcode.Text -> 
      let payload_str = frame.content in 
      begin try 
        let json = Json.from_string payload_str in 
        Lwt_log_core.debug ~section ("Parsed JSON: " ^ (Json.to_string json)) >>= fun () -> 

        (* Attempt to determine message type based on key fields *) 
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
        | _, Some "subscribe", _, _, Some success -> 
            begin match subscription_response_of_yojson json with 
            | Ok sub_resp -> 
                let req_id_str = Option.map string_of_int sub_resp.req_id |> Option.value ~default:"N/A" in 
                if success then 
                  (* Use ack_result here *) 
                  let symbol = Option.map (fun (r: ack_result) -> r.symbol) sub_resp.result |> Option.value ~default:"(unknown symbol)" in 
                  Lwt_log_core.info ~section (Printf.sprintf "Subscription successful for %s (req_id: %s)" symbol req_id_str)
                else 
                  let err = Option.value sub_resp.error ~default:"Unknown subscription error" in 
                  Lwt_log_core.error ~section (Printf.sprintf "Subscription failed (req_id: %s): %s" req_id_str err)
            | Error err -> 
                Lwt_log_core.warning ~section (Printf.sprintf "Failed to parse subscription response: %s. Payload: %s" err payload_str)
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

        (* Status Update *) 
        | Some "status", _, Some "update", _, _ -> 
            begin match status_response_of_yojson json with 
            | Ok status -> 
                begin match status.data with 
                | [status_data] -> Lwt_log_core.info ~section (Printf.sprintf "System Status: %s (Version: %s, API: %s, ConnID: %Ld)" status_data.system status_data.version status_data.api_version status_data.connection_id)
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
      Websocket_lwt_unix.write conn pong (* Send the pong frame *)
  | Frame.Opcode.Pong -> Lwt_log_core.info ~section "Received Pong" >>= fun () -> Lwt.return_unit
  | Frame.Opcode.Close -> Lwt_log_core.info ~section "Received Close" >>= fun () -> Lwt.return_unit
  | _ -> Lwt_log_core.warning ~section ("Received unhandled frame type: " ^ (Frame.Opcode.to_string frame.opcode)) >>= fun () -> Lwt.return_unit

let start _cfg ~on_tick =
  let symbols_to_subscribe = ["BTC/USD"; "ETH/USD"] in
  let rec loop conn =
    Websocket_lwt_unix.read conn >>= fun frame ->
    handle_frame conn frame ~on_tick >>= fun () ->
    loop conn
  in
  connect () >>= fun conn ->
  let subscribe_msg = make_subscribe_message ~req_id:1 symbols_to_subscribe in
  Lwt_log_core.info ~section ("Sending subscribe message: " ^ subscribe_msg.content) >>= fun () ->
  Websocket_lwt_unix.write conn subscribe_msg >>= fun () ->
  loop conn

