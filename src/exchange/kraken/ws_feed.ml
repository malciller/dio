open Lwt.Infix
open Websocket

module Json = Yojson.Safe

(* Configuration type *)
type config = {
  ws_host: string;
  ws_port: int;
  ws_path: string;
  symbols: string list;
  auth_token: string option;
}

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
  contingent: contingent option; [@key "contingent"] [@yojson.option]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

type executions_response = {
  channel: string; [@key "channel"]
  type_: string; [@key "type"]
  data: execution_report list; [@key "data"]
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Utility Functions *)
let float_to_price ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Types.Primitives.Price.of_string_exn ~scale s

let section = Lwt_log_core.Section.make "kraken_ws_feed"

(* Connection Setup *)
let connect cfg is_auth =
  let port = cfg.ws_port in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in

  if is_auth then
    (* Authenticated connection logic based on provided example *) 
    let host = "ws-auth.kraken.com" in
    let uri = Uri.of_string (Printf.sprintf "wss://%s:%d/v2" host port) in (* Use /v2 path *)
    Lwt_log_core.info ~section ("Connecting to (auth) " ^ (Uri.to_string uri)) >>= fun () ->
    let tls_config = `Hostname host, `IP (Ipaddr.of_string_exn "104.16.248.94"), `Port port in
    let endpoint = `TLS tls_config in
    Websocket_lwt_unix.connect ~ctx endpoint uri
  else 
    (* Public connection logic (existing) *) 
    let host = cfg.ws_host in
    let path = "/v2" in
    let uri = Uri.of_string (Printf.sprintf "wss://%s:%d%s" host port path) in
    Lwt_log_core.info ~section ("Connecting to (public) " ^ (Uri.to_string uri)) >>= fun () ->
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

(* Custom Yojson converter for subscribe_message *)
let custom_subscribe_message_to_yojson (msg : subscribe_message) : Json.t =
  `Assoc (
    [("method", `String msg.method_); ("params", custom_channel_params_to_yojson msg.params)] @
    (match msg.req_id with None -> [] | Some id -> [("req_id", `Int id)])
  )

(* Subscription Messages *)
let make_subscribe_message ?req_id (cfg : config) channel =
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
  in
  let msg = {
    method_ = "subscribe";
    params;
    req_id;
  } in
  let content = custom_subscribe_message_to_yojson msg |> Json.to_string in
  Lwt_log_core.debug ~section (Printf.sprintf "Sending subscribe message: %s" content) |> ignore;
  Frame.create ~content ()

(* Frame Handlers *)
let handle_public_frame conn frame ~on_tick =
  match frame.Websocket.Frame.opcode with
  | Frame.Opcode.Text ->
      let json = Json.from_string frame.content in
      begin match Json.Util.(member "channel" json |> to_string_option) with
      | Some "ticker" ->
          begin match ticker_response_of_yojson json with
          | Ok { type_ = ("snapshot" | "update") as type_; data = [ticker]; _ } ->
              Lwt_log_core.debug ~section (Printf.sprintf "Received %s ticker for %s" type_ ticker.symbol) >>= fun () ->
              let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
              let tick_event : Types.Event.tick = {
                src = "kraken";
                symbol = ticker.symbol;
                bid = float_to_price ~scale:8 ticker.bid;
                ask = float_to_price ~scale:8 ticker.ask;
                ts;
              } in
              on_tick tick_event
          | Ok _ ->
              Lwt_log_core.warning ~section ("Unexpected ticker data format: " ^ frame.content)
          | Error err ->
              Lwt_log_core.error ~section (Printf.sprintf "Failed to parse ticker: %s" err)
          end
      | Some "status" ->
          begin match status_response_of_yojson json with
          | Ok { data = [status]; _ } ->
              Lwt_log_core.info ~section (Printf.sprintf "System Status: %s (Version: %s)" status.system status.version)
          | Ok _ ->
              Lwt_log_core.warning ~section ("Unexpected status data format: " ^ frame.content)
          | Error err ->
              Lwt_log_core.error ~section (Printf.sprintf "Failed to parse status: %s" err)
          end
      | Some "heartbeat" ->
          Lwt_log_core.debug ~section "Received Heartbeat"
      | Some "subscribe" ->
          begin match subscription_response_of_yojson json with
          | Ok { success = true; result = Some result; _ } ->
              let channel = Json.Util.to_string_option (Json.Util.member "channel" result) |> Option.value ~default:"unknown" in
              Lwt_log_core.info ~section (Printf.sprintf "Subscribed to %s" channel)
          | Ok { success = false; error = Some err; _ } ->
              Lwt_log_core.error ~section (Printf.sprintf "Subscription failed: %s" err)
          | _ ->
              Lwt_log_core.error ~section "Invalid subscription response"
          end
      | _ ->
          Lwt_log_core.warning ~section ("Unhandled channel: " ^ frame.content)
      end
  | Frame.Opcode.Ping ->
      Lwt_log_core.debug ~section "Received Ping" >>= fun () ->
      Websocket_lwt_unix.write conn (Frame.create ~opcode:Frame.Opcode.Pong ())
  | Frame.Opcode.Pong ->
      Lwt_log_core.debug ~section "Received Pong"
  | Frame.Opcode.Close ->
      Lwt_log_core.info ~section "Received Close"
  | _ ->
      Lwt_log_core.warning ~section ("Unhandled frame type: " ^ Frame.Opcode.to_string frame.Websocket.Frame.opcode)

let handle_auth_frame conn frame ~on_execution =
  match frame.Websocket.Frame.opcode with
  | Frame.Opcode.Text ->
      (* Log raw content first *) 
      Lwt_log_core.debug ~section (Printf.sprintf "Auth frame received: %s" frame.content) >>= fun () ->
      let json = Json.from_string frame.content in
      begin match Json.Util.(member "channel" json |> to_string_option) with
      | Some "executions" ->
          begin match executions_response_of_yojson json with
          | Ok { type_ = ("snapshot" | "update") as type_; data; channel = _ } ->
              Lwt_log_core.info ~section (Printf.sprintf "Received %s executions (%d reports)" type_ (List.length data)) >>= fun () ->
              List.iter (fun report ->
                let side = Option.value report.side ~default:"N/A" in
                let symbol = Option.value report.symbol ~default:"N/A" in
                Lwt_log_core.debug ~section (Printf.sprintf "Execution: %s %s %s %s order_id=%s"
                  symbol side report.exec_type report.order_status report.order_id) |> ignore
              ) data;
              on_execution data
          | Ok _ ->
              Lwt_log_core.warning ~section ("Unexpected executions data format: " ^ frame.content)
          | Error err ->
              Lwt_log_core.error ~section (Printf.sprintf "Failed to parse executions: %s. Payload: %s" err frame.content)
          end
      | Some "status" ->
          begin match status_response_of_yojson json with
          | Ok { data = [status]; _ } ->
              Lwt_log_core.info ~section (Printf.sprintf "System Status: %s (Version: %s)" status.system status.version)
          | Ok _ ->
              Lwt_log_core.warning ~section ("Unexpected status data format: " ^ frame.content)
          | Error err ->
              Lwt_log_core.error ~section (Printf.sprintf "Failed to parse status: %s" err)
          end
      | Some "heartbeat" ->
          Lwt_log_core.debug ~section "Received Heartbeat"
      | Some "subscribe" ->
          begin match subscription_response_of_yojson json with
          | Ok { success = true; result = Some result; _ } ->
              let channel = Json.Util.to_string_option (Json.Util.member "channel" result) |> Option.value ~default:"unknown" in
              Lwt_log_core.info ~section (Printf.sprintf "Subscribed to %s" channel)
          | Ok { success = false; error = Some err; _ } ->
              Lwt_log_core.error ~section (Printf.sprintf "Subscription failed: %s" err)
          | _ ->
              Lwt_log_core.error ~section "Invalid subscription response"
          end
      | _ ->
          Lwt_log_core.warning ~section ("Unhandled channel: " ^ frame.content)
      end
  | Frame.Opcode.Ping ->
      Lwt_log_core.debug ~section "Received Ping" >>= fun () ->
      Websocket_lwt_unix.write conn (Frame.create ~opcode:Frame.Opcode.Pong ())
  | Frame.Opcode.Pong ->
      Lwt_log_core.debug ~section "Received Pong"
  | Frame.Opcode.Close ->
      Lwt_log_core.info ~section "Received Close"
  | _ ->
      Lwt_log_core.warning ~section ("Unhandled frame type: " ^ Frame.Opcode.to_string frame.Websocket.Frame.opcode)

(* Order Tracking *)
let open_buy_orders : (string, execution_report) Hashtbl.t = Hashtbl.create 16
let pending_orders : (string, execution_report) Hashtbl.t = Hashtbl.create 16

let log_open_orders () =
  let orders = Hashtbl.fold (fun k v acc -> (k, v) :: acc) open_buy_orders [] in
  Lwt_log_core.debug ~section (Printf.sprintf "Open orders (%d):" (List.length orders)) >>= fun () ->
  Lwt_list.iter_s (fun (id, order) ->
    Lwt_log_core.debug ~section (Printf.sprintf "  %s: %s %.2f"
      id
      (Option.value order.symbol ~default:"")
      (Option.value order.last_price ~default:0.0))
  ) orders

let handle_execution_report report =
  let order_id = report.order_id in
  let exec_type = report.exec_type in
  let _status = report.order_status in
  let symbol = Option.value report.symbol ~default:"N/A" in
  let side = Option.value report.side ~default:"unknown" in

  match exec_type, side with
  | "canceled", _ ->
      begin match Hashtbl.find_opt open_buy_orders order_id with
      | Some _ ->
          Lwt_log_core.info ~section (Printf.sprintf "Order Canceled: %s %s" order_id symbol) >>= fun () ->
          Hashtbl.remove open_buy_orders order_id;
          Hashtbl.remove pending_orders order_id;
          log_open_orders ()
      | None -> Lwt.return_unit
      end
  | ("filled" | "expired"), _ ->
      begin match Hashtbl.find_opt open_buy_orders order_id with
      | Some _ ->
          Lwt_log_core.info ~section (Printf.sprintf "Order %s: %s %s" exec_type order_id symbol) >>= fun () ->
          Hashtbl.remove open_buy_orders order_id;
          Hashtbl.remove pending_orders order_id;
          log_open_orders ()
      | None -> Lwt.return_unit
      end
  | _, "buy" ->
      begin match exec_type with
      | "pending_new" ->
          Hashtbl.replace pending_orders order_id report;
          Lwt_log_core.debug ~section (Printf.sprintf "Pending Order: %s %s" order_id symbol)
      | "new" ->
          Hashtbl.replace open_buy_orders order_id report;
          Hashtbl.remove pending_orders order_id;
          Lwt_log_core.info ~section (Printf.sprintf "New Order: %s %s" order_id symbol) >>= fun () ->
          log_open_orders ()
      | "trade" ->
          let qty = Option.value report.last_qty ~default:0.0 in
          let price = Option.value report.last_price ~default:0.0 in
          Lwt_log_core.info ~section (Printf.sprintf "Trade: %f %s at %.2f" qty symbol price)
      | "amended" ->
          Hashtbl.replace open_buy_orders order_id report;
          Lwt_log_core.info ~section (Printf.sprintf "Amended Order: %s %s" order_id symbol) >>= fun () ->
          log_open_orders ()
      | ("restated" | "status") ->
          Hashtbl.replace open_buy_orders order_id report;
          Lwt_log_core.debug ~section (Printf.sprintf "Order %s: %s %s" exec_type order_id symbol) >>= fun () ->
          log_open_orders ()
      | _ ->
          Lwt_log_core.debug ~section (Printf.sprintf "Unhandled Execution: %s %s %s" exec_type order_id symbol)
      end
  | _ -> Lwt.return_unit

(* Main Feed Functions *)
let start cfg ~on_tick =
  let rec loop conn =
    Websocket_lwt_unix.read conn >>= fun frame ->
    handle_public_frame conn frame ~on_tick >>= fun () ->
    loop conn
  in
  connect cfg false >>= fun conn ->
  let subscribe_msg = make_subscribe_message ~req_id:1 cfg `Ticker in
  Lwt_log_core.info ~section "Subscribing to ticker feed" >>= fun () ->
  Websocket_lwt_unix.write conn subscribe_msg >>= fun () ->
  loop conn

let start_executions cfg ~on_execution =
  let section = Lwt_log_core.Section.make "kraken_ws_auth" in (* Use a specific section for clarity *) 
  match cfg.auth_token with
  | None -> Lwt.fail_with "Authentication token required for executions feed"
  | Some _ ->
      let rec loop conn =
        Websocket_lwt_unix.read conn >>= fun frame ->
        (* Add catch for read errors potentially *) 
        Lwt.catch 
          (fun () -> 
            handle_auth_frame conn frame ~on_execution:(fun reports ->
              Lwt_list.iter_s handle_execution_report reports >>= fun () ->
              on_execution reports
            )
          )
          (fun ex -> 
            Lwt_log_core.error_f ~section "Error reading/handling auth frame: %s" (Printexc.to_string ex) >>= fun () ->
            (* Optionally re-raise or handle reconnection here *) 
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
          let content_str = subscribe_msg.content in
          Lwt_log_core.info ~section "Subscribing to executions feed" >>= fun () ->
          Lwt_log_core.debug ~section (Printf.sprintf "Sending executions subscribe message: %s" content_str) >>= fun () ->
          Websocket_lwt_unix.write conn subscribe_msg >>= fun () ->
          Lwt_log_core.info ~section "Starting auth message loop..." >>= fun () ->
          loop conn
        )
        (fun ex -> 
          Lwt_log_core.error_f ~section "Failed to connect/subscribe to auth endpoint: %s" (Printexc.to_string ex) >>= fun () ->
          Lwt.fail ex (* Re-throw the exception to be caught by the main loop if needed *) 
        )

