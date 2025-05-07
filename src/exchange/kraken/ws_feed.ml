(* src/exchange/kraken/ws_feed.ml *)

open Lwt.Infix
open Websocket
open Lwt.Syntax
module Json = Yojson.Safe
module JsonUtil = Yojson.Safe.Util
open Types

(* Define the logging section once at the top *)
let section = Lwt_log_core.Section.make "kraken_ws_feed"

(* Simplified promise setup for snapshots *)
let executions_snapshot_processed, resolve_executions_snapshot_processed = Lwt.task ()
let instruments_loaded, resolve_instruments_loaded = Lwt.task ()

(* Expose a function to get the snapshot promise *)
let wait_for_snapshot () = executions_snapshot_processed
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

(* Utility Functions *)
let float_to_price ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Primitives.Price.of_string_exn ~scale s

let float_to_qty ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Primitives.Qty.of_string_exn ~scale s

let safe_string json key default = JsonUtil.(member key json |> to_string_option |> Option.value ~default)
let safe_float json key default = JsonUtil.(member key json |> to_float_option |> Option.value ~default)
let debug_log msg = Lwt_log_core.debug ~section msg

(* Order Side Parsing *)
let parse_order_side = function
  | "buy" -> Some Core.Buy
  | "sell" -> Some Core.Sell
  | _ -> None

(* Order Tracking - Define Hashtables after 'order' type *)
let all_open_orders : (string, Common.order) Hashtbl.t = Hashtbl.create 16
let pending_orders : (string, Common.order) Hashtbl.t = Hashtbl.create 16

let format_order_log (order : Common.order) action =
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
  let orders = Hashtbl.to_seq_values all_open_orders |> List.of_seq in
  debug_log (Printf.sprintf "Open orders (%d):" (List.length orders)) >>= fun () ->
  Lwt_list.iter_s (fun (order: Common.order) ->
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
  | "new" | "pending_new" | "amended" | "restated" | "status" | "partially_filled" -> Open (* Add partially_filled here *)
  | "filled" -> Filled
  | "canceled" | "expired" -> Canceled
  | "rejected" -> Rejected
  | _ ->
      Lwt_log_core.warning ~section (Printf.sprintf "Unhandled Kraken order status: %s, mapping to Rejected" status) |> ignore;
      Rejected

let execution_report_to_market_event (report : Common.execution_report) : Core.market_event option =
  let order_id = report.order_id in
  let client_id = "kraken:" ^ order_id in
  let ts = kraken_ts_to_core_ts report.timestamp in
  let state = kraken_status_to_core_state report.order_status in
  let symbol_opt = report.symbol in
  let price_prec, qty_prec = match symbol_opt with Some sym -> Option.value (get_precisions sym) ~default:(8,8) | None -> (8,8) in

  match report.exec_type, report.last_qty, report.last_price, kraken_side_to_core_side report.side, symbol_opt with
  | ("trade" | "filled"), Some qty_f, Some price_f, Some side, Some symbol when qty_f > 0.0 ->
      begin try
        let price = float_to_price ~scale:price_prec price_f in
        let qty = float_to_qty ~scale:qty_prec qty_f in
        Some (Core.Fill { symbol; order_id; client_id; price; qty; side; ts })
      with ex ->
        Lwt_log_core.error ~section (Printf.sprintf "Failed to convert Fill data for order %s: %s" order_id (Printexc.to_string ex)) |> ignore;
        Some (Core.Ack { order_id; client_id; state; ts })
      end
  | _ ->
      Some (Core.Ack { order_id; client_id; state; ts })

(* Connection Setup *)
let connect (cfg : Config.engine_config) is_auth =
  let port = cfg.ws_port in
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let connect_host = if is_auth then "ws-auth.kraken.com" else cfg.ws_host in
    let path = "/v2" in
  let uri = Uri.of_string (Printf.sprintf "wss://%s:%d%s" connect_host port path) in

  Lwt_unix.getaddrinfo connect_host (string_of_int port) [Unix.(AI_FAMILY PF_INET)] >>= fun addrs ->
    match addrs with
    | { Unix.ai_addr = Unix.ADDR_INET (ip_addr_from_dns, _); _ } :: _ ->
        let ip_to_use = Ipaddr.of_string_exn (Unix.string_of_inet_addr ip_addr_from_dns) in
      let tls_config = `Hostname connect_host, `IP ip_to_use, `Port port in
        let endpoint = `TLS tls_config in
        Websocket_lwt_unix.connect ~ctx endpoint uri
  | _ -> Lwt.fail_with (Printf.sprintf "Failed to resolve host: %s" connect_host)

(* Custom Yojson converter for channel_params *)
let custom_channel_params_to_yojson = function
  | Common.Ticker { symbol; snapshot; event_trigger } ->
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
let custom_subscribe_message_to_yojson (msg : Common.subscribe_message) : Json.t =
  `Assoc (
    [("method", `String msg.method_); ("params", custom_channel_params_to_yojson msg.params)] @
    (match msg.req_id with None -> [] | Some id -> [("req_id", `Int id)])
  )

(* Subscription Messages *)
let make_subscribe_message ?req_id (cfg : Config.engine_config) channel =
  let params = match channel with
    | `Ticker -> 
        Common.Ticker {
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
    Common.method_ = "subscribe";
    Common.params;
    Common.req_id;
  } in
  let content = custom_subscribe_message_to_yojson msg |> Json.to_string in
  Frame.create ~content ()

let handle_public_frame conn (cfg : Config.engine_config) frame ~on_tick =
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
                  begin match Common.ticker_response_of_yojson json with
                  | Ok { type_ = ("snapshot" | "update"); data = ticker_list; _ } ->
                      Lwt_list.iter_s
                        (fun (ticker : Common.ticker_data) ->
                          let symbol = ticker.symbol in
                          let price_prec, _ (*qty_prec not needed for ticker bid/ask*) = Option.value (get_precisions symbol) ~default:(8, 8) in
                          let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
                          let bid_price = float_to_price ~scale:price_prec ticker.bid in
                          let ask_price = float_to_price ~scale:price_prec ticker.ask in
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
                  begin match Common.status_response_of_yojson json with
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
                  begin match Common.instrument_response_of_yojson json with
                  | Ok { type_ = msg_type_str; data = { pairs; _ }; _ } when msg_type_str = "snapshot" || msg_type_str = "update"->
                      Lwt_list.iter_s
                        (fun (pair : Common.pair_data) ->
                          if List.mem pair.symbol cfg.symbols then
                            let () = Hashtbl.replace instrument_precisions pair.symbol (pair.price_precision, pair.qty_precision) in
                            Lwt_log_core.debug ~section
                              (Printf.sprintf "Stored precisions for %s: price=%d, qty=%d"
                                 pair.symbol pair.price_precision pair.qty_precision)
                          else
                            Lwt.return_unit)
                        pairs
                      >>= fun () ->
                      if msg_type_str = "snapshot" && Lwt.state instruments_loaded = Lwt.Sleep then (
                        Lwt_log_core.info ~section "Instrument snapshot processed, resolving instruments promise" >>= fun () ->
                        Lwt.wakeup_later resolve_instruments_loaded ();
                        Lwt.return_unit
                      ) else Lwt.return_unit
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

(* Helper function to process a single order item's state from executions channel *)
let process_execution_order_item_state (order_json : Json.t) (cfg : Config.engine_config) (context_msg_type : string) =
  (* context_msg_type is "snapshot" or "update", for logging context *)
  let order_id = safe_string order_json "order_id" "" in
  let item_exec_type = safe_string order_json "exec_type" "" in
  let order_status_str = safe_string order_json "order_status" "" in
  let symbol_opt = JsonUtil.(member "symbol" order_json |> to_string_option) in
  let side_str_opt = JsonUtil.(member "side" order_json |> to_string_option) in
  let limit_price_opt = JsonUtil.(member "limit_price" order_json |> to_float_option) in
  let last_price_opt = JsonUtil.(member "last_price" order_json |> to_float_option) in
  let last_qty_opt = JsonUtil.(member "last_qty" order_json |> to_float_option) in
  let order_qty_opt = JsonUtil.(member "order_qty" order_json |> to_float_option) in
  let userref_opt = JsonUtil.(member "userref" order_json |> to_int_option |> Option.map string_of_int) in

  (* Detailed log for easier debugging of incoming data for state changes *)
  debug_log (Printf.sprintf "[StateProc:%s] ID:%s, ExecType:%s, Status:%s, Symbol:%s, Side:%s, UserRef:%s, LimitPx:%s, OrderQty:%s, LastPx:%s, LastQty:%s"
    (String.capitalize_ascii context_msg_type) order_id item_exec_type order_status_str
    (Option.value symbol_opt ~default:"N/A") (Option.value side_str_opt ~default:"N/A")
    (Option.value userref_opt ~default:"N/A")
    (Option.map string_of_float limit_price_opt |> Option.value ~default:"N/A")
    (Option.map string_of_float order_qty_opt |> Option.value ~default:"N/A")
    (Option.map string_of_float last_price_opt |> Option.value ~default:"N/A")
    (Option.map string_of_float last_qty_opt |> Option.value ~default:"N/A")
  ) >>= fun () ->

  match item_exec_type with
  | "canceled" ->
      (match Hashtbl.find_opt all_open_orders order_id with
      | Some existing_order ->
          let symbol = existing_order.order_symbol in
          debug_log (format_order_log existing_order ("CANCELED" ^ (if context_msg_type = "snapshot" then " (Snapshot)" else ""))) >>= fun () ->
          handle_order_cancellation order_id symbol >>= fun () ->
          Hashtbl.remove all_open_orders order_id;
          Hashtbl.remove pending_orders order_id;
          log_open_orders ()
      | None -> Lwt.return_unit)
  | "filled" | "expired" -> (* Handles items explicitly marked as "filled" or "expired" *)
      (match Hashtbl.find_opt all_open_orders order_id with
      | Some existing_order ->
          Hashtbl.remove all_open_orders order_id;
          Hashtbl.remove pending_orders order_id;
          debug_log (format_order_log existing_order (String.uppercase_ascii item_exec_type ^ (if context_msg_type = "snapshot" then " (Snapshot)" else ""))) >>= fun () ->
          log_open_orders ()
      | None -> Lwt.return_unit)
  | _ -> (* Handles "new", "pending_new", "amended", "restated", "status", "trade", etc. *)
      let symbol =
        match item_exec_type, symbol_opt with
        | ("amended" | "new"), _ -> (* Prioritize existing order's symbol for consistency *)
            let existing_opt = if item_exec_type = "new" then Hashtbl.find_opt pending_orders order_id else Hashtbl.find_opt all_open_orders order_id in
            (match existing_opt with Some o -> o.order_symbol | None -> Option.value symbol_opt ~default:"")
        | _, Some s -> s
        | _, None -> ""
      in
      let side_opt =
        match item_exec_type with
        | ("amended" | "new") ->
            let existing_opt = if item_exec_type = "new" then Hashtbl.find_opt pending_orders order_id else Hashtbl.find_opt all_open_orders order_id in
            (match existing_opt with Some o -> o.side | None -> parse_order_side (Option.value side_str_opt ~default:""))
        | _ -> parse_order_side (Option.value side_str_opt ~default:"")
      in

      (* Only process if the symbol is in our configured list *)
      if List.exists (fun s -> String.equal s symbol) cfg.symbols then
          let status = kraken_status_to_core_state order_status_str in
          let limit_price =
            match item_exec_type with
            | "new" -> (match Hashtbl.find_opt pending_orders order_id with Some o -> o.limit_price | None -> Option.value limit_price_opt ~default:0.0)
            | "amended" -> (match Hashtbl.find_opt all_open_orders order_id with Some o -> Option.value limit_price_opt ~default:o.limit_price | None -> Option.value limit_price_opt ~default:0.0)
            | _ -> Option.value limit_price_opt ~default:0.0
          in
          let qty =
            match item_exec_type with
            | "new" -> (match Hashtbl.find_opt pending_orders order_id with Some o -> o.qty | None -> Option.value order_qty_opt ~default:0.0)
            | "amended" -> (match Hashtbl.find_opt all_open_orders order_id with Some o -> Option.value order_qty_opt ~default:o.qty | None -> Option.value order_qty_opt ~default:0.0)
            | _ -> Option.value order_qty_opt ~default:0.0
          in
          let order : Common.order = {
            order_id; client_id = userref_opt; order_symbol = symbol;
            side = side_opt; status; limit_price; qty;
          } in
          let* log_msg_lwt =
            let suffix = if context_msg_type = "snapshot" then " (Snapshot)" else "" in
            match item_exec_type with
            | "pending_new" -> Hashtbl.replace pending_orders order_id order; Lwt.return (format_order_log order ("PENDING" ^ suffix))
            | "new" ->
                Hashtbl.replace all_open_orders order_id order;
                Hashtbl.remove pending_orders order_id;
                Lwt.return (format_order_log order ("NEW" ^ suffix))
            | "trade" ->
                let last_qty_val = Option.value last_qty_opt ~default:0.0 in
                let last_price_val = Option.value last_price_opt ~default:0.0 in
                if status = Core.Open then (* If order is still open (e.g. partially_filled) *)
                  (Hashtbl.replace all_open_orders order_id order; (* Changed to all_open_orders *)
                   Lwt.return (Printf.sprintf "[ORDER PARTIAL FILL%s] %f %s at %.2f (Order remains open)" suffix last_qty_val order.order_symbol last_price_val))
                else (* If trade results in Filled or other terminal state *)
                  (Hashtbl.remove all_open_orders order_id; (* Changed to all_open_orders *)
                   Hashtbl.remove pending_orders order_id;
                   Lwt.return (Printf.sprintf "[ORDER FILL%s] %f %s at %.2f (Order now terminal)" suffix last_qty_val order.order_symbol last_price_val))
            | "amended" -> Hashtbl.replace all_open_orders order_id order; Lwt.return (format_order_log order ("AMENDED" ^ suffix))
            | "restated" | "status" -> Hashtbl.replace all_open_orders order_id order; Lwt.return (format_order_log order ((String.uppercase_ascii item_exec_type) ^ suffix))
            | _ -> Lwt.return (format_order_log order (("UPDATE (" ^ item_exec_type ^ ")" ) ^ suffix))
          in
          debug_log log_msg_lwt >>= fun () ->
          if List.mem item_exec_type ["new"; "amended"; "restated"; "status"; "pending_new"] then log_open_orders () else Lwt.return_unit
      else
        Lwt.return_unit (* Skip untracked symbol *)

let handle_auth_frame conn (cfg: Config.engine_config) frame ~on_execution =
  match frame.Websocket.Frame.opcode with
  | Frame.Opcode.Text ->
      let json = Json.from_string frame.content in
      Lwt.catch
        (fun () ->
          match JsonUtil.(member "method" json |> to_string_option) with
          | Some "subscribe" ->
              let success = JsonUtil.(member "success" json |> to_bool_option |> Option.value ~default:false) in
              let req_id_opt = JsonUtil.(member "req_id" json |> to_int_option) in
              let error_opt = JsonUtil.(member "error" json |> to_string_option) in
              let req_id_str = Option.map string_of_int req_id_opt |> Option.value ~default:"N/A" in
              if not success then
                let err_msg = Option.value error_opt ~default:("unknown error, payload: " ^ frame.content) in
                Lwt_log_core.error ~section (Printf.sprintf "Auth subscription failed (req_id=%s): %s" req_id_str err_msg)
              else
                let channel_subscribed = JsonUtil.(member "result" json |> member "channel" |> to_string_option |> Option.value ~default:"N/A") in
                Lwt_log_core.info ~section (Printf.sprintf "Auth subscription successful (req_id=%s, channel=%s)" req_id_str channel_subscribed)
          | _ -> (* Handle data messages *)
              let channel_opt = JsonUtil.(member "channel" json |> to_string_option) in
              match channel_opt with
              | Some "executions" ->
                  let msg_type_opt = JsonUtil.(member "type" json |> to_string_option) in
                  let data_json_list = JsonUtil.(member "data" json |> to_list) in
                  begin match msg_type_opt with
                  | Some "snapshot" ->
                      Lwt_list.iter_s (fun order_json ->
                          process_execution_order_item_state order_json cfg "snapshot"
                      ) data_json_list >>= fun () ->
                      if Lwt.state executions_snapshot_processed = Lwt.Sleep then (
                          Lwt_log_core.info ~section "Execution snapshot processed, resolving executions_snapshot_processed promise" >>= fun () ->
                          Lwt.wakeup_later resolve_executions_snapshot_processed ();
                      Lwt.return_unit
                      ) else Lwt.return_unit
                  | Some "update" ->
                      (* Step 1: Generate market events *)
                      let market_events = List.filter_map (fun order_json ->
                          let order_id = safe_string order_json "order_id" "" in
                          let item_exec_type = safe_string order_json "exec_type" "" in
                          let order_status_str = safe_string order_json "order_status" "" in
                          let symbol_opt = JsonUtil.(member "symbol" order_json |> to_string_option) in
                          let side_str_opt = JsonUtil.(member "side" order_json |> to_string_option) in
                          let last_price_opt = JsonUtil.(member "last_price" order_json |> to_float_option) in
                          let last_qty_opt = JsonUtil.(member "last_qty" order_json |> to_float_option) in
                          let timestamp_str = safe_string order_json "timestamp" "" in
                          let userref_opt = JsonUtil.(member "userref" order_json |> to_int_option |> Option.map string_of_int) in

                          let client_id = Option.value userref_opt ~default:("kraken:" ^ order_id) in
                          let ts = kraken_ts_to_core_ts timestamp_str in
                          let core_state = kraken_status_to_core_state order_status_str in

                          match symbol_opt with
                          | None -> (* Cannot generate event without symbol for Fill *)
                              if List.mem item_exec_type ["canceled"; "expired"; "rejected"] || core_state != Core.Open then
                                Some (Core.Ack { order_id; client_id; state = core_state; ts })
                              else None
                          | Some symbol ->
                              let price_prec, qty_prec = Option.value (get_precisions symbol) ~default:(8, 8) in
                              match item_exec_type, last_qty_opt, last_price_opt, kraken_side_to_core_side side_str_opt with
                              | ("trade" | "filled"), Some qty_f, Some price_f, Some side when qty_f > 0.0 ->
                                  (try
                                     Some (Core.Fill { symbol; order_id; client_id; price=(float_to_price ~scale:price_prec price_f); qty=(float_to_qty ~scale:qty_prec qty_f); side; ts })
                               with ex -> 
                                 Lwt_log_core.error ~section (Printf.sprintf "Failed converting Fill data for update %s: %s" order_id (Printexc.to_string ex)) |> Lwt.ignore_result; 
                                     Some (Core.Ack { order_id; client_id; state = core_state; ts }))
                              | _ -> (* For any other exec_type or if not a valid trade for Fill, generate Ack *)
                                  Some (Core.Ack { order_id; client_id; state = core_state; ts })
                      ) data_json_list in

                      (* Step 2: Call on_execution *)
                      let* () = 
                        if market_events <> [] then (
                          debug_log (Printf.sprintf "Calling on_execution with %d events from update" (List.length market_events)) >>= fun () ->
                          on_execution market_events
                        ) else Lwt.return_unit (* Removed redundant log for no events for brevity *)
                      in
                      
                      (* Step 3: Update Internal State *)
                      Lwt_list.iter_s (fun order_json ->
                          process_execution_order_item_state order_json cfg "update"
                      ) data_json_list
                  | Some other_type ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Unknown execution message type: %s. Payload: %s" other_type frame.content)
                  | None ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Message type is missing in execution message. Payload: %s" frame.content)
                  end
              | Some "status" -> (* Existing status handling *)
                  begin match Common.status_response_of_yojson json with
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

(* Getter for open orders (used by Strategy) - Updated Type and Name *)
let get_all_open_orders () : (string, Common.order) Hashtbl.t = all_open_orders

(* Main Feed Functions *)
let start (cfg : Config.engine_config) ~on_tick =
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

let start_executions (cfg : Config.engine_config) ~on_execution =
  (* Using global 'section' now *)
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
