(* src/exchange/kraken/kraken_incoming_data.ml *)

open Lwt.Infix
open Websocket
open Lwt.Syntax
module Json = Yojson.Safe
module JsonUtil = Yojson.Safe.Util
open Dio_types
open State
open Discord_webhook

let section = Lwt_log_core.Section.make "kraken_ws_feed"

(* get orderbook symbols from config *)
let get_orderbook_symbols (runtime_cfg : Config.runtime_cfg) : string list =
  (* Return ALL symbols for arbitrage strategy - it needs orderbook data for all pairs *)
  List.map (fun (asset : Config.asset_cfg) -> asset.symbol) runtime_cfg.assets

let executions_snapshot_processed, resolve_executions_snapshot_processed = Lwt.task ()
let instruments_loaded, resolve_instruments_loaded = Lwt.task ()

let wait_for_snapshot () = executions_snapshot_processed
let wait_for_instruments () = instruments_loaded

(* Storage for instrument precisions: symbol -> (price_precision, qty_precision) *)
let instrument_precisions : (string, (int * int)) Hashtbl.t = Hashtbl.create 16
let instrument_data : (string, Kraken_common_types.pair_data) Hashtbl.t = Hashtbl.create 256

let get_precisions symbol : (int * int) option = Hashtbl.find_opt instrument_precisions symbol

let get_instrument symbol : Kraken_common_types.pair_data option = Hashtbl.find_opt instrument_data symbol

let get_price_precision symbol : int option =
  match Hashtbl.find_opt instrument_precisions symbol with
  | Some (price_prec, _) -> Some price_prec
  | None -> None

let float_to_price ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Primitives.Price.of_string_exn ~scale s

let float_to_qty ~scale f =
  let s = Printf.sprintf "%.*f" scale f in
  Primitives.Qty.of_string_exn ~scale s

let safe_string json key default = JsonUtil.(member key json |> to_string_option |> Option.value ~default)
let safe_float json key default = JsonUtil.(member key json |> to_float_option |> Option.value ~default)
let debug_log msg = Lwt_log_core.debug ~section msg

let state : State.t ref = ref State.initial

let redact_token_in_json_string (json_str : string) : string =
  try
    let json = Yojson.Safe.from_string json_str in
    match json with
    | `Assoc assoc ->
        let redactor (key, value) =
          if key = "params" then
            match value with
            | `Assoc params_assoc ->
                let redacted_params = List.map (fun (k, v) ->
                  if k = "token" then (k, `String "[REDACTED]") else (k, v)
                ) params_assoc in
                (key, `Assoc redacted_params)
            | _ -> (key, value)
          else
            (key, value)
        in
        `Assoc (List.map redactor assoc) |> Yojson.Safe.to_string
    | _ -> json_str (* Not the expected structure, return original *)
  with
  | _ -> json_str (* Parsing failed, return original *)

(* Order Side Parsing *)
let parse_order_side = function
  | "buy" -> Some Core.Buy
  | "sell" -> Some Core.Sell
  | _ -> None

(* Order Tracking - Define Hashtables after 'order' type *)
let all_open_orders : (string, Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16
let pending_orders : (string, Kraken_common_types.order) Hashtbl.t = Hashtbl.create 16

let format_order_log (order : Kraken_common_types.order) action =
  Printf.sprintf "[ORDER %s] ID: %s, Symbol: %s, Side: %s, Status: %s, Price: %.8f"
    action order.order_id order.order_symbol
    (match order.side with Some Buy -> "Buy" | Some Sell -> "Sell" | None -> "Unknown")
    (match order.status with 
     | Core.Open -> "Open"
     | Core.Filled -> "Filled"
     | Core.Canceled -> "Canceled"
     | Core.Rejected -> "Rejected" 
    )
    order.limit_price

let log_open_orders () =
  let orders = Hashtbl.to_seq_values all_open_orders |> List.of_seq in
  debug_log (Printf.sprintf "Open orders (%d):" (List.length orders)) >>= fun () ->
  Lwt_list.iter_s (fun (order: Kraken_common_types.order) ->
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

let kraken_status_to_core_state status : Core.order_state = 
  match status with
  | "new" | "pending_new" | "amended" | "restated" | "status" | "partially_filled" -> Open 
  | "filled" -> Filled
  | "canceled" | "expired" -> Canceled
  | "rejected" -> Rejected
  | _ ->
      Lwt_log_core.warning ~section (Printf.sprintf "Unhandled Kraken order status: %s, mapping to Rejected" status) |> ignore;
      Rejected

let execution_report_to_market_event (report : Kraken_common_types.execution_report) : Core.market_event option =
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
  | Kraken_common_types.Ticker { symbol; snapshot; event_trigger } ->
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
  | Kraken_common_types.Book { symbol; depth; snapshot } ->
      `Assoc [("channel", `String "book"); ("symbol", `List (List.map (fun s -> `String s) symbol)); ("depth", `Int depth); ("snapshot", `Bool snapshot)]

(* Custom Yojson converter for subscribe_message *)
let custom_subscribe_message_to_yojson (msg : Kraken_common_types.subscribe_message) : Json.t =
  `Assoc (
    [("method", `String msg.method_); ("params", custom_channel_params_to_yojson msg.params)] @
    (match msg.req_id with None -> [] | Some id -> [("req_id", `Int id)])
  )

(* Subscription Messages *)
let make_subscribe_message ?req_id (cfg : Config.engine_config) channel =
  let params = match channel with
    | `Ticker -> 
        Kraken_common_types.Ticker {
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
    | `Book symbols ->
        Book {
          symbol = symbols;
          depth = 25;
          snapshot = true;
        }
  in
  let msg = {
    Kraken_common_types.method_ = "subscribe";
    Kraken_common_types.params;
    Kraken_common_types.req_id;
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
                  begin match Kraken_common_types.ticker_response_of_yojson json with
                  | Ok { type_ = ("snapshot" | "update"); data = ticker_list; _ } ->
                      Lwt_list.iter_s
                        (fun (ticker : Kraken_common_types.ticker_data) ->
                          let symbol = ticker.symbol in
                          let price_prec, _ = Option.value (get_precisions symbol) ~default:(8, 8) in
                          let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
                          let bid_price = float_to_price ~scale:price_prec ticker.bid in
                          let ask_price = float_to_price ~scale:price_prec ticker.ask in
                          let last_price = float_to_price ~scale:price_prec ticker.last in
                          let current_price =
                            if Primitives.Price.equal last_price (Primitives.Price.zero price_prec) then
                              Primitives.Price.midpoint bid_price ask_price
                            else
                              last_price
                          in
                          state := State.update_price symbol current_price !state;
                          let event_tick : Event.tick = {
                            src = "kraken";
                            symbol;
                            bid = bid_price;
                            ask = ask_price;
                            current_price;
                            ts;
                            ask_qty = ticker.ask_qty;
                            bid_qty = ticker.bid_qty;
                            change = ticker.change;
                            change_pct = ticker.change_pct;
                            high = ticker.high;
                            last_price = ticker.last;
                            low = ticker.low;
                            volume = ticker.volume;
                            vwap = ticker.vwap;
                          } in
                          on_tick event_tick
                      ) ticker_list
                  | Ok _ ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Unexpected ticker data format: %s" frame.content)
                  | Error err ->
                      Lwt_log_core.error ~section (Printf.sprintf "Failed to parse ticker: %s. Payload: %s" err frame.content)
                  end
              | Some "status" ->
                  begin match Kraken_common_types.status_response_of_yojson json with
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
                  begin match Kraken_common_types.instrument_response_of_yojson json with
                  | Ok { type_ = msg_type_str; data = { pairs; _ }; _ } when msg_type_str = "snapshot" || msg_type_str = "update"->
                      Lwt_list.iter_s
                        (fun (pair : Kraken_common_types.pair_data) ->
                          if List.mem pair.symbol cfg.symbols then
                            let () = 
                              Hashtbl.replace instrument_precisions pair.symbol (pair.price_precision, pair.qty_precision);
                              Hashtbl.replace instrument_data pair.symbol pair
                            in
                            Lwt_log_core.debug ~section
                              (Printf.sprintf "Stored instrument data for %s" pair.symbol)
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
                      Lwt_log_core.warning ~section (Printf.sprintf "Unexpected instrument data format")
                  | Error err ->
                      Lwt_log_core.error ~section (Printf.sprintf "Failed to parse instrument data: %s" err)
                  end
              | Some "book" ->
                  (* Handle book message and generate ticks for top-of-book changes *)
                  let* () = Kraken_orderbook.handle_book_message ~get_precisions json in
                  (* Generate ticks for symbols that had book updates *)
                  let open Yojson.Safe.Util in
                  (try
                    let data_list = json |> member "data" |> to_list in
                    Lwt_list.iter_s (fun data_json ->
                      let symbol = data_json |> member "symbol" |> to_string in
                      (* Generate tick from current orderbook state *)
                      match Kraken_orderbook.get_best_bid_ask symbol with
                      | Some (bid_price, ask_price) ->
                          let price_prec, _ = Option.value (get_precisions symbol) ~default:(8, 8) in
                          let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
                          let bid_price_primitive = float_to_price ~scale:price_prec bid_price in
                          let ask_price_primitive = float_to_price ~scale:price_prec ask_price in
                          let current_price = Primitives.Price.midpoint bid_price_primitive ask_price_primitive in
                          
                          (* Create tick event *)
                          let event_tick : Event.tick = {
                            src = "kraken";
                            symbol;
                            bid = bid_price_primitive;
                            ask = ask_price_primitive;
                            current_price;
                            ts;
                            ask_qty = 0.0; 
                            bid_qty = 0.0;
                            change = 0.0;  
                            change_pct = 0.0;
                            high = 0.0;
                            last_price = 0.0;
                            low = 0.0;
                            volume = 0.0;
                            vwap = 0.0;
                          } in
                          
                          Lwt_log_core.debug ~section (Printf.sprintf "Generated book tick for %s: bid=%s ask=%s" 
                            symbol 
                            (Primitives.Price.to_string bid_price_primitive) 
                            (Primitives.Price.to_string ask_price_primitive)) >>= fun () ->
                          
                          (* Send tick to strategy *)
                          on_tick event_tick
                      | None ->
                          Lwt_log_core.debug ~section (Printf.sprintf "No best bid/ask available for %s after book update" symbol) >>= fun () ->
                          Lwt.return_unit
                    ) data_list
                  with
                  | exn ->
                      Lwt_log_core.error ~section (Printf.sprintf "Failed to generate ticks from book update: %s" (Printexc.to_string exn)) >>= fun () ->
                      Lwt.return_unit
                  )
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

  let symbol_for_stats =
      let existing_opt =
        match item_exec_type with
        | "new" -> Hashtbl.find_opt pending_orders order_id
        | _ -> Hashtbl.find_opt all_open_orders order_id 
      in
      match existing_opt with
      | Some o -> Some o.order_symbol
      | None -> symbol_opt 
  in

  debug_log (Printf.sprintf "[StateProc:%s] ID:%s, ExecType:%s, Status:%s, Symbol:%s, Side:%s, UserRef:%s, LimitPx:%s, OrderQty:%s, LastPx:%s, LastQty:%s"
    (String.capitalize_ascii context_msg_type) order_id item_exec_type order_status_str
    (Option.value symbol_for_stats ~default:"N/A") (Option.value side_str_opt ~default:"N/A")
    (Option.value userref_opt ~default:"N/A")
    (Option.map string_of_float limit_price_opt |> Option.value ~default:"N/A")
    (Option.map string_of_float order_qty_opt |> Option.value ~default:"N/A")
    (Option.map string_of_float last_price_opt |> Option.value ~default:"N/A")
    (Option.map string_of_float last_qty_opt |> Option.value ~default:"N/A")
  ) >>= fun () ->

  match item_exec_type with
  | "canceled" ->
      let%lwt was_in_pending_and_stat_handled =
        match Hashtbl.find_opt pending_orders order_id with
        | Some po ->
            Lwt_log_core.debug ~section (Printf.sprintf "[StatsUpdate] Calling dec_pending for %s (canceled/pending)" po.order_symbol) >>= fun () -> 
            state := dec_pending po.order_symbol !state;
            Hashtbl.remove pending_orders order_id;
            Lwt_log_core.debug ~section (format_order_log po ("CANCELED (from Pending State)" ^ (if context_msg_type = "snapshot" then " (Snapshot)" else ""))) >>= fun () ->
            Lwt.return true
        | None -> Lwt.return false
      in
      (match Hashtbl.find_opt all_open_orders order_id with
      | Some existing_order ->
          let symbol = existing_order.order_symbol in
          Hashtbl.remove all_open_orders order_id;
          debug_log (format_order_log existing_order ("CANCELED" ^ (if context_msg_type = "snapshot" then " (Snapshot)" else ""))) >>= fun () ->
          handle_order_cancellation order_id symbol >>= fun () ->
          log_open_orders ()
      | None -> 
          if not was_in_pending_and_stat_handled then
            Lwt_log_core.debug ~section (Printf.sprintf "[ORDER CANCELED UNKNOWN] ID: %s not found in open or pending." order_id)
          else
            Lwt.return_unit
      )
  | "filled" | "expired" ->
      let%lwt was_in_pending_and_stat_handled =
        match Hashtbl.find_opt pending_orders order_id with
        | Some po ->
            Lwt_log_core.debug ~section (Printf.sprintf "[StatsUpdate] Calling dec_pending for %s (filled/expired/pending)" po.order_symbol) >>= fun () -> (* Added Log *)
            state := dec_pending po.order_symbol !state;
            Hashtbl.remove pending_orders order_id;
            Lwt_log_core.debug ~section (format_order_log po ((String.uppercase_ascii item_exec_type) ^ " (from Pending State)" ^ (if context_msg_type = "snapshot" then " (Snapshot)" else ""))) >>= fun () ->
            Lwt.return true
        | None -> Lwt.return false
      in
      (match Hashtbl.find_opt all_open_orders order_id with
      | Some existing_order ->
          Hashtbl.remove all_open_orders order_id;
          debug_log (format_order_log existing_order (String.uppercase_ascii item_exec_type ^ (if context_msg_type = "snapshot" then " (Snapshot)" else ""))) >>= fun () ->
          log_open_orders ()
      | None -> 
          if not was_in_pending_and_stat_handled then
            Lwt_log_core.debug ~section (Printf.sprintf "[ORDER %s UNKNOWN] ID: %s not found in open or pending." (String.uppercase_ascii item_exec_type) order_id)
          else
            Lwt.return_unit 
      )
  | _ -> 
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
            | "trade" -> (* For trade events, preserve existing price if no new price provided *)
                (match Hashtbl.find_opt all_open_orders order_id with Some o -> Option.value limit_price_opt ~default:o.limit_price | None -> Option.value limit_price_opt ~default:0.0)
            | _ -> Option.value limit_price_opt ~default:0.0
          in
          let qty =
            match item_exec_type with
            | "new" -> (match Hashtbl.find_opt pending_orders order_id with Some o -> o.qty | None -> Option.value order_qty_opt ~default:0.0)
            | "amended" -> (match Hashtbl.find_opt all_open_orders order_id with Some o -> Option.value order_qty_opt ~default:o.qty | None -> Option.value order_qty_opt ~default:0.0)
            | _ -> Option.value order_qty_opt ~default:0.0
          in
          let order : Kraken_common_types.order = {
            order_id; client_id = userref_opt; order_symbol = symbol;
            side = side_opt; status; limit_price; qty;
          } in
          let* log_msg_lwt =
            let suffix = if context_msg_type = "snapshot" then " (Snapshot)" else "" in
            match item_exec_type with
            | "pending_new" ->
                Hashtbl.replace pending_orders order_id order;
                Lwt_log_core.debug ~section (Printf.sprintf "[StatsUpdate] Order is pending_new: Calling inc_pending for %s (%s)" order.order_symbol context_msg_type) >>= fun () ->
                state := inc_pending order.order_symbol !state; 
                Lwt.return (format_order_log order ("PENDING" ^ suffix))
            | "new" ->
                let was_pending_internally = Hashtbl.mem pending_orders order_id in
                Hashtbl.replace all_open_orders order_id order;
                Hashtbl.remove pending_orders order_id;

                if was_pending_internally then (
                  Lwt_log_core.debug ~section (Printf.sprintf "[StatsUpdate] Order moved from internal pending to new: Calling dec_pending for %s (%s)" order.order_symbol context_msg_type) |> Lwt.ignore_result;
                  state := dec_pending order.order_symbol !state 
                );

                Lwt_log_core.debug ~section (Printf.sprintf "[StatsUpdate] Order is new/open on exchange: Calling inc_pending for %s (%s)" order.order_symbol context_msg_type) |> Lwt.ignore_result;
                state := inc_pending order.order_symbol !state;

                Lwt.return (format_order_log order ("NEW" ^ suffix))
            | "trade" ->
                Lwt_log_core.debug ~section (Printf.sprintf "[StatsUpdate] Calling inc_trades for %s" order.order_symbol) >>= fun () -> (* Added Log *)
                state := inc_trades order.order_symbol !state; 
                let last_qty_val = Option.value last_qty_opt ~default:0.0 in
                let last_price_val = Option.value last_price_opt ~default:0.0 in
                if status = Core.Open then 
                  (match Hashtbl.find_opt all_open_orders order_id with
                   | Some existing ->
                       let remaining_qty = existing.qty -. last_qty_val in
                       let updated_order = { order with qty = (if remaining_qty > 0.0 then remaining_qty else 0.0) } in
                       Hashtbl.replace all_open_orders order_id updated_order;
                       Lwt.return (Printf.sprintf "[ORDER PARTIAL FILL%s] %f %s at %.2f (Remaining qty: %.8f)" suffix last_qty_val order.order_symbol last_price_val remaining_qty)
                   | None ->
                       Lwt.return (Printf.sprintf "[ORDER PARTIAL FILL%s] %f %s at %.2f (No existing order found)" suffix last_qty_val order.order_symbol last_price_val))
                else 
                  (* Check if it was pending before moving *)
                  let was_pending = Hashtbl.mem pending_orders order_id in
                  Hashtbl.remove all_open_orders order_id;
                  Hashtbl.remove pending_orders order_id;
                  if was_pending then state := dec_pending order.order_symbol !state; 
                  Lwt.return (Printf.sprintf "[ORDER FILL%s] %f %s at %.2f (Order now terminal)" suffix last_qty_val order.order_symbol last_price_val)
            | "amended" -> Hashtbl.replace all_open_orders order_id order; Lwt.return (format_order_log order ("AMENDED" ^ suffix))
            | "restated" | "status" -> Hashtbl.replace all_open_orders order_id order; Lwt.return (format_order_log order ((String.uppercase_ascii item_exec_type) ^ suffix))
            | _ -> Lwt.return (format_order_log order (("UPDATE (" ^ item_exec_type ^ ")" ) ^ suffix))
          in
          debug_log log_msg_lwt >>= fun () ->
          if List.mem item_exec_type ["new"; "amended"; "restated"; "status"; "pending_new"] then log_open_orders () else Lwt.return_unit
      else
        Lwt.return_unit 

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
                let redacted_payload = redact_token_in_json_string frame.content in
                let err_msg = Option.value error_opt ~default:("unknown error, payload: " ^ redacted_payload) in
                Lwt_log_core.error ~section (Printf.sprintf "Auth subscription failed (req_id=%s): %s" req_id_str err_msg)
              else
                let channel_subscribed = JsonUtil.(member "result" json |> member "channel" |> to_string_option |> Option.value ~default:"N/A") in
                Lwt_log_core.info ~section (Printf.sprintf "Auth subscription successful (req_id=%s, channel=%s)" req_id_str channel_subscribed)
          | _ -> 
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
                      let market_events = List.flatten (List.map (fun order_json ->
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
                          | None -> 
                              if List.mem item_exec_type ["canceled"; "expired"; "rejected"] || core_state != Core.Open then
                                [Core.Ack { order_id; client_id; state = core_state; ts }]
                              else []
                          | Some symbol ->
                              let price_prec, qty_prec = Option.value (get_precisions symbol) ~default:(8, 8) in
                              match item_exec_type, last_qty_opt, last_price_opt, kraken_side_to_core_side side_str_opt with
                              | ("trade" | "filled"), Some qty_f, Some price_f, Some side when qty_f > 0.0 ->
                                  (try
                                     let fill_event = Core.Fill { symbol; order_id; client_id; price=(float_to_price ~scale:price_prec price_f); qty=(float_to_qty ~scale:qty_prec qty_f); side; ts } in
                                     
                                     let asset_name, quote_name = 
                                       match get_instrument symbol with
                                       | Some inst -> inst.base, inst.quote
                                       | None -> 
                                           let maybe_base = String.sub symbol 0 (String.length symbol - 4) in
                                           let maybe_quote = String.sub symbol (String.length symbol - 4) 4 in
                                           maybe_base, maybe_quote
                                     in
                                     let value = price_f *. qty_f in
                                     let value_str = 
                                         if String.ends_with ~suffix:"USD" quote_name then
                                             Printf.sprintf "USD %.2f" value
                                         else
                                             Printf.sprintf "%.4f %s" value quote_name
                                     in
                                     let qty_str = Printf.sprintf "%.8f" qty_f in
                                     let payload : fill_notification_payload = {
                                       side;
                                       asset_name;
                                       qty_str;
                                       value_str;
                                       order_id;
                                       symbol;
                                     } in
                                     Lwt.async (fun () -> send_message (Fill payload));

                                     if core_state = Filled then
                                       [fill_event; Core.Ack { order_id; client_id; state = core_state; ts }]
                                     else
                                       [fill_event]
                                   with ex -> 
                                     Lwt_log_core.error ~section (Printf.sprintf "Failed converting Fill data for update %s: %s" order_id (Printexc.to_string ex)) |> Lwt.ignore_result; 
                                     [Core.Ack { order_id; client_id; state = core_state; ts }])
                              | _ -> 
                                  [Core.Ack { order_id; client_id; state = core_state; ts }]
                      ) data_json_list) in

                      (* Step 2: Update Internal State (moved before calling on_execution to avoid race condition) *)
                      Lwt_list.iter_s (fun order_json ->
                          process_execution_order_item_state order_json cfg "update"
                      ) data_json_list >>= fun () ->

                      (* Step 3: Call on_execution (after state update) *)
                      if market_events <> [] then (
                        debug_log (Printf.sprintf "Calling on_execution with %d events from update" (List.length market_events)) >>= fun () ->
                        on_execution market_events
                      ) else Lwt.return_unit

                  | Some other_type ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Unknown execution message type: %s. Payload: %s" other_type frame.content)
                  | None ->
                      Lwt_log_core.warning ~section (Printf.sprintf "Message type is missing in execution message. Payload: %s" frame.content)
                  end
              | Some "status" -> 
                  begin match Kraken_common_types.status_response_of_yojson json with
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

(* Getter for open orders (used by Strategy) *)
let get_all_open_orders () : (string, Kraken_common_types.order) Hashtbl.t = all_open_orders

(* Main Feed Functions *)
let start ?runtime_cfg (cfg : Config.engine_config) ~on_tick =
  let rec loop conn =
    Lwt.catch
      (fun () ->
        Websocket_lwt_unix.read conn >>= fun frame ->
        handle_public_frame conn cfg frame ~on_tick >>= fun () ->
        loop conn)
      (fun exn ->
        Lwt_log_core.error_f ~section "Error in public feed read loop: %s" (Printexc.to_string exn) >>= fun () ->
        (* Send a close frame if possible, but don't wait for it to complete *)
        Lwt.catch
          (fun () -> 
            Websocket_lwt_unix.write conn (Frame.create ~opcode:Frame.Opcode.Close ()) >>= fun _ ->
            Lwt.return_unit)
          (fun _ -> Lwt.return_unit) >>= fun () ->
        (* Re-throw the exception so Feed.start's retry loop will handle it *)
        Lwt.fail exn)
  in
  connect cfg false >>= fun conn ->
  let subscribe_ticker_msg = make_subscribe_message ~req_id:1 cfg `Ticker in
  let subscribe_instrument_msg = make_subscribe_message ~req_id:3 cfg `Instrument in 
  Websocket_lwt_unix.write conn subscribe_ticker_msg >>= fun () ->
  Websocket_lwt_unix.write conn subscribe_instrument_msg >>= fun () -> 
  
  (* Subscribe to book channels for orderbook symbols *)
  (match runtime_cfg with
   | Some runtime_cfg ->
     let orderbook_symbols = get_orderbook_symbols runtime_cfg in
     if List.length orderbook_symbols > 0 then (
       let subscribe_book_msg = make_subscribe_message ~req_id:4 cfg (`Book orderbook_symbols) in
       Websocket_lwt_unix.write conn subscribe_book_msg >>= fun () ->
       Lwt_log_core.info ~section (Printf.sprintf "Subscribed to book channel for %d symbols" (List.length orderbook_symbols))
     ) else
       Lwt.return_unit
   | None -> Lwt.return_unit
  ) >>= fun () ->
  
  loop conn

let start_executions (cfg : Config.engine_config) ~on_execution =
  match cfg.auth_token with
  | None -> Lwt.fail_with "Authentication token required for executions feed"
  | Some _ ->
      let rec loop conn =
        Lwt.catch 
          (fun () -> 
            Websocket_lwt_unix.read conn >>= fun frame ->
            handle_auth_frame conn cfg frame ~on_execution >>= fun () ->
            loop conn
          )
          (fun ex -> 
            Lwt_log_core.error_f ~section "Error in auth feed read loop: %s" (Printexc.to_string ex) >>= fun () ->
            (* Send a close frame if possible, but don't wait for it to complete *)
            Lwt.catch
              (fun () -> 
                Websocket_lwt_unix.write conn (Frame.create ~opcode:Frame.Opcode.Close ()) >>= fun _ ->
                Lwt.return_unit)
              (fun _ -> Lwt.return_unit) >>= fun () ->
            (* Re-throw the exception so Feed.start_executions's retry loop will handle it *)
            Lwt.fail ex
          )
      in
      Lwt.catch 
        (fun () -> 
          Lwt_log_core.info ~section "Attempting to connect to auth endpoint..." >>= fun () ->
          connect cfg true >>= fun conn ->
          Lwt_log_core.info ~section "Successfully connected to auth endpoint." >>= fun () ->
          let subscribe_msg = make_subscribe_message ~req_id:2 cfg `Executions in
          Lwt_log_core.info ~section "Subscribing to executions feed" >>= fun () ->
          let redacted_content = redact_token_in_json_string subscribe_msg.content in
          Lwt_log_core.debug ~section (Printf.sprintf "Sending executions subscribe message: %s" redacted_content) >>= fun () ->
          Websocket_lwt_unix.write conn subscribe_msg >>= fun () ->
          Lwt_log_core.info ~section "Starting auth message loop..." >>= fun () ->
          loop conn
        )
        (fun ex -> 
          Lwt_log_core.error_f ~section "Failed to connect/subscribe to auth endpoint: %s" (Printexc.to_string ex) >>= fun () ->
          Lwt.fail ex
        )
