(** Kraken Exchange WebSocket API Types and Utilities

    This module defines OCaml types for Kraken's WebSocket API v2, including
    subscription parameters, response messages, and order management structures.
    Provides nonce generation and request signing for authenticated operations.
*)

open Dio_types
open Lwt_log_core
open Lwt.Infix
open State


let section = Section.make "kraken_common_types"

(** Global counter for generating unique client IDs *)
let client_id_counter = ref 0

(** Generate a unique client ID for orders *)
let generate_unique_client_id side =
  let side_prefix = match side with Core.Buy -> "b" | Core.Sell -> "s" in
  let timestamp_mod = Int64.rem (Int64.of_float (Unix.time () *. 1000.)) 100000L in  (* Last 5 digits of milliseconds *)
  let counter = !client_id_counter in
  client_id_counter := counter + 1;
  Printf.sprintf "%s%05Ld%04d" side_prefix timestamp_mod counter

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
  cum_qty: float option; [@key "cum_qty"] [@yojson.option]
  cum_cost: float option; [@key "cum_cost"] [@yojson.option]
  avg_price: float option; [@key "avg_price"] [@yojson.option]
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

(*
  Shared Strategy Utilities

  Common functionality used by both Greedy Market Making and Valley Market Making strategies
*)

(** Shared state management for market making strategies *)
module StrategyState = struct
  let section = Section.make "kraken.strategy.common"

  (** Price information tracking *)
  let price_info : (string, Event.tick) Hashtbl.t = Hashtbl.create 16

  (** Open orders tracking *)
  let open_orders : (string, order) Hashtbl.t = Hashtbl.create 16

  (** Pending orders tracking - orders sent to command buffer but not yet acknowledged *)
  let pending_orders : (string, (string * Core.side)) Hashtbl.t = Hashtbl.create 16

  (** USD balance tracking *)
  let usd_balance : float ref = ref 0.0

  (** Last amend time tracking for rate limiting *)
  let last_amend_time : (string, float) Hashtbl.t = Hashtbl.create 16

  (** Amend cooldown in seconds *)
  let amend_cooldown = 5.0

  (** Update USD balance from exchange *)
  let refresh_usd_balance get_balances_fn =
    get_balances_fn () >>= fun (_, _, _, balances) ->
    let z_usd_balance = Hashtbl.find_opt balances "ZUSD" |> Option.value ~default:0.0 in
    let usd_balance_val = Hashtbl.find_opt balances "USD" |> Option.value ~default:0.0 in
    let new_balance = z_usd_balance +. usd_balance_val in
    usd_balance := new_balance;
    if Hashtbl.length balances = 0 then
      warning_f ~section "No balance data received from WebSocket, USD balance may be stale: %.2f" new_balance
    else
      info_f ~section "Refreshed USD balance: %.2f (from %d balance entries)" new_balance (Hashtbl.length balances)

  (** Get current USD balance *)
  let get_usd_balance () = !usd_balance

  (** Get current price data for symbol *)
  let get_price symbol = Hashtbl.find_opt price_info symbol

  (** Store latest price tick data *)
  let update_price (tick : Event.tick) =
    Hashtbl.replace price_info tick.symbol tick;
    update_global_price tick.symbol tick.current_price;
    Lwt.return_unit

  (** Sync local order state with exchange *)
  let sync_open_orders get_exchange_orders_fn () =
    let exchange_orders = get_exchange_orders_fn () in
    Hashtbl.clear open_orders;
    Hashtbl.iter (fun order_id (order : order) ->
      Hashtbl.add open_orders order_id order
    ) exchange_orders;
    Lwt.return_unit

  (** Check if symbol has any open buy orders *)
  let has_open_buy_order symbol =
    let buy_orders = Hashtbl.fold (fun _ (order : order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Buy then
        order :: acc
      else
        acc
    ) open_orders [] in
    List.length buy_orders > 0

  (** Check if symbol has any pending buy orders *)
  let has_pending_buy_order symbol =
    Hashtbl.fold (fun _ (sym, side) acc ->
      acc || (String.equal sym symbol && side = Core.Buy)
    ) pending_orders false

  (** Add a pending order *)
  let add_pending_order client_id symbol side =
    Hashtbl.replace pending_orders client_id (symbol, side)

  (** Remove a pending order when acknowledged *)
  let remove_pending_order client_id =
    Hashtbl.remove pending_orders client_id

  (** Check if symbol has any open sell orders *)
  let has_open_sell_order symbol =
    let sell_orders = Hashtbl.fold (fun _ (order : order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Sell then
        order :: acc
      else
        acc
    ) open_orders [] in
    List.length sell_orders > 0

  (** Get total quantity of open sell orders for symbol *)
  let get_open_sell_order_qty symbol =
    Hashtbl.fold (fun _ (order : order) acc ->
      if String.equal order.order_symbol symbol && order.side = Some Core.Sell then
        acc +. order.qty
      else
        acc
    ) open_orders 0.0

  (** Get total quantity of an asset locked in open orders for a given symbol *)
  let get_balance_in_open_orders symbol =
    Hashtbl.fold (fun _ (order : order) acc ->
      if String.equal order.order_symbol symbol then
        acc +. order.qty
      else
        acc
    ) open_orders 0.0

  (** Create exchange-compliant order with proper formatting *)
  let create_order ~symbol ~side ~price ~qty ~get_precisions_fn =
    match get_precisions_fn symbol with
    | Some (price_prec, qty_prec) ->
        let price_str = Primitives.Price.to_string price in
        let qty_str = Primitives.Qty.to_string qty in
        let formatted_price = Primitives.Price.of_string_exn ~scale:price_prec price_str in
        let formatted_qty = Primitives.Qty.of_string_exn ~scale:qty_prec qty_str in
        let client_id = generate_unique_client_id side in
        let order = Core.Add {
          dst = "kraken";
          client_id;
          symbol;
          side;
          price = formatted_price;
          qty = formatted_qty;
          tif = GTC;
          tags = [];
        } in
        Some order
    | None ->
        None

  (** Initialize orders from exchange state *)
  let initialize_orders (runtime_cfg : Config.runtime_cfg) strategy_type get_exchange_orders_fn =
    let orderbook_symbols = List.filter_map (fun (asset: Config.asset_cfg) ->
      match asset.strategy with
      | strategy when strategy = strategy_type -> Some asset.symbol
      | _ -> None
    ) runtime_cfg.assets in
    let exchange_orders = get_exchange_orders_fn () in
    Hashtbl.clear open_orders;
    let log_promises = Hashtbl.fold (fun order_id (order : order) promises ->
      let log_promise =
        if List.mem order.order_symbol orderbook_symbols then (
          Hashtbl.replace open_orders order_id order;
          info_f ~section "Loaded existing order %s for %s" order_id order.order_symbol
        ) else
          Lwt.return_unit
      in
      log_promise :: promises
    ) exchange_orders [] in
    Lwt.join log_promises >>= fun () ->
    info_f ~section "Initialized %d open orders from exchange" (Hashtbl.length open_orders)

  (** Utility functions for market making strategies *)

  (** Get formatted price string for display *)
  let format_price price =
    Primitives.Price.to_string price

  (** Get formatted quantity string for display *)
  let format_qty qty =
    Primitives.Qty.to_string qty

  (** Calculate price difference percentage *)
  let price_diff_pct old_price new_price =
    let old_float = Float.of_string (format_price old_price) in
    let new_float = Float.of_string (format_price new_price) in
    let diff = abs_float (old_float -. new_float) in
    if old_float > 0.0 then diff /. old_float else 0.0

  (** Check if price difference exceeds threshold *)
  let price_diff_exceeds_threshold old_price new_price threshold =
    price_diff_pct old_price new_price > threshold

  (** Get asset configuration for a symbol *)
  let get_asset_config (runtime_cfg : Config.runtime_cfg) symbol =
    List.find_opt (fun (asset: Config.asset_cfg) ->
      String.equal asset.symbol symbol
    ) runtime_cfg.assets

  (** Get instrument data for a symbol *)
  let get_instrument get_instrument_fn symbol =
    get_instrument_fn symbol

  (** Get precisions for a symbol *)
  let get_precisions get_precisions_fn symbol =
    get_precisions_fn symbol

  (** Get all open orders from exchange *)
  let get_all_open_orders get_all_open_orders_fn () =
    get_all_open_orders_fn ()

  (** Wait for WebSocket snapshot *)
  let wait_for_snapshot wait_for_snapshot_fn () =
    wait_for_snapshot_fn ()

  (** Wait for instruments data *)
  let wait_for_instruments wait_for_instruments_fn () =
    wait_for_instruments_fn ()

  (** Create a standardized order with proper formatting for Kraken *)
  let create_standard_order ~symbol ~side ~price ~qty ~get_precisions_fn =
    match get_precisions_fn symbol with
    | Some (price_prec, qty_prec) ->
        let price_str = format_price price in
        let qty_str = format_qty qty in
        let formatted_price = Primitives.Price.of_string_exn ~scale:price_prec price_str in
        let formatted_qty = Primitives.Qty.of_string_exn ~scale:qty_prec qty_str in
        let client_id = generate_unique_client_id side in
        let order = Core.Add {
          dst = "kraken";
          client_id;
          symbol;
          side;
          price = formatted_price;
          qty = formatted_qty;
          tif = GTC;
          tags = [];
        } in
        Some order
    | None ->
        None

  (*
    Higher-level Strategy Operations
  *)

  (** Get available balance for a symbol considering open orders *)
  let get_available_balance symbol get_instrument_fn get_balances_fn =
    match get_instrument_fn symbol with
    | Some instrument ->
        let base_currency = instrument.base in
        get_balances_fn () >>= fun (spot_balances, _, liquid_balances, _) ->
        let spot_bal = Hashtbl.find_opt spot_balances base_currency |> Option.value ~default:0.0 in
        let liquid_bal = Hashtbl.find_opt liquid_balances base_currency |> Option.value ~default:0.0 in
        let total_balance = spot_bal +. liquid_bal in
        let balance_in_orders = get_balance_in_open_orders symbol in
        Lwt.return (total_balance -. balance_in_orders, Some instrument)
    | None ->
        Lwt.return (0.0, None)

  (** Create a sell order for remaining balance before pausing strategy *)
  let create_final_sell_order ~symbol ~cmd_buffer ~section ~get_instrument_fn ~get_precisions_fn ~get_balances_fn =
    get_available_balance symbol get_instrument_fn get_balances_fn >>= fun (available_balance, instrument_opt) ->
    match instrument_opt with
    | Some instrument ->
        if available_balance > 0.00001 then (
          let qty_prec = instrument.qty_precision in
          (* Round down to qty_precision to exclude dust fractions *)
          let clean_qty = floor (available_balance *. 10.0 ** float_of_int qty_prec) /. (10.0 ** float_of_int qty_prec) in
          match get_price symbol with
          | Some tick ->
              let sell_price = tick.ask in
              let qty_str = Printf.sprintf "%.*f" qty_prec clean_qty in
              let sell_qty = Primitives.Qty.of_string_exn ~scale:qty_prec qty_str in
              begin match create_order ~symbol ~side:Sell ~price:sell_price ~qty:sell_qty
                      ~get_precisions_fn with
              | Some sell_cmd ->
                  Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
                  info_f ~section "Placed final sell order for %s." symbol
              | None ->
                  error_f ~section "Failed to create final sell order for %s." symbol
              end
          | None ->
              warning_f ~section "No price info for %s, cannot place final sell order." symbol
        ) else (
          Lwt.return_unit
        )
    | None ->
        warning_f ~section "No instrument data for %s, cannot place final sell order." symbol

  (** Create buy/sell order pair at top-of-book prices *)
  let create_top_of_book_orders ~symbol ~qty ~cmd_buffer ~section ~get_precisions_fn =
    match get_price symbol with
    | Some tick ->
        let buy_price = tick.bid in
        let sell_price = tick.ask in
        let buy_order = create_order ~symbol ~side:Buy ~price:buy_price ~qty
                        ~get_precisions_fn in
        let sell_order = create_order ~symbol ~side:Sell ~price:sell_price ~qty
                         ~get_precisions_fn in
        (match buy_order, sell_order with
        | Some buy_cmd, Some sell_cmd ->
            info_f ~section "Successfully created both orders for %s, pushing to buffer" symbol >>= fun () ->
            Ringbuffer.push cmd_buffer buy_cmd >>= fun () ->
            info_f ~section "Buy order pushed to buffer for %s" symbol >>= fun () ->
            Ringbuffer.push cmd_buffer sell_cmd >>= fun () ->
            info_f ~section "Sell order pushed to buffer for %s" symbol
        | Some _, None ->
            error_f ~section "Failed to create sell order for %s" symbol >>= fun () ->
            Lwt.return_unit
        | None, Some _ ->
            error_f ~section "Failed to create buy order for %s" symbol >>= fun () ->
            Lwt.return_unit
        | None, None ->
            error_f ~section "Failed to create both orders for %s" symbol >>= fun () ->
            Lwt.return_unit)
    | None ->
        warning_f ~section "No price info for %s" symbol

  (** Handle order amendment with rate limiting *)
  let amend_order_if_needed ~order ~new_price ~cmd_buffer ~section =
    let top_bid_price_float = Float.of_string (format_price new_price) in
    let order_price_float = order.limit_price in
    let price_diff = abs_float (order_price_float -. top_bid_price_float) in
    let price_diff_pct = if order_price_float > 0.0 then price_diff /. order_price_float else 0.0 in

    if price_diff_pct > 0.0001 then ( (* 0.01% threshold *)
      let now = Unix.gettimeofday () in
      let last_amend = Hashtbl.find_opt last_amend_time order.order_id |> Option.value ~default:0.0 in
      let time_since_last_amend = now -. last_amend in

      if time_since_last_amend >= amend_cooldown then (
        info_f ~section "Prices differ, creating amend command for order %s (%.8f -> %.8f)"
          order.order_id order_price_float top_bid_price_float >>= fun () ->
        let amend_cmd = Core.Amend {
          dst = "kraken";
          order_id = order.order_id;
          symbol = order.order_symbol;
          new_price = new_price;
          new_qty = Primitives.Qty.of_string_exn ~scale:8 (Printf.sprintf "%.8f" order.qty);
          ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
        } in
        Ringbuffer.push cmd_buffer amend_cmd >>= fun () ->
        Hashtbl.replace last_amend_time order.order_id now;
        info_f ~section "Amending order %s to new price %s" order.order_id (format_price new_price)
      ) else (
        debug_f ~section "Skipping amend for order %s - cooldown active (%.1fs remaining)"
          order.order_id (amend_cooldown -. time_since_last_amend)
      )
    ) else (
      Lwt.return_unit
    )

  (** Generalized order amendment with custom price calculation and optional post-amend callback *)
  let amend_order_with_callback ~order ~new_price ~cmd_buffer ~section ~qty_precision ~post_amend_callback =
    let top_bid_price_float = Float.of_string (format_price new_price) in
    let order_price_float = order.limit_price in
    let price_diff = abs_float (order_price_float -. top_bid_price_float) in
    let price_diff_pct = if order_price_float > 0.0 then price_diff /. order_price_float else 0.0 in

    if price_diff_pct > 0.0001 then ( (* 0.01% threshold *)
      let now = Unix.gettimeofday () in
      let last_amend = Hashtbl.find_opt last_amend_time order.order_id |> Option.value ~default:0.0 in
      let time_since_last_amend = now -. last_amend in

      if time_since_last_amend >= amend_cooldown then (
        info_f ~section "Prices differ, creating amend command for order %s (%.8f -> %.8f)"
          order.order_id order_price_float top_bid_price_float >>= fun () ->
        let amend_cmd = Core.Amend {
          dst = "kraken";
          order_id = order.order_id;
          symbol = order.order_symbol;
          new_price = new_price;
          new_qty = Primitives.Qty.of_string_exn ~scale:qty_precision (Printf.sprintf "%.*f" qty_precision order.qty);
          ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
        } in
        Ringbuffer.push cmd_buffer amend_cmd >>= fun () ->
        Hashtbl.replace last_amend_time order.order_id now;
        post_amend_callback () >>= fun () ->
        info_f ~section "Amending order %s to new price %s" order.order_id (format_price new_price)
      ) else (
        debug_f ~section "Skipping amend for order %s - cooldown active (%.1fs remaining)"
          order.order_id (amend_cooldown -. time_since_last_amend)
      )
    ) else (
      Lwt.return_unit
    )

  (** Handle order fill events and recreate orders as needed *)
  let handle_order_fill ~order_id ~symbol ~price ~qty ~side ~cmd_buffer ~runtime_cfg ~section ~handle_fill_fn ~sync_orders_fn ~refresh_balance_fn ~create_orders_fn =
    match Hashtbl.find_opt open_orders order_id with
    | Some order ->
        (* Handle fill event with balance updates *)
        let fill_event = {
          Event.src = "kraken";
          symbol = order.order_symbol;
          order_id;
          side = side;
          qty = qty;
          price = price;
          ts = Unix.time () |> Int64.of_float;
        } in
        handle_fill_fn fill_event >>= fun () ->
        sync_orders_fn () >>= fun () ->
        refresh_balance_fn () >>= fun () ->

        if not (Hashtbl.mem open_orders order_id) then (
          info_f ~section "Order %s completely filled" order_id >>= fun () ->
          (* Only create new orders if it was a buy order that was filled *)
          if side = `Buy then (
            match get_asset_config runtime_cfg symbol with
            | Some asset_cfg ->
                create_orders_fn ~symbol ~qty:asset_cfg.qty ~cmd_buffer ~section
            | None ->
                warning_f ~section "No configuration found for %s" symbol
          ) else (
            Lwt.return_unit
          )
        ) else (
          Lwt.return_unit
        )
    | None ->
        Lwt.return_unit

end
