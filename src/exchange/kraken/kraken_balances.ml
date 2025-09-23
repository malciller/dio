(**
 * Kraken Balances Module
 *
 * Manages real-time balance tracking via WebSocket feed and maintains transaction history.
 * Handles balance snapshots, updates, and trade-related events while preventing race conditions.
 *)

open Lwt.Infix
open Lwt_log_core
open Dio_types

(** Ringbuffer telemetry interface for this module *)
module RingbufferTelemetryInterface = struct
  let set_functions = Ringbuffer.TelemetryInterface.set_functions
end

(** Mutex for balance state synchronization *)
let balance_mutex = Lwt_mutex.create ()

(** Ringbuffer for ordered balance update processing *)
let balance_update_queue : (string * Yojson.Safe.t) Ringbuffer.t = Ringbuffer.create ~name:"balance_update_queue" 1000

(** Ringbuffer for ordered fill event processing *)
let fill_event_queue : Event.fill Ringbuffer.t = Ringbuffer.create ~name:"fill_event_queue" 1000

let section = Section.make "kraken_balances"

(** Balance storage by wallet type *)
let spot_balances : (string, float) Hashtbl.t = Hashtbl.create(16)
let earn_balances : (string, float) Hashtbl.t = Hashtbl.create(16)
let liquid_balances : (string, float) Hashtbl.t = Hashtbl.create(16)
let balances : (string, float) Hashtbl.t = Hashtbl.create(16) (** Aggregated spot + earn balances *)

(** Previous balances for change detection *)
let previous_balances : (string, float) Hashtbl.t = Hashtbl.create(16)

(** Condition signaled when initial balance snapshot is received *)
let balances_initialized = Lwt_condition.create ()

(** Flag indicating if balances have been initialized from WebSocket *)
let balances_ready = ref false

(** Trading performance tracking *)
let orders_filled = ref 0
let orders_partially_filled = ref 0
let orders_not_filled = ref 0
let total_fill_ratio = ref 0.0
let total_pnl = ref 0.0
let realized_pnl = ref 0.0

(** Record trading performance metrics *)
let record_trading_performance () =
  let total_orders = !orders_filled + !orders_partially_filled + !orders_not_filled in
  let fill_rate = if total_orders > 0 then Float.of_int !orders_filled /. Float.of_int total_orders else 0.0 in
  let partial_fill_rate = if total_orders > 0 then Float.of_int !orders_partially_filled /. Float.of_int total_orders else 0.0 in

  !Ringbuffer.telemetry_record_gauge ["trading"; "performance"] "fill_rate" fill_rate >>= fun () ->
  !Ringbuffer.telemetry_record_gauge ["trading"; "performance"] "partial_fill_rate" partial_fill_rate >>= fun () ->
  !Ringbuffer.telemetry_record_gauge ["trading"; "performance"] "total_pnl" !total_pnl >>= fun () ->
  !Ringbuffer.telemetry_record_gauge ["trading"; "performance"] "realized_pnl" !realized_pnl >>= fun () ->
  !Ringbuffer.telemetry_record_counter ["trading"; "performance"] "orders_filled" !orders_filled >>= fun () ->
  !Ringbuffer.telemetry_record_counter ["trading"; "performance"] "orders_partially_filled" !orders_partially_filled >>= fun () ->
  !Ringbuffer.telemetry_record_counter ["trading"; "performance"] "orders_not_filled" !orders_not_filled

(** Classify balance change type for transaction categorization *)
let classify_balance_change asset old_balance new_balance =
  let diff = new_balance -. old_balance in
  debug_f ~section "Classifying balance change for %s: %.8f -> %.8f (diff: %.8f)"
    asset old_balance new_balance diff >>= fun () ->
  if abs_float diff < 0.000001 then
    Lwt.return_none (* No significant change *)
  else if diff > 0.0 then
    (* Positive change - could be deposit, staking reward, or trade *)
    if old_balance = 0.0 then
      Lwt.return_some (`Deposit, diff) (* New asset *)
    else
      Lwt.return_some (`Credit, diff) (* Generic credit until we can classify better *)
  else
    (* Negative change - could be withdrawal, fee, or trade *)
    Lwt.return_some (`Debit, diff)

(** Process balance change and create transaction record *)
let process_balance_change asset old_balance new_balance =
  let%lwt classification = classify_balance_change asset old_balance new_balance in
  match classification with
  | Some (change_type, amount) ->
      let tx_type = match change_type with
        | `Deposit -> Primitives.Deposit
        | `Credit -> Primitives.Staking_Reward (* Assume staking reward for now *)
        | `Debit -> Primitives.Fee (* Assume fee for now *)
      in
      let tx = {
        Primitives.id = Primitives.Id.gen ();
        asset;
        amount;
        timestamp = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
        transaction_type = tx_type;
        cost_basis = None; (* Will be updated if we can determine price *)
        total_cost = None;
        balance_after = new_balance;
      } in

      let tx = if tx_type = Primitives.Deposit || tx_type = Primitives.Staking_Reward then (
        let pair = asset ^ "/USD" in
        match Hashtbl.find_opt Dio_types.State.global_state.prices pair with
        | Some pi ->
            let p = pi.price in
            let price_float = Float.of_string (Primitives.Price.to_string p) in
            { tx with
              cost_basis = Some price_float;
              total_cost = Some (price_float *. amount);
            }
        | None -> tx
      ) else tx in

      let%lwt _ = Transaction_history.add_transaction tx in
      debug_f ~section "Balance change detected: %s %.8f (%s)"
        asset amount
        (match change_type with
         | `Deposit -> "DEPOSIT"
         | `Credit -> "CREDIT"
         | `Debit -> "DEBIT")
  | None ->
      debug_f ~section "No significant balance change for %s" asset

(** Process initial balance snapshot from WebSocket feed *)
let handle_balance_snapshot data =
  Lwt_mutex.with_lock balance_mutex (fun () ->
    let open Yojson.Safe.Util in
    Hashtbl.clear previous_balances;
    Hashtbl.iter (fun asset balance -> Hashtbl.replace previous_balances asset balance) balances;
    Hashtbl.clear balances;
    Hashtbl.clear spot_balances;
    Hashtbl.clear earn_balances;
    Hashtbl.clear liquid_balances;

    data |> to_list |> Lwt_list.iter_s (fun item ->
      let asset = item |> member "asset" |> to_string in
      let wallets = item |> member "wallets" |> to_list in

      Lwt_list.iter_s (fun wallet ->
        let wallet_type = wallet |> member "type" |> to_string in
        let wallet_id = wallet |> member "id" |> to_string in
        let balance = wallet |> member "balance" |> to_float in

        (match wallet_type with
        | "spot" when wallet_id = "main" -> Hashtbl.replace spot_balances asset balance
        | "earn" ->
            (match wallet_id with
            | "liquid" -> Hashtbl.replace liquid_balances asset balance
            | _ -> (* bonded, flexible, locked *)
                let current_earn = Hashtbl.find_opt earn_balances asset |> Option.value ~default:0.0 in
                Hashtbl.replace earn_balances asset (current_earn +. balance)
            )
        | _ -> ()
        );
        Lwt.return_unit
      ) wallets >>= fun () ->

      let spot = Hashtbl.find_opt spot_balances asset |> Option.value ~default:0.0 in
      let earn = Hashtbl.find_opt earn_balances asset |> Option.value ~default:0.0 in
      Hashtbl.replace balances asset (spot +. earn);

      debug_f ~section "Balance snapshot for %s: spot=%.8f, earn=%.8f, liquid=%.8f, total=%.8f"
        asset spot earn
        (Hashtbl.find_opt liquid_balances asset |> Option.value ~default:0.0)
        (spot +. earn)
    ) >>= fun () ->
    balances_ready := true;
    Lwt_condition.broadcast balances_initialized ();
    info_f ~section "Processed balance snapshot. %d assets loaded." (Hashtbl.length balances)
  )

(** Process incremental balance updates from WebSocket feed *)
let handle_balance_update data =
  let open Yojson.Safe.Util in
  data |> to_list |> Lwt_list.iter_s (fun item ->
    debug_f ~section "Full balance update item: %s" (Yojson.Safe.to_string item) >>= fun () ->

    let asset = item |> member "asset" |> to_string in
    Ringbuffer.push balance_update_queue (asset, item) >>= fun () ->
    debug_f ~section "Pushed balance update for %s to ringbuffer" asset
  )

(** Process balance updates from ringbuffer in FIFO order *)
let process_balance_update_from_queue (asset, item) =
  Lwt_mutex.with_lock balance_mutex (fun () ->
    let open Yojson.Safe.Util in
    debug_f ~section "Processing balance update for %s from ringbuffer" asset >>= fun () ->

    let event_type = item |> member "type" |> to_string in
    let subtype_opt = try Some (item |> member "subtype" |> to_string) with _ -> None in

    let is_trade_related =
      let category = try Some (item |> member "category" |> to_string) with _ -> None in
      let wallet_type = try Some (item |> member "wallet_type" |> to_string) with _ -> None in
      let is_internal_transfer = event_type = "transfer" || (
        event_type = "earn" && match subtype_opt with
        | Some subtype -> List.mem subtype ["deposit"; "withdrawal"]
        | None -> false
      )
      in
      let is_spot_trade = event_type = "trade" && wallet_type = Some "spot" in
      let is_staking_trade = event_type = "trade" && category = Some "trade" && wallet_type = Some "earn" in
      is_internal_transfer || is_spot_trade || is_staking_trade
    in

    match item |> member "balance" |> to_float_option with
    | None ->
        warning_f ~section "Balance update for %s has no 'balance' field. Cannot process update: %s" asset (Yojson.Safe.to_string item)
    | Some new_balance ->
        let wallet_type = item |> member "wallet_type" |> to_string_option |> Option.value ~default:"" in
        let wallet_id = item |> member "wallet_id" |> to_string_option |> Option.value ~default:"" in

        (match wallet_type with
        | "spot" when wallet_id = "main" -> Hashtbl.replace spot_balances asset new_balance
        | "earn" ->
            (match wallet_id with
            | "liquid" -> Hashtbl.replace liquid_balances asset new_balance
            | _ -> (* This assumes the update provides the new total for the specific earn wallet, not a delta *)
               Hashtbl.replace earn_balances asset new_balance
            )
        | _ -> ()
        );

        let spot = Hashtbl.find_opt spot_balances asset |> Option.value ~default:0.0 in
        let earn = Hashtbl.find_opt earn_balances asset |> Option.value ~default:0.0 in
        let new_total_balance = spot +. earn in
        Hashtbl.replace balances asset new_total_balance;

        let amount = item |> member "amount" |> to_float in (* amount is still useful for tx creation *)

        if is_trade_related then
          debug_f ~section "Skipping tx generation for trade-related event on %s, balance updated to %.8f" asset new_total_balance
        else
          let tx_type =
            match event_type with
            | "earn" | "staking" -> if amount > 0.0 then Primitives.Staking_Reward else Primitives.Withdrawal
            | "deposit" -> Primitives.Deposit
            | "withdrawal" -> Primitives.Withdrawal
            | "trade" -> if amount > 0.0 then Primitives.Deposit else Primitives.Withdrawal (* Treat non-spot trades as deposit/withdrawal *)
            | _ -> if amount > 0.0 then Primitives.Deposit else Primitives.Fee (* Fallback for unknown types *)
          in
          let tx = {
            Primitives.id = Primitives.Id.gen ();
            asset;
            amount;
            timestamp = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
            transaction_type = tx_type;
            cost_basis = None;
            total_cost = None;
            balance_after = new_total_balance;
          } in

          let final_tx =
            if tx_type = Primitives.Staking_Reward || tx_type = Primitives.Deposit then
              let pair = asset ^ "/USD" in
              match Hashtbl.find_opt Dio_types.State.global_state.prices pair with
              | Some pi ->
                  let p = pi.price in
                  let price_float = Float.of_string (Primitives.Price.to_string p) in
                  { tx with cost_basis = Some price_float; total_cost = Some (price_float *. amount) }
              | None -> tx
            else
              tx
          in
          Transaction_history.add_transaction final_tx
  )

(** Process fill events from ringbuffer and create transaction records *)
let process_fill_event_from_queue fill =
  Lwt_mutex.with_lock balance_mutex (fun () ->
    let open Event in
    debug_f ~section "Processing fill event for %s from ringbuffer" fill.symbol >>= fun () ->

    let base_tx = Transaction_history.transaction_from_fill fill in
    let%lwt _ = Transaction_history.add_transaction base_tx in

    (* Update trading performance metrics *)
    (* All fills processed here are actual fills, so count them as filled *)
    incr orders_filled;
    Lwt.async (fun () -> record_trading_performance ());

    (* Calculate PnL impact for this fill *)
    let price_float = Float.of_string (Primitives.Price.to_string fill.price) in
    let qty_float = Float.of_string (Primitives.Qty.to_string fill.qty) in
    let pnl_impact = match fill.side with
      | `Buy -> price_float *. qty_float  (* Cost of purchase *)
      | `Sell -> price_float *. qty_float  (* Revenue from sale *)
    in
    total_pnl := !total_pnl +. pnl_impact;
    Lwt.async (fun () ->
      !Ringbuffer.telemetry_record_gauge ["trading"; "performance"] "total_pnl" !total_pnl
    );

    Lwt.return_unit
  )

(** Start background processors for balance updates and fill events *)
let start_balance_update_processor () =
  (* Register event-driven processors *)
  Ringbuffer.create_consumer balance_update_queue ~name:"balance_processor" 
    ~processor:process_balance_update_from_queue;
  
  Ringbuffer.create_consumer fill_event_queue ~name:"fill_processor" 
    ~processor:process_fill_event_from_queue;
  
  Lwt.async (fun () -> info ~section "Started event-driven balance update and fill event processors");
  ()

(** Route WebSocket messages to appropriate balance handlers *)
let handle_balances_message msg =
  Lwt.catch (fun () ->
    let json = Yojson.Safe.from_string msg in
    let open Yojson.Safe.Util in
    (* Check for subscription confirmation messages first *)
    match member "method" json |> to_string_option with
    | Some "subscribe" ->
        let success = member "success" json |> to_bool_option |> Option.value ~default:false in
        let error = member "error" json |> to_string_option in
        if success then
          let channel = member "result" json |> member "channel" |> to_string_option |> Option.value ~default:"unknown" in
          debug_f ~section "Subscription successful for channel: %s" channel
        else
          let error_msg = Option.value error ~default:"unknown error" in
          warning_f ~section "Subscription failed: %s. Payload: %s" error_msg msg
    | _ ->
        (* Handle data messages by channel *)
        match member "channel" json |> to_string_option with
        | Some "balances" ->
            (match member "type" json |> to_string_option with
            | Some "snapshot" -> handle_balance_snapshot (member "data" json)
            | Some "update" -> handle_balance_update (member "data" json)
            | _ -> warning_f ~section "Unhandled message type on balances channel: %s" msg
            )
        | Some channel ->
            if channel <> "heartbeat" then
              debug_f ~section "Received non-balances message on channel %s: %s" channel msg
            else
              Lwt.return_unit
        | None ->
            debug_f ~section "Received message without channel: %s" msg
  ) (fun exn ->
    error_f ~section "JSON parsing error in balances message: %s (%s)" msg (Printexc.to_string exn) >>= fun () ->
    Lwt.return_unit
  )

(** Subscribe to Kraken balances WebSocket channel *)
let subscribe_to_balances conn token =
  let sub_msg =
    `Assoc [
      ("method", `String "subscribe");
      ("params", `Assoc [
        ("channel", `String "balances");
        ("token", `String token)
      ])
    ] |> Yojson.Safe.to_string
  in
  let frame = Websocket.Frame.create ~content:sub_msg () in
  Websocket_lwt_unix.write conn frame

(** Listen for balance messages on WebSocket connection *)
let rec listen_for_balances conn =
  Websocket_lwt_unix.read conn >>= fun frame ->
  let content = frame.content in
  handle_balances_message content >>= fun () ->
  listen_for_balances conn

(** Initialize WebSocket connection for real-time balance updates *)
let initialize_ws_balances_feed (cfg : Config.engine_config) (token : string) =
  start_balance_update_processor ();

  Lwt.async (fun () ->
    let rec connect_and_listen () =
      Lwt.catch
        (fun () ->
          info_f ~section "Connecting to Kraken balances WebSocket..." >>= fun () ->
          let port = cfg.ws_port in
          let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
          let connect_host = "ws-auth.kraken.com" in
          let path = "/v2" in
          let uri = Uri.of_string (Printf.sprintf "wss://%s:%d%s" connect_host port path) in

          Lwt_unix.getaddrinfo connect_host (string_of_int port) [Unix.(AI_FAMILY PF_INET)] >>= fun addrs ->
          match addrs with
          | { Unix.ai_addr = Unix.ADDR_INET (ip_addr_from_dns, _); _ } :: _ ->
              let ip_to_use = Ipaddr.of_string_exn (Unix.string_of_inet_addr ip_addr_from_dns) in
              let tls_config = `Hostname connect_host, `IP ip_to_use, `Port port in
              let endpoint = `TLS tls_config in
              Websocket_lwt_unix.connect ~ctx endpoint uri >>= fun conn ->
              info_f ~section "Connected to Kraken balances WebSocket." >>= fun () ->
              subscribe_to_balances conn token >>= fun () ->
              listen_for_balances conn
          | _ -> Lwt.fail_with (Printf.sprintf "Failed to resolve host: %s" connect_host)
        )
        (fun exn ->
          error_f ~section "Balances WebSocket error: %s. Reconnecting in 10s..." (Printexc.to_string exn) >>= fun () ->
          Lwt_unix.sleep 10.0 >>= fun () ->
          connect_and_listen ()
        )
    in
    connect_and_listen ()
  )

(** Queue fill events for processing and transaction record creation *)
let handle_fill_event fill =
  Ringbuffer.push fill_event_queue fill >>= fun () ->
  debug_f ~section "Pushed fill event for %s to ringbuffer" fill.symbol

(** Initialize transaction history with current balance state *)
let initialize_transaction_history () =
  if !balances_ready then
    let balance_list = Hashtbl.to_seq balances |> List.of_seq in
    Lwt_list.iter_s (fun (asset, balance) ->
      if balance > 0.000001 then
        Transaction_history.initialize_from_balance asset balance >>= fun () ->
        info_f ~section "Initialized transaction history for %s with balance %.8f" asset balance
      else
        Lwt.return_unit
    ) balance_list >>= fun () ->
    info_f ~section "Initialized transaction history for all assets"
  else
    warning_f ~section "Cannot initialize transaction history - balances not ready"

(** Wait for balance initialization with timeout, return current balance state *)
let wait_for_balances () =
  if !balances_ready then (
    Lwt.return (Hashtbl.copy spot_balances, Hashtbl.copy earn_balances, Hashtbl.copy liquid_balances, Hashtbl.copy balances)
  ) else (
    let timeout = 30.0 in
    Lwt.pick [
      (Lwt_condition.wait balances_initialized >>= fun () ->
       info_f ~section "Balance WebSocket initialized, received initial snapshot" >>= fun () ->
       Lwt.return (Hashtbl.copy spot_balances, Hashtbl.copy earn_balances, Hashtbl.copy liquid_balances, Hashtbl.copy balances));
      (Lwt_unix.sleep timeout >>= fun () ->
       warning_f ~section "Balance WebSocket initialization timeout after %.1f seconds, returning empty balances" timeout >>= fun () ->
       Lwt.return (Hashtbl.create 16, Hashtbl.create 16, Hashtbl.create 16, Hashtbl.create 16))
    ]
  )
