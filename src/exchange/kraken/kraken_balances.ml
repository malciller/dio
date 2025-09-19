(* src/exchange/kraken/kraken_balances.ml *)

open Lwt.Infix
open Lwt_log_core
open Dio_types

(* Ringbuffer for balance updates to prevent race conditions *)
let balance_update_queue : (string * Yojson.Safe.t) Ringbuffer.t = Ringbuffer.create 1000

(* Ringbuffer for fill events to prevent race conditions *)
let fill_event_queue : Event.fill Ringbuffer.t = Ringbuffer.create 1000

let section = Section.make "kraken.balances"

(* In-memory store for balances *)
let spot_balances : (string, float) Hashtbl.t = Hashtbl.create(16)
let earn_balances : (string, float) Hashtbl.t = Hashtbl.create(16)
let liquid_balances : (string, float) Hashtbl.t = Hashtbl.create(16)
let balances : (string, float) Hashtbl.t = Hashtbl.create(16) (* Aggregated: spot + earn *)

(* Previous balance snapshots for change detection *)
let previous_balances : (string, float) Hashtbl.t = Hashtbl.create(16)

(* Condition to signal when the initial balance snapshot is received *)
let balances_initialized = Lwt_condition.create ()

(* Track whether balances have been initialized *)
let balances_ready = ref false

(* Detect balance change type *)
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

(* Process balance change and create transaction record *)
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

let handle_balance_snapshot data =
  let open Yojson.Safe.Util in
  (* Clear all balance tables *)
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

    (* Update aggregated balance *)
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

let handle_balance_update data =
  let open Yojson.Safe.Util in
  data |> to_list |> Lwt_list.iter_s (fun item ->
    debug_f ~section "Full balance update item: %s" (Yojson.Safe.to_string item) >>= fun () ->

    let asset = item |> member "asset" |> to_string in

    (* Push balance update to ringbuffer for ordered processing *)
    Ringbuffer.push balance_update_queue (asset, item) >>= fun () ->
    debug_f ~section "Pushed balance update for %s to ringbuffer" asset
  )

(* Process balance updates from the ringbuffer in order *)
let process_balance_update_from_queue () =
  Ringbuffer.pop balance_update_queue >>= fun (asset, item) ->
  let open Yojson.Safe.Util in
  debug_f ~section "Processing balance update for %s from ringbuffer" asset >>= fun () ->

  let event_type = item |> member "type" |> to_string in
  let subtype_opt = try Some (item |> member "subtype" |> to_string) with _ -> None in

  (* Trade-related events are handled via the fill feed, so we skip tx generation here
     but MUST still update the balance correctly using the amount field. *)
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

      (* Update specific wallet balance *)
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

      (* Recalculate and update aggregated balance *)
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

(* Process fill events from the ringbuffer *)
let process_fill_event_from_queue () =
  Ringbuffer.pop fill_event_queue >>= fun fill ->
  let open Event in
  debug_f ~section "Processing fill event for %s from ringbuffer" fill.symbol >>= fun () ->

  let base_tx = Transaction_history.transaction_from_fill fill in

  let base_asset, quote_asset_opt =
    try
      let idx = String.index fill.symbol '/' in
      let base = String.sub fill.symbol 0 idx in
      let quote = String.sub fill.symbol (idx + 1) (String.length fill.symbol - idx - 1) in
      (base, Some quote)
    with Not_found -> (fill.symbol, None)
  in

  (* Update base asset spot balance *)
  let old_base_balance = Hashtbl.find_opt spot_balances base_asset |> Option.value ~default:0.0 in
  let new_base_spot_balance = old_base_balance +. base_tx.amount in
  Hashtbl.replace spot_balances base_asset new_base_spot_balance;

  (* Recalculate aggregated balance for base asset *)
  let base_earn_balance = Hashtbl.find_opt earn_balances base_asset |> Option.value ~default:0.0 in
  let new_base_total_balance = new_base_spot_balance +. base_earn_balance in
  Hashtbl.replace balances base_asset new_base_total_balance;

  let updated_base_tx = { base_tx with
    asset = base_asset;
    balance_after = new_base_total_balance;
  } in
  let%lwt _ = Transaction_history.add_transaction updated_base_tx in

  (match quote_asset_opt with
  | Some quote_asset ->
      let qty_float = float_of_string (Primitives.Qty.to_string fill.qty) in
      let price_float = float_of_string (Primitives.Price.to_string fill.price) in
      let quote_amount = qty_float *. price_float in
      let quote_tx_amount = match fill.side with
        | `Buy -> -.quote_amount
        | `Sell -> quote_amount
      in

      (* Update quote asset spot balance *)
      let old_quote_balance = Hashtbl.find_opt spot_balances quote_asset |> Option.value ~default:0.0 in
      let new_quote_spot_balance = old_quote_balance +. quote_tx_amount in
      Hashtbl.replace spot_balances quote_asset new_quote_spot_balance;

      (* Recalculate aggregated balance for quote asset *)
      let quote_earn_balance = Hashtbl.find_opt earn_balances quote_asset |> Option.value ~default:0.0 in
      let new_quote_total_balance = new_quote_spot_balance +. quote_earn_balance in
      Hashtbl.replace balances quote_asset new_quote_total_balance;

      let quote_tx = {
        Primitives.id = Primitives.Id.gen ();
        asset = quote_asset;
        amount = quote_tx_amount;
        timestamp = fill.ts;
        transaction_type = Primitives.Trade {
          order_id = fill.order_id;
          side = fill.side;
          price = fill.price;
          qty = fill.qty;
        };
        cost_basis = None;
        total_cost = None;
        balance_after = new_quote_total_balance;
      } in
      let%lwt _ = Transaction_history.add_transaction quote_tx in
      Lwt.return_unit
  | None -> Lwt.return_unit
  ) >>= fun () ->
   debug_f ~section "Processed fill event for %s: %s %.8f @ %s"
     fill.symbol
     (match fill.side with `Buy -> "BUY" | `Sell -> "SELL")
     (float_of_string (Primitives.Qty.to_string fill.qty))
     (Primitives.Price.to_string fill.price)

(* Start the balance update processor *)
let start_balance_update_processor () =
  let rec balance_loop () =
    process_balance_update_from_queue () >>= fun () ->
    balance_loop ()
  in
  let rec fill_loop () =
    process_fill_event_from_queue () >>= fun () ->
    fill_loop ()
  in
  Lwt.async balance_loop;
  Lwt.async fill_loop

let handle_balances_message msg =
  Lwt.catch (fun () ->
    let json = Yojson.Safe.from_string msg in
    let open Yojson.Safe.Util in
    match member "channel" json with
    | `String "balances" ->
        (match member "type" json with
        | `String "snapshot" -> handle_balance_snapshot (member "data" json)
        | `String "update" -> handle_balance_update (member "data" json)
        | _ -> warning_f ~section "Unhandled message type on balances channel: %s" msg
        )
    | _ ->
        Lwt.catch (fun () ->
          let channel = member "channel" json |> to_string in
          if channel <> "heartbeat" then
            debug_f ~section "Received non-balances message: %s" msg
          else
            Lwt.return_unit
        ) (fun exn ->
          warning_f ~section "Error parsing non-balances message: %s (%s)" msg (Printexc.to_string exn)
        )
  ) (fun exn ->
    error_f ~section "JSON parsing error in balances message: %s (%s)" msg (Printexc.to_string exn) >>= fun () ->
    Lwt.return_unit
  )

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

let rec listen_for_balances conn =
  Websocket_lwt_unix.read conn >>= fun frame ->
  let content = frame.content in
  handle_balances_message content >>= fun () ->
  listen_for_balances conn

let initialize_ws_balances_feed (cfg : Config.engine_config) (token : string) =
  (* Start the balance update processor *)
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

(* Handle fill events from trading *)
let handle_fill_event fill =
  (* Push fill event to ringbuffer for ordered processing *)
  Ringbuffer.push fill_event_queue fill >>= fun () ->
  debug_f ~section "Pushed fill event for %s to ringbuffer" fill.symbol

(* Initialize transaction history from current balances *)
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

let wait_for_balances () =
  if !balances_ready then (
    Lwt.return (Hashtbl.copy spot_balances, Hashtbl.copy earn_balances, Hashtbl.copy liquid_balances, Hashtbl.copy balances)
  ) else (
    let timeout = 30.0 in (* 30 second timeout *)
    Lwt.pick [
      (Lwt_condition.wait balances_initialized >>= fun () ->
       info_f ~section "Balance WebSocket initialized, received initial snapshot" >>= fun () ->
       Lwt.return (Hashtbl.copy spot_balances, Hashtbl.copy earn_balances, Hashtbl.copy liquid_balances, Hashtbl.copy balances));
      (Lwt_unix.sleep timeout >>= fun () ->
       warning_f ~section "Balance WebSocket initialization timeout after %.1f seconds, returning empty balances" timeout >>= fun () ->
       Lwt.return (Hashtbl.create 16, Hashtbl.create 16, Hashtbl.create 16, Hashtbl.create 16))
    ]
  )
