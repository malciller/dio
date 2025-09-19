(* src/exchange/kraken/kraken_balances.ml *)

open Lwt.Infix
open Lwt_log_core
open Dio_types

let section = Section.make "kraken.balances"

(* In-memory store for balances *)
let balances : (string, float) Hashtbl.t = Hashtbl.create(16)

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
  Hashtbl.clear previous_balances;
  (* Copy current balances to previous before clearing *)
  Hashtbl.iter (fun asset balance ->
    Hashtbl.replace previous_balances asset balance
  ) balances;

  Hashtbl.clear balances;
  data |> to_list |> Lwt_list.iter_s (fun item ->
    let asset = item |> member "asset" |> to_string in
    let balance_val = item |> member "balance" |> to_float in
    Hashtbl.add balances asset balance_val;
    debug_f ~section "Balance snapshot: %s -> %f" asset balance_val
  ) >>= fun () ->
  balances_ready := true;
  Lwt_condition.broadcast balances_initialized ();
  info_f ~section "Processed balance snapshot. %d assets loaded." (Hashtbl.length balances)

let handle_balance_update data =
  let open Yojson.Safe.Util in
  data |> to_list |> Lwt_list.iter_s (fun item ->
    debug_f ~section "Full balance update item: %s" (Yojson.Safe.to_string item) >>= fun () ->

    let asset = item |> member "asset" |> to_string in
    let new_balance = item |> member "balance" |> to_float in
    let old_balance = Hashtbl.find_opt balances asset |> Option.value ~default:0.0 in
    let event_type = item |> member "type" |> to_string in
    let subtype_opt = try Some (item |> member "subtype" |> to_string) with _ -> None in
    let category = item |> member "category" |> to_string in
    let wallet_type = item |> member "wallet_type" |> to_string in
    let wallet_id = item |> member "wallet_id" |> to_string in

    let is_trade = event_type = "trade" in
    let is_staking = event_type = "staking" in
    let is_earn = event_type = "earn" in
    let is_staking_trade = is_trade && category = "trade" && (wallet_type = "earn" || wallet_id = "bonded") in
    let is_internal_transfer = match subtype_opt with
      | Some subtype -> List.mem subtype ["stakingfromspot"; "spotfromstaking"; "stakingtospot"; "spottostaking"]
      | None -> false
    in

    if is_earn then (
      let amount = item |> member "amount" |> to_float in
      let updated_balance = old_balance +. amount in
      Hashtbl.replace balances asset updated_balance;
      process_balance_change asset old_balance updated_balance >>= fun () ->
      debug_f ~section "Balance update (via earn amount): %s -> %f (was %f)" asset updated_balance old_balance
    ) else if is_internal_transfer || is_trade || is_staking || is_staking_trade then (
      let skip_reason = if is_trade then "trade" else if is_staking then "staking" else if is_staking_trade then "staking trade" else "internal transfer" in
      debug_f ~section "Skipping %s for %s: %s %s (wallet: %s/%s)" 
        skip_reason asset event_type (Option.value ~default:"" subtype_opt) wallet_type wallet_id >>= fun () ->
      Hashtbl.replace balances asset new_balance;
      Lwt.return_unit
    ) else (
      (* Process balance change before updating *)
      process_balance_change asset old_balance new_balance >>= fun () ->
      Hashtbl.replace balances asset new_balance;
      debug_f ~section "Balance update: %s -> %f (was %f)" asset new_balance old_balance
    )
  )

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
  let tx = Transaction_history.transaction_from_fill fill in
  (* Update balance_after with current balance of the base asset (e.g., SOL for SOL/USD) *)
  let base_asset =
    try
      let idx = String.index fill.symbol '/' in
      String.sub fill.symbol 0 idx
    with Not_found -> fill.symbol
  in
  let updated_tx = { tx with
    balance_after = Hashtbl.find_opt balances base_asset |> Option.value ~default:0.0
  } in
  let%lwt _ = Transaction_history.add_transaction updated_tx in
  debug_f ~section "Processed fill event for %s: %s %.8f @ %s"
    fill.symbol
    (match fill.side with `Buy -> "BUY" | `Sell -> "SELL")
    (float_of_string (Primitives.Qty.to_string fill.qty))
    (Primitives.Price.to_string fill.price)

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
    Lwt.return (Hashtbl.copy balances)
  ) else (
    let timeout = 30.0 in (* 30 second timeout *)
    Lwt.pick [
      (Lwt_condition.wait balances_initialized >>= fun () ->
       info_f ~section "Balance WebSocket initialized, received initial snapshot" >>= fun () ->
       Lwt.return (Hashtbl.copy balances));
      (Lwt_unix.sleep timeout >>= fun () ->
       warning_f ~section "Balance WebSocket initialization timeout after %.1f seconds, returning empty balances" timeout >>= fun () ->
       Lwt.return (Hashtbl.create 16))
    ]
  )
