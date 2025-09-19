(* src/dio_types/transaction_history.ml *)

open Lwt.Infix
open Primitives
open Event

let section = Lwt_log_core.Section.make "transaction_history"

(* Transaction storage *)
let transaction_history : (string, transaction list) Hashtbl.t = Hashtbl.create 64
let cost_basis_cache : (string, cost_basis_info) Hashtbl.t = Hashtbl.create 64

let is_stablecoin asset =
  let stablecoins = ["USD"; "USDT"; "USDC"; "USDG"; "USDR"] in
  List.mem asset stablecoins

(* Get transactions for an asset *)
let get_transactions asset =
  Hashtbl.find_opt transaction_history asset |> Option.value ~default:[]

(* Get cost basis for an asset *)
let get_cost_basis asset =
  Hashtbl.find_opt cost_basis_cache asset

(* Calculate cost basis from transactions *)
let calculate_cost_basis asset =
  let transactions = get_transactions asset in
  let buy_transactions = List.filter (fun tx ->
    match tx.transaction_type with
    | Trade { side = `Buy; _ } -> true
    | Deposit | Staking_Reward -> true (* These add to our cost basis *)
    | _ -> false
  ) transactions in

  let total_units, total_cost = List.fold_left (fun (units, cost) tx ->
    match tx.transaction_type with
    | Trade { side = `Buy; price; qty; _ } ->
        let price_float = float_of_string (Price.to_string price) in
        let qty_float = float_of_string (Qty.to_string qty) in
        (units +. qty_float, cost +. (price_float *. qty_float))
    | Deposit | Staking_Reward ->
        (* For deposits/rewards, we need to estimate value at time of transaction *)
        (* This is a simplified approach - in production you'd want historical prices *)
        (units +. tx.amount, cost +. (abs_float tx.amount *. Option.value tx.cost_basis ~default:(if is_stablecoin asset then 1.0 else 0.0)))
    | _ -> (units, cost)
  ) (0.0, 0.0) buy_transactions in

  if total_units > 0.0 then
    Some {
      total_units;
      total_cost_basis = total_cost;
      average_cost_per_unit = total_cost /. total_units;
      last_updated = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
    }
  else
    None

(* Add a transaction and update cost basis *)
let add_transaction tx =
  let asset = tx.asset in

  (* Add to transaction history *)
  let current_txs = get_transactions asset in
  let updated_txs = tx :: current_txs in
  Hashtbl.replace transaction_history asset updated_txs;

  (* Recalculate cost basis *)
  let new_cost_basis = calculate_cost_basis asset in
  (match new_cost_basis with
   | Some cb -> Hashtbl.replace cost_basis_cache asset cb
   | None -> Hashtbl.remove cost_basis_cache asset);

  Lwt_log_core.debug ~section
    (Printf.sprintf "Added transaction for %s: %s %.8f @ %s"
       asset
       (match tx.transaction_type with
        | Trade { side = `Buy; _ } -> "BUY"
        | Trade { side = `Sell; _ } -> "SELL"
        | Deposit -> "DEPOSIT"
        | Withdrawal -> "WITHDRAWAL"
        | Staking_Reward -> "STAKING_REWARD"
        | Fee -> "FEE"
        | Adjustment -> "ADJUSTMENT"
        | Unknown -> "UNKNOWN")
       tx.amount
       (Int64.to_string tx.timestamp))
  >>= fun () ->
  Lwt.return_unit

(* Get accumulated cost for current balance *)
let get_accumulated_cost asset current_balance =
  match get_cost_basis asset with
  | Some cb ->
      if current_balance >= cb.total_units then
        (* All current balance comes from tracked transactions *)
        Some cb.total_cost_basis
      else
        (* Only some of current balance comes from tracked transactions *)
        Some (cb.average_cost_per_unit *. current_balance)
  | None -> None

(* Create transaction from fill event *)
let transaction_from_fill fill =
  let order_id = fill.order_id in
  let asset =
    (* Use base asset when symbol is a pair like "SOL/USD" *)
    try
      let idx = String.index fill.symbol '/' in
      String.sub fill.symbol 0 idx
    with Not_found -> fill.symbol
  in
  let qty_float = float_of_string (Qty.to_string fill.qty) in
  let price_float = float_of_string (Price.to_string fill.price) in
  let amount = match fill.side with
    | `Buy -> qty_float    (* Positive for buys *)
    | `Sell -> -.qty_float (* Negative for sells *)
  in
  let tx_type = Trade {
    order_id;
    side = fill.side;
    price = fill.price;
    qty = fill.qty;
  } in
  let cost_basis = if fill.side = `Buy then Some price_float else None in
  let total_cost = if fill.side = `Buy then Some (price_float *. qty_float) else None in

  {
    id = Id.gen ();
    asset;
    amount;
    timestamp = fill.ts;
    transaction_type = tx_type;
    cost_basis;
    total_cost;
    balance_after = 0.0; (* Will be updated when we know the balance *)
  }

(* Initialize from existing balance (for reconciliation) *)
let initialize_from_balance asset initial_balance =
  if initial_balance > 0.0 then
    let tx = {
      id = Id.gen ();
      asset;
      amount = initial_balance;
      timestamp = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
      transaction_type = Adjustment;
      cost_basis = None; (* Unknown cost basis for initial balance *)
      total_cost = None;
      balance_after = initial_balance;
    } in
    let%lwt _ = add_transaction tx in
    Lwt.return_unit
  else
    Lwt.return_unit

(* Clear all transaction history *)
let clear_history () =
  Hashtbl.clear transaction_history;
  Hashtbl.clear cost_basis_cache;
  Lwt_log_core.info ~section "Cleared all transaction history"

(* Get unrealized P&L for an asset *)
let get_unrealized_pnl asset current_balance current_price_usd =
  match get_cost_basis asset with
  | Some cb ->
      let current_value = current_balance *. current_price_usd in
      let cost_basis_value = min cb.total_cost_basis (cb.average_cost_per_unit *. current_balance) in
      Some (current_value -. cost_basis_value)
  | None -> None

(* Reconcile balance discrepancies *)
let reconcile_balance asset current_balance =
  let transactions = get_transactions asset in
  let expected_balance = List.fold_left (fun acc tx -> acc +. tx.amount) 0.0 transactions in
  let discrepancy = current_balance -. expected_balance in

  if abs_float discrepancy > 0.000001 then (
    if discrepancy > 0.0 then (
      Lwt_log_core.warning ~section
        (Printf.sprintf "Balance reconciliation needed for %s: current=%.8f expected=%.8f discrepancy=%.8f (adding positive adjustment)"
           asset current_balance expected_balance discrepancy) >>= fun () ->

      (* Create positive adjustment transaction *)
      let adjustment_tx = {
        id = Id.gen ();
        asset;
        amount = discrepancy;
        timestamp = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
        transaction_type = Adjustment;
        cost_basis = None;
        total_cost = None;
        balance_after = current_balance;
      } in
      add_transaction adjustment_tx >>= fun () ->
      Lwt_log_core.info ~section
        (Printf.sprintf "Created positive adjustment for %s: %.8f" asset discrepancy)
    ) else (
      Lwt_log_core.warning ~section
        (Printf.sprintf "Negative discrepancy detected for %s: current=%.8f expected=%.8f discrepancy=%.8f (adding negative adjustment)"
           asset current_balance expected_balance discrepancy) >>= fun () ->
      let adjustment_tx = {
        id = Id.gen ();
        asset;
        amount = discrepancy;
        timestamp = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float;
        transaction_type = Adjustment;
        cost_basis = None;
        total_cost = None;
        balance_after = current_balance;
      } in
      add_transaction adjustment_tx >>= fun () ->
      Lwt_log_core.info ~section
        (Printf.sprintf "Created negative adjustment for %s: %.8f" asset discrepancy)
    )
  ) else (
    Lwt_log_core.debug ~section
      (Printf.sprintf "Balance reconciliation for %s: OK (current=%.8f expected=%.8f)"
         asset current_balance expected_balance) >>= fun () ->
    Lwt.return_unit
  )

(* Get balance summary for reconciliation *)
let get_balance_summary asset =
  let transactions = get_transactions asset in
  let buy_txs = List.filter (fun tx ->
    match tx.transaction_type with
    | Trade { side = `Buy; _ } -> true
    | Deposit | Staking_Reward -> true
    | _ -> false
  ) transactions in

  let sell_txs = List.filter (fun tx ->
    match tx.transaction_type with
    | Trade { side = `Sell; _ } -> true
    | Withdrawal | Fee -> true
    | _ -> false
  ) transactions in

  let total_bought = List.fold_left (fun acc tx -> acc +. abs_float tx.amount) 0.0 buy_txs in
  let total_sold = List.fold_left (fun acc tx -> acc +. abs_float tx.amount) 0.0 sell_txs in
  let net_position = total_bought -. total_sold in

  (total_bought, total_sold, net_position, List.length transactions)

(* Validate transaction history integrity *)
let validate_history asset =
  let transactions = get_transactions asset in
  let rec validate_txs txs expected_balance =
    match txs with
    | [] -> true, expected_balance
    | tx :: rest ->
        let new_balance = expected_balance +. tx.amount in
        if abs_float (new_balance -. tx.balance_after) > 0.000001 then (
          Lwt_log_core.warning ~section
            (Printf.sprintf "Transaction integrity check failed for %s: expected_balance=%.8f tx.balance_after=%.8f"
               asset new_balance tx.balance_after) |> ignore;
          false, new_balance
        ) else (
          validate_txs rest new_balance
        )
  in
  let is_valid, _ = validate_txs transactions 0.0 in
  if not is_valid then
    Lwt_log_core.warning ~section
      (Printf.sprintf "Transaction history integrity validation failed for %s" asset)
    |> ignore;
  is_valid

(* Export transaction history for debugging *)
let export_history asset =
  let txs = get_transactions asset in
  List.map (fun tx ->
    Printf.sprintf "%s: %s %.8f (%s)"
      (Int64.to_string tx.timestamp)
      tx.asset
      tx.amount
      (match tx.transaction_type with
       | Trade { side = `Buy; _ } -> "BUY"
       | Trade { side = `Sell; _ } -> "SELL"
       | Deposit -> "DEPOSIT"
       | Withdrawal -> "WITHDRAWAL"
       | Staking_Reward -> "STAKING_REWARD"
       | Fee -> "FEE"
       | Adjustment -> "ADJUSTMENT"
       | Unknown -> "UNKNOWN")
  ) txs

(* Get balance reconciliation report *)
let get_reconciliation_report () =
  let assets = Hashtbl.fold (fun asset _ acc -> asset :: acc) transaction_history [] in
  List.map (fun asset ->
    let (total_bought, total_sold, net_position, tx_count) = get_balance_summary asset in
    let is_valid = validate_history asset in
    Printf.sprintf "%s: bought=%.8f sold=%.8f net=%.8f tx_count=%d valid=%b"
      asset
      total_bought
      total_sold
      net_position
      tx_count
      is_valid
  ) assets
