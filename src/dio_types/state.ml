(* src/dio_types/state.ml *)

open Primitives

module SMap = Map.Make(String)

let section = Lwt_log_core.Section.make "dio_state"

type t = {
  pending_orders: int SMap.t;
  trades_executed: Int64.t SMap.t;
  current_prices: Primitives.Price.t SMap.t;
}

let initial = {
  pending_orders = SMap.empty;
  trades_executed = SMap.empty;
  current_prices = SMap.empty;
}

let inc_pending asset state =
  let pending_orders =
    SMap.update asset (fun v -> Some (Option.value ~default:0 v + 1)) state.pending_orders in
  { state with pending_orders }

let dec_pending asset state =
  let pending_orders =
    SMap.update asset
      (fun v_opt ->
         match v_opt with
         | Some n when n > 0 -> Some (n - 1) | _ -> None)
      state.pending_orders
  in
  { state with pending_orders }

let inc_trades asset state =
  let trades_executed =
    SMap.update asset
      (fun v -> Some (Int64.succ (Option.value ~default:Int64.zero v)))
      state.trades_executed
  in
  { state with trades_executed }

let update_price symbol price state =
  let current_prices = SMap.add symbol price state.current_prices in
  { state with current_prices }

let get_price symbol state = SMap.find_opt symbol state.current_prices

type price_info = {
  price : Price.t;
  timestamp : int64;
}

type strategy_assignment =
  | Grid
  | Orderbook
  | Arbitrage
  | Monitor

type global_state_t = {
  mutable symbols : string list;
  prices : (string, price_info) Hashtbl.t;
  strategy_assignments : (string, strategy_assignment) Hashtbl.t;
}

let global_state : global_state_t = {
  prices = Hashtbl.create 16;
  symbols = [];
  strategy_assignments = Hashtbl.create 16;
}

(* --- Symbols --- *)
let set_symbols (syms: string list) =
  global_state.symbols <- syms

let get_all_symbols () =
  global_state.symbols

(* --- Prices --- *)
let update_global_price symbol price =
  let info = { price; timestamp = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) } in
  Hashtbl.replace global_state.prices symbol info

let get_global_price symbol =
  match Hashtbl.find_opt global_state.prices symbol with
  | Some info -> Some info.price
  | None -> None

(* --- Strategy Assignments --- *)
let update_global_strategy_assignment symbol strategy =
  Hashtbl.replace global_state.strategy_assignments symbol strategy

let get_global_strategy_assignment symbol =
  Hashtbl.find_opt global_state.strategy_assignments symbol