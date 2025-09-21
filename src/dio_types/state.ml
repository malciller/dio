(** State management for Dio trading system.

    Provides both per-session state tracking and global system state.
    Handles order counts, trade execution tracking, and price management. *)

open Primitives

module SMap = Map.Make(String)

let section = Lwt_log_core.Section.make "dio_state"

(** Per-session trading state tracking. *)
type t = {
  pending_orders: int SMap.t;      (** Count of pending orders by asset symbol *)
  trades_executed: Int64.t SMap.t; (** Total trades executed by asset symbol *)
  current_prices: Primitives.Price.t SMap.t; (** Latest known prices by symbol *)
}

(** Initial empty state for new sessions. *)
let initial = {
  pending_orders = SMap.empty;
  trades_executed = SMap.empty;
  current_prices = SMap.empty;
}

(** Increment pending order count for an asset. *)
let inc_pending asset state =
  let pending_orders =
    SMap.update asset (fun v -> Some (Option.value ~default:0 v + 1)) state.pending_orders in
  { state with pending_orders }

(** Decrement pending order count for an asset. Removes entry if count reaches zero. *)
let dec_pending asset state =
  let pending_orders =
    SMap.update asset
      (fun v_opt ->
         match v_opt with
         | Some n when n > 0 -> Some (n - 1) | _ -> None)
      state.pending_orders
  in
  { state with pending_orders }

(** Increment trade execution count for an asset. *)
let inc_trades asset state =
  let trades_executed =
    SMap.update asset
      (fun v -> Some (Int64.succ (Option.value ~default:Int64.zero v)))
      state.trades_executed
  in
  { state with trades_executed }

(** Update the current price for a symbol. *)
let update_price symbol price state =
  let current_prices = SMap.add symbol price state.current_prices in
  { state with current_prices }

(** Get current price for a symbol, returns None if not found. *)
let get_price symbol state = SMap.find_opt symbol state.current_prices

(** Price data with timestamp for tracking updates. *)
type price_info = {
  price : Price.t;     (** The price value *)
  timestamp : int64;   (** Microsecond timestamp of price update *)
}

(** Trading strategy types that can be assigned to symbols. *)
type strategy_assignment =
  | Grid      (** Grid trading strategy *)
  | Orderbook (** Orderbook-based strategy *)
  | Arbitrage (** Arbitrage strategy *)
  | Monitor   (** Price monitoring only *)
  | VMM       (** Volatility Market Making strategy *)

(** Global system state shared across all components. *)
type global_state_t = {
  mutable symbols : string list;  (** List of active trading symbols *)
  prices : (string, price_info) Hashtbl.t;  (** Symbol -> price info mapping *)
  strategy_assignments : (string, strategy_assignment) Hashtbl.t;  (** Symbol -> strategy mapping *)
}

(** Global state instance. *)
let global_state : global_state_t = {
  prices = Hashtbl.create 16;
  symbols = [];
  strategy_assignments = Hashtbl.create 16;
}

(** Symbol management functions. *)

(** Set the list of active trading symbols. *)
let set_symbols (syms: string list) =
  global_state.symbols <- syms

(** Get all currently active trading symbols. *)
let get_all_symbols () =
  global_state.symbols

(** Price management functions. *)

(** Update price for a symbol with current timestamp. *)
let update_global_price symbol price =
  let info = { price; timestamp = Int64.of_float (Unix.gettimeofday () *. 1_000_000.) } in
  Hashtbl.replace global_state.prices symbol info

(** Get current price for a symbol from global state. *)
let get_global_price symbol =
  match Hashtbl.find_opt global_state.prices symbol with
  | Some info -> Some info.price
  | None -> None

(** Strategy assignment functions. *)

(** Assign a trading strategy to a symbol. *)
let update_global_strategy_assignment symbol strategy =
  Hashtbl.replace global_state.strategy_assignments symbol strategy

(** Get the assigned strategy for a symbol. *)
let get_global_strategy_assignment symbol =
  Hashtbl.find_opt global_state.strategy_assignments symbol