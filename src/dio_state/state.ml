(** State management for trading engine.

    Tracks pending orders, executed trades, current prices, and strategy assignments
    across different trading symbols. Provides both immutable state operations and
    global mutable state for shared access.
*)

open Dio_types
module SMap = Map.Make(String)

let section = Lwt_log_core.Section.make "dio_state"

(** Trading strategy types supported by the engine. *)
type strategy_type =
  | Grid       (** Grid trading strategy *)
  | Orderbook  (** Orderbook-based strategy *)
  | Arbitrage  (** Arbitrage trading strategy *)
  | Monitor    (** Market monitoring strategy *)

(** Core trading state record containing all mutable trading data.

    Maps are keyed by trading symbol (string) for efficient lookups.
    All operations are designed to be thread-safe when used with proper synchronization.
*)
type t = {
  pending_orders: int SMap.t;           (** Count of pending orders per symbol *)
  trades_executed: Int64.t SMap.t;      (** Total trades executed per symbol *)
  current_prices: Primitives.Price.t SMap.t;  (** Latest price per symbol *)
  strategy_assignments: strategy_type SMap.t; (** Active strategy per symbol *)
}

(** Initial empty state for new state instances. *)
let initial = {
  pending_orders = SMap.empty;
  trades_executed = SMap.empty;
  current_prices = SMap.empty;
  strategy_assignments = SMap.empty;
}

(** Pending order management functions *)

(** Increment pending order count for a symbol. *)
let inc_pending asset state =
  let pending_orders =
    SMap.update asset (fun v -> Some (Option.value ~default:0 v + 1)) state.pending_orders in
  { state with pending_orders }

(** Decrement pending order count for a symbol. Removes entry if count reaches 0. *)
let dec_pending asset state =
  let pending_orders =
    SMap.update asset
      (fun v_opt ->
         match v_opt with
         | Some n when n > 0 -> Some (n - 1) | _ -> None)
      state.pending_orders
  in
  { state with pending_orders }

(** Increment executed trades count for a symbol. *)
let inc_trades asset state =
  let trades_executed =
    SMap.update asset
      (fun v -> Some (Int64.succ (Option.value ~default:Int64.zero v)))
      state.trades_executed
  in
  { state with trades_executed }

(** Price management functions *)

(** Update current price for a symbol. *)
let update_price symbol price state =
  let current_prices = SMap.add symbol price state.current_prices in
  { state with current_prices }

let get_price symbol state = SMap.find_opt symbol state.current_prices

(** Global state management for shared access across the application.

    These functions provide mutable global state operations. Use with caution
    and ensure proper synchronization in concurrent contexts.
*)

(** Global state instance - initialized to empty state. *)
let global_state = ref initial

let update_global_price symbol price =
  global_state := update_price symbol price !global_state

let get_global_price symbol = get_price symbol !global_state

(** Strategy assignment management functions *)

(** Assign a trading strategy to a symbol. *)
let update_strategy_assignment symbol strategy state =
  let strategy_assignments = SMap.add symbol strategy state.strategy_assignments in
  { state with strategy_assignments }

let update_global_strategy_assignment symbol strategy =
  global_state := update_strategy_assignment symbol strategy !global_state

(** Get the assigned strategy for a symbol. Returns None if no strategy assigned. *)
let get_strategy_assignment symbol state =
  SMap.find_opt symbol state.strategy_assignments

(** Get the globally assigned strategy for a symbol. *)
let get_global_strategy_assignment symbol = get_strategy_assignment symbol !global_state
