(* src/dio_state/state.ml *)


open Dio_types
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
