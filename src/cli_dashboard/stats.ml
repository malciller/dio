(** Dashboard statistics and data aggregation utilities *)

open Dio_types

(** Global timestamp when the dashboard was started *)
let start_ts = Unix.gettimeofday ()

(** Maximum number of dashboard log entries to retain *)
let max_dashboard_logs = 15

(** Mutable reference to list of recent dashboard log messages *)
let dashboard_logs = ref []

(** Add a message to the dashboard logs, maintaining the maximum limit *)
let add_dashboard_log (message : string) : unit =
  let current_logs = !dashboard_logs in
  let new_logs = message :: current_logs in
  dashboard_logs :=
    if List.length new_logs > max_dashboard_logs then
      let rec take_first n lst =
        if n <= 0 then []
        else match lst with
          | [] -> []
          | h :: t -> h :: take_first (n - 1) t
      in
      take_first max_dashboard_logs new_logs
    else
      new_logs

(** Get current market price for a trading symbol *)
let get_price symbol = State.get_global_price symbol

(** Get open buy and sell orders for a trading symbol
    @return Tuple of (buy_orders, sell_orders) where each is a list of (price, qty) pairs *)
let get_orders_for_symbol symbol =
  let orders = Kraken.Kraken_incoming_data.get_all_open_orders () in
  let orders_list =
    Hashtbl.to_seq_values orders
    |> List.of_seq
    |> List.filter (fun order -> String.equal order.Kraken.Kraken_common_types.order_symbol symbol)
  in
  let buy_orders, sell_orders =
    List.partition (fun order -> order.Kraken.Kraken_common_types.side = Some Core.Buy) orders_list
  in
  let to_price_qty order = (order.Kraken.Kraken_common_types.limit_price, order.Kraken.Kraken_common_types.qty) in
  (List.map to_price_qty buy_orders, List.map to_price_qty sell_orders) 