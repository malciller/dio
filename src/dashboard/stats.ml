open Dio_types
include State (* This includes all the state functions *)

let start_ts = Unix.gettimeofday ()

(* ─── Dashboard Log Storage ───────────────────────────────────── *)
let max_dashboard_logs = 10 (* Max number of log lines to keep for the dashboard *)
let dashboard_logs = ref []

let add_dashboard_log (message : string) : unit =
  let updated_logs = message :: !dashboard_logs in
  dashboard_logs := 
    if List.length updated_logs > max_dashboard_logs then
      List.rev (List.tl (List.rev updated_logs)) (* Remove the oldest (last in reversed list) *)
    else
      updated_logs
(* ─────────────────────────────────────────────────────────────── *)

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