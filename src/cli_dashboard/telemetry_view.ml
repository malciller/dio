(** Dashboard view for telemetry metrics *)

open Notty
open Notty.A
open Lwt.Infix


(* Telemetry dashboard view *)

(** Check if a string contains a given substring *)
let string_contains s sub =
  try
    let _ = Str.search_forward (Str.regexp_string sub) s 0 in
    true
  with Not_found -> false

(** Format duration in milliseconds with appropriate precision *)
let format_duration_ms duration =
  let ms = duration *. 1000.0 in
  if ms >= 1000.0 then
    Printf.sprintf "%.1fs" (ms /. 1000.0)
  else if ms >= 1.0 then
    Printf.sprintf "%.1fms" ms
  else
    Printf.sprintf "%.2fms" ms

(** Format counter values with k/M suffixes *)
let format_counter count =
  if count >= 1_000_000 then
    Printf.sprintf "%.1fM" (Float.of_int count /. 1_000_000.0)
  else if count >= 1_000 then
    Printf.sprintf "%.1fk" (Float.of_int count /. 1_000.0)
  else
    string_of_int count

(** Determine if a metric name represents an actual duration/timer (not rate or histogram) *)
let is_timer_metric name =
  string_contains name "latency" || string_contains name "duration" || string_contains name "time" ||
  string_contains name "processing_time"

(** Determine if a metric name represents a rate metric that should be displayed as X/sec *)
let is_rate_metric name =
  string_contains name "rate" && not (string_contains name "histogram")

(** Determine if a metric name represents a histogram of rate values *)
let is_rate_histogram_metric name =
  string_contains name "rate" && string_contains name "histogram"

(** Format a rate value with appropriate units (per second) *)
let format_rate_value v =
  if v >= 1_000_000.0 then
    Printf.sprintf "%.1fM/s" (v /. 1_000_000.0)
  else if v >= 1_000.0 then
    Printf.sprintf "%.1fk/s" (v /. 1_000.0)
  else if v >= 10.0 then
    Printf.sprintf "%.0f/s" v
  else
    Printf.sprintf "%.1f/s" v

(** Format a numeric (non-duration) value with compact notation *)
let format_numeric_value v =
  if v >= 1_000_000.0 then
    Printf.sprintf "%.1fM" (v /. 1_000_000.0)
  else if v >= 1_000.0 then
    Printf.sprintf "%.1fk" (v /. 1_000.0)
  else if v >= 10.0 then
    Printf.sprintf "%.0f" v
  else
    Printf.sprintf "%.1f" v

(** Determine if a metric name represents a counter that should show total/cumulative values *)
let is_counter_metric name =
  string_contains name "processed" || string_contains name "sent" ||
  string_contains name "successful" || string_contains name "failed" ||
  string_contains name "commands" || string_contains name "events" ||
  string_contains name "attempts"

(** Calculate the maximum width needed for metric names *)
let calculate_max_metric_name_width stats =
  let priority_order = [
    "exchange.kraken.order_latency";
    "router.command_processing_time";
    "ringbuffer.push.duration";
    "ringbuffer.pop.duration";
    "feed.tick_rate_histogram";
    "feed.exec_rate_histogram";
    "feed.ticks_processed";
    "feed.executions_processed";
    "exchange.kraken.orders_sent";
    "exchange.kraken.orders_successful";
    "exchange.kraken.orders_failed";
    "router.commands_processed";
    "router.duplicate_commands";
    "ringbuffer.buffer_full.events";
    "ringbuffer.buffer_empty.events";
    "feed.connection_attempts";
    "feed.connection_failures";
    "feed.exec_connection_attempts";
    "feed.exec_connection_failures";
    "router.command_errors";
    "router.unknown_exchange";
  ] in

  (* Find the maximum length among priority metrics and actual metrics *)
  let max_priority_length = List.fold_left (fun acc name -> max acc (String.length name)) 0 priority_order in
  let max_actual_length = List.fold_left (fun acc (name, _) -> max acc (String.length name)) 0 stats in

  max max_priority_length max_actual_length

(** Render a single metric row *)
let render_metric_row max_width (name, stats) =
  let name_part = I.string (A.fg A.white) (Printf.sprintf "%-*s" max_width name) in
  let count_part = I.string (A.fg A.cyan) (Printf.sprintf "%8s" (format_counter stats.Dio_types.Telemetry_types.count)) in

  (* Format values based on metric type *)
  let mean_str =
    if is_counter_metric name then
      format_numeric_value stats.Dio_types.Telemetry_types.mean
    else if is_rate_metric name || is_rate_histogram_metric name then
      format_rate_value stats.Dio_types.Telemetry_types.mean
    else if is_timer_metric name then
      format_duration_ms stats.Dio_types.Telemetry_types.mean
    else
      format_numeric_value stats.Dio_types.Telemetry_types.mean in

  let p95_str =
    if is_counter_metric name then
      format_numeric_value stats.Dio_types.Telemetry_types.p95
    else if is_rate_metric name || is_rate_histogram_metric name then
      format_rate_value stats.Dio_types.Telemetry_types.p95
    else if is_timer_metric name then
      format_duration_ms stats.Dio_types.Telemetry_types.p95
    else
      format_numeric_value stats.Dio_types.Telemetry_types.p95 in

  let p99_str =
    if is_counter_metric name then
      format_numeric_value stats.Dio_types.Telemetry_types.p99
    else if is_rate_metric name || is_rate_histogram_metric name then
      format_rate_value stats.Dio_types.Telemetry_types.p99
    else if is_timer_metric name then
      format_duration_ms stats.Dio_types.Telemetry_types.p99
    else
      format_numeric_value stats.Dio_types.Telemetry_types.p99 in

  let mean_part = I.string (A.fg A.green) (Printf.sprintf "%8s" mean_str) in
  let p95_part = I.string (A.fg A.yellow) (Printf.sprintf "%8s" p95_str) in
  let p99_part = I.string (A.fg A.red) (Printf.sprintf "%8s" p99_str) in

  I.hcat [
    name_part;
    I.string A.empty " ";
    count_part;
    I.string A.empty " ";
    mean_part;
    I.string A.empty " ";
    p95_part;
    I.string A.empty " ";
    p99_part
  ]

(** Render telemetry panel header *)
let render_header max_width =
  let title = I.string (A.fg A.cyan ++ A.st A.bold) "PERFORMANCE METRICS" in
  let metric_header = Printf.sprintf "%-*s" max_width "Metric" in
  let headers = I.hcat [
    I.string (A.fg A.white ++ A.st A.bold) metric_header;
    I.string A.empty " ";
    I.string (A.fg A.cyan ++ A.st A.bold) "   Count";
    I.string A.empty " ";
    I.string (A.fg A.green ++ A.st A.bold) "    Mean";
    I.string A.empty " ";
    I.string (A.fg A.yellow ++ A.st A.bold) "     P95";
    I.string A.empty " ";
    I.string (A.fg A.red ++ A.st A.bold) "     P99";
  ] in
  let separator_length = String.length metric_header + 1 + 8 + 1 + 8 + 1 + 8 + 1 + 8 in
  let separator = I.string (A.fg (A.gray 10)) (String.make separator_length '-') in
  I.vcat [title; headers; separator]

(** Sort metrics by importance for display *)
let sort_metrics_by_priority stats =
  let priority_order = [
    "exchange.kraken.order_latency";
    "router.command_processing_time";
    "ringbuffer.push.duration";
    "ringbuffer.pop.duration";
    "feed.tick_rate_histogram";
    "feed.exec_rate_histogram";
    "feed.ticks_processed";
    "feed.executions_processed";
    "exchange.kraken.orders_sent";
    "exchange.kraken.orders_successful";
    "exchange.kraken.orders_failed";
    "router.commands_processed";
    "router.duplicate_commands";
    "ringbuffer.buffer_full.events";
    "ringbuffer.buffer_empty.events";
    "feed.connection_attempts";
    "feed.connection_failures";
    "feed.exec_connection_attempts";
    "feed.exec_connection_failures";
    "router.command_errors";
    "router.unknown_exchange";
  ] in
  
  let get_priority name = 
    let rec find_index i = function
      | [] -> 999
      | h :: _ when h = name -> i
      | _ :: t -> find_index (i + 1) t
    in
    find_index 0 priority_order
  in
  
  List.sort (fun (name1, _) (name2, _) ->
    compare (get_priority name1) (get_priority name2)
  ) stats |> List.filter (fun (name, _stats) ->
    (* Filter out initialization metrics that just show "1.0" and provide no value *)
    not (name = "feed.telemetry.initialized" ||
         name = "feed.connection_initialized" ||
         name = "feed.exec_connection_initialized")
  )

(** Main telemetry panel rendering function *)
let render_telemetry_panel _width height =
  Lwt.catch (fun () ->
    Telemetry.get_all_stats () >>= fun all_stats ->
    
    let sorted_stats = sort_metrics_by_priority all_stats in

    (* Take only top metrics that fit in the available height *)
    let max_rows = max 0 (height - 4) in (* Reserve space for header and padding *)
    let displayed_stats =
      if List.length sorted_stats > max_rows then
        let rec take n lst =
          match n, lst with
          | 0, _ -> []
          | _, [] -> []
          | n, h :: t when n > 0 -> h :: take (n - 1) t
          | _, _ -> []
        in
        take max_rows sorted_stats
      else
        sorted_stats
    in

    let max_width = calculate_max_metric_name_width displayed_stats in
    let header = render_header max_width in
    let metric_rows = List.map (render_metric_row max_width) displayed_stats in
    
    let content = match metric_rows with
      | [] -> [I.string (A.fg (A.gray 10)) "No metrics available yet..."]
      | rows -> rows
    in
    
    let panel_content = I.vcat (header :: content) in
    Lwt.return panel_content
    
  ) (fun exn ->
    Printf.eprintf "Error rendering telemetry panel: %s\n%!" (Printexc.to_string exn);
    let error_msg = I.string (A.fg A.red) "Error loading telemetry data" in
    Lwt.return error_msg
  )

(** Pure rendering of telemetry panel given pre-fetched stats (avoids Lwt in UI render path) *)
let render_telemetry_panel_preloaded _width height (all_stats : (string * Dio_types.Telemetry_types.metric_stats) list) =
  let sorted_stats = sort_metrics_by_priority all_stats in
  let max_rows = max 0 (height - 4) in
  let rec take n lst =
    match n, lst with
    | 0, _ -> []
    | _, [] -> []
    | n, h :: t when n > 0 -> h :: take (n - 1) t
    | _, _ -> []
  in
  let displayed_stats = if List.length sorted_stats > max_rows then take max_rows sorted_stats else sorted_stats in

  let max_width = calculate_max_metric_name_width displayed_stats in
  let header = render_header max_width in
  let metric_rows = List.map (render_metric_row max_width) displayed_stats in
  let content = match metric_rows with
    | [] -> [I.string (A.fg (A.gray 10)) "No metrics available yet..."]
    | rows -> rows
  in
  I.vcat (header :: content)

(** Get summary statistics for header display *)
let get_telemetry_summary () =
  Lwt.catch (fun () ->
    Telemetry.get_all_stats () >>= fun all_stats ->
    
    let total_metrics = List.length all_stats in
    let avg_latency = 
      let latency_metrics = List.filter (fun (name, _) -> 
        String.contains name '.' && (
          string_contains name "latency" ||
          string_contains name "duration" ||
          string_contains name "time"
        )
      ) all_stats in
      if latency_metrics = [] then 0.0
      else
        let total_mean = List.fold_left (fun acc (_, stats) -> 
          acc +. stats.Dio_types.Telemetry_types.mean
        ) 0.0 latency_metrics in
        total_mean /. Float.of_int (List.length latency_metrics)
    in
    
    Lwt.return (total_metrics, avg_latency)
    
  ) (fun _ ->
    Lwt.return (0, 0.0)
  )
