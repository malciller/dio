(** Dashboard view for telemetry metrics *)

open Notty
open Notty.A
open Lwt.Infix
open Dio_types.Telemetry_types

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

(** Format value based on metric type *)
let format_metric_value metric_type value =
  match metric_type with
  | "timer" -> format_duration_ms value
  | "counter" -> format_numeric_value value
  | "gauge" ->
      (* Check if it's a rate-like gauge *)
      if value >= 1.0 then format_rate_value value
      else format_numeric_value value
  | "histogram" -> format_numeric_value value  (* Show average for histograms *)
  | _ -> format_numeric_value value

(** Calculate the maximum width needed for metric names *)
let calculate_max_metric_name_width stats =
  (* Find the maximum length among all actual metrics *)
  List.fold_left (fun acc (name, _) -> max acc (String.length name)) 0 stats

(** Helper function to check if a string contains a substring *)
let string_contains s substr =
  try
    let _ = Str.search_forward (Str.regexp_string substr) s 0 in
    true
  with Not_found -> false

(** Render a single metric row *)
let render_metric_row max_width (name, stats) =
  (* Extract metric type from name (stored in telemetry system) *)
  (* This is a simplified approach - in a real system we'd store the metric type with the stats *)
  let metric_type = if string_contains name "latency" || string_contains name "duration" || string_contains name "processing_time" then "timer"
                   else if string_contains name "rate" then "gauge"
                   else if string_contains name "histogram" then "histogram"
                   else "counter" in

  let name_part = I.string (A.fg A.white) (Printf.sprintf "%-*s" max_width name) in
  let count_part = I.string (A.fg A.cyan) (Printf.sprintf "%8s" (format_counter stats.count)) in

  (* Format values based on metric type *)
  let mean_str = format_metric_value metric_type stats.mean in
  let p95_str = format_metric_value metric_type stats.p95 in
  let p99_str = format_metric_value metric_type stats.p99 in

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

(** Sort metrics alphabetically for display *)
let sort_metrics_alphabetically stats =
  List.sort (fun (name1, _) (name2, _) ->
    String.compare name1 name2
  ) stats |> List.filter (fun (name, _stats) ->
    (* Filter out initialization metrics that just show "1.0" and provide no value *)
    not (string_contains name "telemetry.initialized" ||
         string_contains name "connection_initialized")
  )

(** Main telemetry panel rendering function *)
let render_telemetry_panel _width height =
  Lwt.catch (fun () ->
    Telemetry.get_all_stats () >>= fun all_stats ->

    let sorted_stats = sort_metrics_alphabetically all_stats in

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
  let sorted_stats = sort_metrics_alphabetically all_stats in
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
        string_contains name "latency" ||
        string_contains name "duration" ||
        string_contains name "processing_time"
      ) all_stats in
      if latency_metrics = [] then 0.0
      else
        let total_mean = List.fold_left (fun acc (_, stats) ->
          acc +. stats.mean
        ) 0.0 latency_metrics in
        total_mean /. Float.of_int (List.length latency_metrics)
    in

    Lwt.return (total_metrics, avg_latency)

  ) (fun _ ->
    Lwt.return (0, 0.0)
  )
