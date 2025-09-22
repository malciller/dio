(** Simple telemetry collection and analysis module *)

open Lwt.Infix

(** Thread-safe storage for metrics *)
let metrics_store : (string, Dio_types.Telemetry_types.metric list) Hashtbl.t = Hashtbl.create 64
let metrics_mutex = Lwt_mutex.create ()

(** Configuration *)
let max_metrics_per_key = 1000
let stats_window_seconds = 300.0 (* 5 minutes *)

(** Record a new metric *)
let record_metric (metric : Dio_types.Telemetry_types.metric) : unit Lwt.t =
  Lwt_mutex.with_lock metrics_mutex (fun () ->
    let key = Printf.sprintf "%s.%s" 
      (match metric.component with
       | Feed -> "feed"
       | Router -> "router" 
       | Strategy s -> "strategy." ^ s
       | Exchange s -> "exchange." ^ s
       | Ringbuffer s -> "ringbuffer." ^ s)
      metric.name
    in
    let current_metrics = 
      match Hashtbl.find_opt metrics_store key with
      | Some metrics -> metric :: metrics
      | None -> [metric]
    in
    (* Keep only recent metrics within window *)
    let cutoff_time = Unix.gettimeofday () -. stats_window_seconds in
    let filtered_metrics =
      List.filter (fun (m : Dio_types.Telemetry_types.metric) -> m.timestamp >= cutoff_time) current_metrics
      |> fun lst -> 
        if List.length lst > max_metrics_per_key then
          let rec take n = function
            | [] -> []
            | h :: t when n > 0 -> h :: take (n - 1) t
            | _ -> []
          in
          take max_metrics_per_key lst
        else lst
    in
    Hashtbl.replace metrics_store key filtered_metrics;
    Lwt.return_unit
  )

(** Initialize the telemetry system with a startup metric *)
let init () : unit Lwt.t =
  let metric = Dio_types.Telemetry_types.{
    component = Feed;
    name = "telemetry.initialized";
    value = Counter 1L;
    timestamp = Unix.gettimeofday ();
    tags = [];
  } in
  record_metric metric

(** Convenience functions for common metrics *)
let increment_counter component name ?(tags=[]) delta =
  let metric = Dio_types.Telemetry_types.{
    component;
    name;
    value = Counter (Int64.of_int delta);
    timestamp = Unix.gettimeofday ();
    tags;
  } in
  record_metric metric

let record_gauge component name ?(tags=[]) value =
  let metric = Dio_types.Telemetry_types.{
    component;
    name;
    value = Gauge value;
    timestamp = Unix.gettimeofday ();
    tags;
  } in
  record_metric metric

let record_timer component name ?(tags=[]) duration =
  let metric = Dio_types.Telemetry_types.{
    component;
    name;
    value = Timer duration;
    timestamp = Unix.gettimeofday ();
    tags;
  } in
  record_metric metric

let record_histogram component name ?(tags=[]) values =
  let metric = Dio_types.Telemetry_types.{
    component;
    name;
    value = Histogram values;
    timestamp = Unix.gettimeofday ();
    tags;
  } in
  record_metric metric

(** High-level timing utility *)
let time_operation component name ?(tags=[]) operation =
  let start_time = Unix.gettimeofday () in
  operation () >>= fun result ->
  let duration = Unix.gettimeofday () -. start_time in
  record_timer component name ~tags duration >>= fun () ->
  Lwt.return result

(** Calculate statistics for a metric *)
let calculate_stats (metrics : Dio_types.Telemetry_types.metric list) : Dio_types.Telemetry_types.metric_stats option =
  if metrics = [] then None
  else
    (* Group metrics by type for different statistical treatment *)
    let counters = List.filter_map (fun (m : Dio_types.Telemetry_types.metric) ->
      match m.value with Counter v -> Some (Int64.to_float v) | _ -> None) metrics in
    let gauges = List.filter_map (fun (m : Dio_types.Telemetry_types.metric) ->
      match m.value with Gauge v -> Some v | _ -> None) metrics in
    let timers = List.filter_map (fun (m : Dio_types.Telemetry_types.metric) ->
      match m.value with Timer v -> Some v | _ -> None) metrics in
    let histograms = List.filter_map (fun (m : Dio_types.Telemetry_types.metric) ->
      match m.value with Histogram values -> Some values | _ -> None) metrics in

    (* Flatten histograms into individual values *)
    let histogram_values = List.flatten histograms in

    (* Combine all numeric values *)
    let all_values = counters @ gauges @ timers @ histogram_values in

    if all_values = [] then None
    else
      let sorted = List.sort Float.compare all_values in
      let count = List.length sorted in
      let sum = List.fold_left (+.) 0.0 sorted in
      let mean = sum /. Float.of_int count in
      let min_val = List.hd sorted in
      let max_val = List.hd (List.rev sorted) in
      let p95_idx = max 0 (count * 95 / 100 - 1) in
      let p99_idx = max 0 (count * 99 / 100 - 1) in
      let p95 = List.nth sorted p95_idx in
      let p99 = List.nth sorted p99_idx in
      Some Dio_types.Telemetry_types.{ count; mean; min = min_val; max = max_val; p95; p99 }

(** Get current statistics for all metrics *)
let get_all_stats () : (string * Dio_types.Telemetry_types.metric_stats) list Lwt.t =
  Lwt_mutex.with_lock metrics_mutex (fun () ->
    let stats = Hashtbl.fold (fun key metrics acc ->
      match calculate_stats metrics with
      | Some stats -> (key, stats) :: acc
      | None -> acc
    ) metrics_store [] in
    Lwt.return stats
  )