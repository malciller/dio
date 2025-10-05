(** Improved telemetry collection and analysis module with type safety and incremental statistics *)

open Lwt.Infix
open Dio_types.Telemetry_types

(** Thread-safe storage for metrics using improved data structures *)
let metrics_store : (string, metric_entry) Hashtbl.t = Hashtbl.create 256
let metrics_mutex = Lwt_mutex.create ()
let config_ref = ref default_config

(** Sampling state - uses simple counter for fast sampling decision *)
let sample_counter = ref 0

(** Fast sampling check - returns true if this call should be sampled *)
let should_sample () =
  if !config_ref.sampling_rate >= 1.0 then true
  else if !config_ref.sampling_rate <= 0.0 then false
  else begin
    incr sample_counter;
    let threshold = int_of_float (1.0 /. !config_ref.sampling_rate) in
    (!sample_counter mod threshold) = 0
  end

(** Configuration management *)
let get_config () = !config_ref
let set_config cfg = 
  config_ref := cfg;
  sample_counter := 0 (* Reset sampling counter when config changes *)

(** Generate composite key from component path and metric name *)
let make_key component_path name =
  (Component.to_string component_path) ^ "." ^ name

(** Convert typed metric to internal storage format *)
let metric_to_entry (component: component_path) name value timestamp =
  let key = make_key component name in
  let metric_type = match value with
    | Counter_val _ -> "counter"
    | Gauge_val _ -> "gauge"
    | Timer_val _ -> "timer"
    | Histogram_val _ -> "histogram"
  in
  {
    key;
    component_path = component;
    metric_name = name;
    current_stats = None;
    raw_values = [(timestamp, value)];
    metric_type;
    last_accessed = timestamp;
  }

(** Update incremental statistics for a metric entry *)
let update_incremental_stats (entry: metric_entry) : unit =
  if not !config_ref.enable_incremental_stats then () else

  let values = List.map snd entry.raw_values in
  match values with
  | [] -> entry.current_stats <- None
  | _ ->
      let extract_numeric = function
        | Counter_val v -> Some (Int64.to_float v)
        | Gauge_val v -> Some v
        | Timer_val v -> Some v
        | Histogram_val vs -> Some (List.fold_left (fun acc x -> acc +. x) 0.0 vs /. Float.of_int (List.length vs))
      in
      let numeric_values = List.filter_map extract_numeric values in

      if numeric_values = [] then
        entry.current_stats <- None
      else
        let sorted = List.sort Float.compare numeric_values in
        let count = List.length sorted in
        let sum = List.fold_left (+.) 0.0 sorted in
        let mean = sum /. Float.of_int count in
        let min_val = List.hd sorted in
        let max_val = List.hd (List.rev sorted) in
        let p95_idx = max 0 (count * 95 / 100 - 1) in
        let p99_idx = max 0 (count * 99 / 100 - 1) in
        let p95 = try List.nth sorted p95_idx with _ -> min_val in
        let p99 = try List.nth sorted p99_idx with _ -> max_val in

        entry.current_stats <- Some {
          count;
          mean;
          min = min_val;
          max = max_val;
          p95;
          p99;
          last_updated = Unix.gettimeofday ();
        }

(** Add a raw value to a metric entry and update statistics *)
let add_value_to_entry (entry: metric_entry) value timestamp =
  let cutoff_time = timestamp -. !config_ref.stats_window_seconds in

  (* Add new value *)
  entry.raw_values <- (timestamp, value) :: entry.raw_values;
  entry.last_accessed <- timestamp;

  (* Filter old values and limit size *)
  let recent_values =
    List.filter (fun (ts, _) -> ts >= cutoff_time) entry.raw_values
    |> fun lst ->
      if List.length lst > !config_ref.max_metrics_per_key then
        let rec take n acc = function
          | [] -> List.rev acc
          | h :: t when n > 0 -> take (n - 1) (h :: acc) t
          | _ -> List.rev acc
        in
        take !config_ref.max_metrics_per_key [] lst
      else lst
  in

  entry.raw_values <- recent_values;

  (* Update incremental statistics *)
  update_incremental_stats entry

(** Record a typed metric (internal function) with sampling *)
let record_typed_metric (component: component_path) name value () : unit Lwt.t =
  (* Fast path: check if telemetry is enabled and if we should sample *)
  if not !config_ref.enabled || not (should_sample ()) then 
    Lwt.return_unit 
  else
    Lwt_mutex.with_lock metrics_mutex (fun () ->
      let key = make_key component name in
      let timestamp = Unix.gettimeofday () in

      match Hashtbl.find_opt metrics_store key with
      | Some entry ->
          add_value_to_entry entry value timestamp;
          Lwt.return_unit
      | None ->
          let new_entry = metric_to_entry component name value timestamp in
          Hashtbl.add metrics_store key new_entry;
          update_incremental_stats new_entry;
          Lwt.return_unit
    )

(** Record a hot-path metric - can be completely disabled for production *)
let record_hot_path_metric (component: component_path) name value () : unit Lwt.t =
  if !config_ref.disable_hot_path_metrics then 
    Lwt.return_unit
  else
    record_typed_metric component name value ()

(** Type-safe metric recording functions *)
let record_counter component name delta =
  record_typed_metric component name (Counter_val (Int64.of_int delta)) ()

let record_gauge component name value =
  record_typed_metric component name (Gauge_val value) ()

let record_timer component name duration =
  record_typed_metric component name (Timer_val duration) ()

let record_histogram component name values =
  if !config_ref.enable_detailed_histograms then
    record_typed_metric component name (Histogram_val values) ()
  else
    (* For non-detailed mode, record only summary statistics *)
    let avg = List.fold_left (+.) 0.0 values /. Float.of_int (List.length values) in
    record_gauge component name avg

(** Hot-path optimized metric recording functions - can be completely disabled *)
let record_timer_hot_path component name duration =
  record_hot_path_metric component name (Timer_val duration) ()

let record_counter_hot_path component name delta =
  record_hot_path_metric component name (Counter_val (Int64.of_int delta)) ()

(** Batch multiple metrics recording for better performance *)
let record_batch_metrics metrics =
  let process_metric (component, name, value) =
    record_typed_metric component name value ()
  in
  Lwt_list.iter_p process_metric metrics

(** High-level timing utility with type safety *)
let time_operation component name operation =
  let start_time = Unix.gettimeofday () in
  operation () >>= fun result ->
  let duration = Unix.gettimeofday () -. start_time in
  record_timer component name duration >>= fun () ->
  Lwt.return result

(** Legacy compatibility functions *)
let increment_counter (legacy_comp: legacy_component) name delta =
  let component_path = match legacy_comp with
    | Feed -> ["feed"]
    | Router -> ["router"]
    | Strategy s -> ["strategy"; s]
    | Exchange s -> ["exchange"; s]
    | Ringbuffer s -> ["ringbuffer"; s]
  in
  record_counter component_path name delta

let record_gauge_legacy legacy_comp name value =
  let component_path = match legacy_comp with
    | Feed -> ["feed"]
    | Router -> ["router"]
    | Strategy s -> ["strategy"; s]
    | Exchange s -> ["exchange"; s]
    | Ringbuffer s -> ["ringbuffer"; s]
  in
  record_gauge component_path name value

let record_timer_legacy legacy_comp name duration =
  let component_path = match legacy_comp with
    | Feed -> ["feed"]
    | Router -> ["router"]
    | Strategy s -> ["strategy"; s]
    | Exchange s -> ["exchange"; s]
    | Ringbuffer s -> ["ringbuffer"; s]
  in
  record_timer component_path name duration

let record_histogram_legacy legacy_comp name values =
  let component_path = match legacy_comp with
    | Feed -> ["feed"]
    | Router -> ["router"]
    | Strategy s -> ["strategy"; s]
    | Exchange s -> ["exchange"; s]
    | Ringbuffer s -> ["ringbuffer"; s]
  in
  record_histogram component_path name values

(** Initialize the telemetry system *)
let init () : unit Lwt.t =
  if not !config_ref.enabled then Lwt.return_unit else
  begin
    (* Start cleanup task if needed *)
    if !config_ref.stats_window_seconds > 0.0 then
      Lwt.async (fun () ->
        let rec cleanup_loop () =
          Lwt_unix.sleep 60.0 >>= fun () -> (* Clean up every minute *)
          Lwt_mutex.with_lock metrics_mutex (fun () ->
            let now = Unix.gettimeofday () in
            let cutoff_time = now -. !config_ref.stats_window_seconds in

            (* Remove old raw values from all entries *)
            Hashtbl.iter (fun _key entry ->
              entry.raw_values <- List.filter (fun (ts, _) -> ts >= cutoff_time) entry.raw_values;
              update_incremental_stats entry;
            ) metrics_store;

            (* Remove entries with no recent data *)
            let to_remove = Hashtbl.fold (fun key entry acc ->
              if entry.raw_values = [] then key :: acc else acc
            ) metrics_store [] in

            List.iter (Hashtbl.remove metrics_store) to_remove;

            Lwt.return_unit
          ) >>= cleanup_loop
        in
        cleanup_loop ()
      );

    Lwt.return_unit
  end

(** Get current statistics for all metrics *)
let get_all_stats () : (string * metric_stats) list Lwt.t =
  Lwt_mutex.with_lock metrics_mutex (fun () ->
    let now = Unix.gettimeofday () in

    (* Update last accessed time for all entries *)
    Hashtbl.iter (fun _ entry -> entry.last_accessed <- now) metrics_store;

    let stats = Hashtbl.fold (fun _key entry acc ->
      match entry.current_stats with
      | Some stats -> (entry.key, stats) :: acc
      | None -> acc
    ) metrics_store [] in

    Lwt.return stats
  )

(** Get statistics for a specific metric *)
let get_metric_stats component_path name : metric_stats option Lwt.t =
  Lwt_mutex.with_lock metrics_mutex (fun () ->
    let key = make_key component_path name in
    match Hashtbl.find_opt metrics_store key with
    | Some entry ->
        entry.last_accessed <- Unix.gettimeofday ();
        Lwt.return entry.current_stats
    | None -> Lwt.return None
  )

(** Export functions for external monitoring systems *)
let export_all_stats () : (string * metric_stats) list Lwt.t =
  get_all_stats ()

let export_raw_metrics () : (string * (float * metric_value) list) list Lwt.t =
  Lwt_mutex.with_lock metrics_mutex (fun () ->
    let now = Unix.gettimeofday () in
    let metrics = Hashtbl.fold (fun _ entry acc ->
      if entry.raw_values <> [] then
        (entry.key, entry.raw_values) :: acc
      else acc
    ) metrics_store [] in

    (* Update access times *)
    Hashtbl.iter (fun _ entry -> entry.last_accessed <- now) metrics_store;

    Lwt.return metrics
  )

(** Reset all metrics (for testing or clean slate) *)
let reset () : unit Lwt.t =
  Lwt_mutex.with_lock metrics_mutex (fun () ->
    Hashtbl.clear metrics_store;
    Lwt.return_unit
  )