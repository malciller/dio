(** Improved telemetry system for tracking trading engine performance *)

(** Configuration for telemetry system behavior *)
type telemetry_config = {
  enabled: bool;                           (** Whether telemetry is enabled *)
  max_metrics_per_key: int;               (** Maximum metrics to keep per key *)
  stats_window_seconds: float;            (** Time window for statistics *)
  enable_detailed_histograms: bool;       (** Whether to collect detailed histograms *)
  enable_incremental_stats: bool;         (** Whether to maintain incremental statistics *)
  export_interval_seconds: float option; (** Optional export interval *)
  sampling_rate: float;                   (** Sample rate (0.0-1.0), 0.01 = 1% sampling *)
  disable_hot_path_metrics: bool;         (** Disable metrics in hot paths (ringbuffer item processing) *)
}

(** Default telemetry configuration *)
let default_config = {
  enabled = true;
  max_metrics_per_key = 1000;
  stats_window_seconds = 300.0; (* 5 minutes *)
  enable_detailed_histograms = false;
  enable_incremental_stats = true;
  export_interval_seconds = None;
  sampling_rate = 1.0; (* 100% sampling by default *)
  disable_hot_path_metrics = false;
}

(** Production telemetry configuration with aggressive sampling *)
let production_config = {
  enabled = true;
  max_metrics_per_key = 500;
  stats_window_seconds = 300.0;
  enable_detailed_histograms = false;
  enable_incremental_stats = true;
  export_interval_seconds = None;
  sampling_rate = 0.01; (* 1% sampling *)
  disable_hot_path_metrics = true; (* Disable hot path metrics *)
}

(** Phantom types for type-safe metric definitions *)
type counter = Counter
type gauge = Gauge
type timer = Timer
type histogram = Histogram

(** Type-safe metric name definitions *)
module type METRIC_NAME = sig
  val name: string
  val component: string
  val metric_type: string
end

(** Component definitions with hierarchical structure *)
type component_path = string list

(** Individual metric with type-safe metadata *)
type 'a typed_metric = {
  component: component_path;
  name: string;
  value: 'a;
  timestamp: float;
  tags: (string * string) list;
}

(** Supported metric value types *)
type metric_value =
  | Counter_val of int64
  | Gauge_val of float
  | Timer_val of float
  | Histogram_val of float list

(** Aggregated statistics for a metric *)
type metric_stats = {
  count: int;
  mean: float;
  min: float;
  max: float;
  p95: float;
  p99: float;
  last_updated: float;
}

(** Internal metric storage entry *)
type metric_entry = {
  key: string;                    (** Composite key for the metric *)
  component_path: component_path; (** Hierarchical component path *)
  metric_name: string;            (** Metric name *)
  mutable current_stats: metric_stats option;  (** Current aggregated stats *)
  mutable raw_values: (float * metric_value) list;  (** Raw values with timestamps *)
  metric_type: string;           (** Type of metric for display purposes *)
  mutable last_accessed: float;  (** Last time this metric was accessed *)
}

(** Legacy component type for backward compatibility *)
type legacy_component =
  | Feed
  | Router
  | Strategy of string
  | Exchange of string
  | Ringbuffer of string

(** Utility functions for working with component paths *)
module Component = struct
  let make = List.filter (fun s -> s <> "")
  let to_string path = String.concat "." path
  let from_string s = String.split_on_char '.' s |> make
  let to_legacy = function
    | ["feed"] -> Feed
    | ["router"] -> Router
    | "strategy" :: name :: _ -> Strategy name
    | "exchange" :: name :: _ -> Exchange name
    | "ringbuffer" :: name :: _ -> Ringbuffer name
    | _ -> failwith "Invalid component path"
end
