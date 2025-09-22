(** Telemetry system for tracking trading engine performance *)


(** Performance metric types *)
type metric_type =
  | Counter of int64       (** Monotonically increasing counter *)
  | Gauge of float         (** Point-in-time value *)
  | Histogram of float list (** Distribution of values *)
  | Timer of float         (** Duration measurement in seconds *)

(** Telemetry event categories *)
type component = 
  | Feed 
  | Router 
  | Strategy of string 
  | Exchange of string
  | Ringbuffer of string

(** Individual metric with metadata *)
type metric = {
  component: component;
  name: string;
  value: metric_type;
  timestamp: float;
  tags: (string * string) list;
}

(** Aggregated statistics for a metric *)
type metric_stats = {
  count: int;
  mean: float;
  min: float;
  max: float;
  p95: float;
  p99: float;
}
