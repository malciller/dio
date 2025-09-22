
(** Thread-safe circular buffer implementation using Lwt for concurrency control.

    Provides blocking push/pop operations with automatic capacity management.
    Uses power-of-2 sizing for efficient indexing via bitwise AND masking. *)

open Lwt.Infix

let section = Lwt_log_core.Section.make "dio_types.ringbuffer"

(** Telemetry interface - functions that can be set by external modules *)
let telemetry_record_timer : (string list -> string -> float -> unit Lwt.t) ref = 
  ref (fun _ _ _ -> Lwt.return_unit)
let telemetry_record_counter : (string list -> string -> int -> unit Lwt.t) ref = 
  ref (fun _ _ _ -> Lwt.return_unit)
let telemetry_record_gauge : (string list -> string -> float -> unit Lwt.t) ref = 
  ref (fun _ _ _ -> Lwt.return_unit)

(** Set telemetry functions from external modules *)
let set_telemetry_functions record_timer record_counter record_gauge =
  telemetry_record_timer := record_timer;
  telemetry_record_counter := record_counter;
  telemetry_record_gauge := record_gauge

(** Module interface to expose telemetry setup *)
module TelemetryInterface = struct
  let set_functions = set_telemetry_functions
end

(** Enhanced ringbuffer with telemetry collection *)
type 'a t = {
  buf       : 'a option array;
  mask      : int;
  mutable head : int;
  mutable tail : int;
  mutex     : Lwt_mutex.t;
  not_full  : unit Lwt_condition.t;
  not_empty : unit Lwt_condition.t;
  name      : string;  (** Buffer identifier for telemetry *)
  capacity  : int;     (** Actual capacity (power of 2) *)
}

(** Rounds up to the nearest power of 2 for efficient circular indexing. *)
let round_pow2 n =
  let rec aux p = if p >= n then p else aux (p * 2) in
  aux 1

(** Creates a new ringbuffer with the specified minimum capacity.
    Capacity will be rounded up to the nearest power of 2. *)
let create ?(name="ringbuffer") cap =
  let cap = round_pow2 cap in
  {
    buf       = Array.make cap None;
    mask      = cap - 1;
    head      = 0;
    tail      = 0;
    mutex     = Lwt_mutex.create ();
    not_full  = Lwt_condition.create ();
    not_empty = Lwt_condition.create ();
    name      = name;
    capacity  = cap;
  }

(** Returns the current number of elements in the ringbuffer. *)
let length q = q.head - q.tail

(** Returns the current fill percentage of the ringbuffer. *)
let fill_percentage q =
  let len = length q in
  let cap = q.capacity in
  if cap = 0 then 0.0 else (Float.of_int len /. Float.of_int cap) *. 100.0

(** Returns true if the ringbuffer is at maximum capacity. *)
let is_full q  = length q = q.capacity

(** Returns true if the ringbuffer contains no elements. *)
let is_empty q = q.head = q.tail

(** Record queue depth gauge - REMOVED: Non-duration metric *)
(* let record_depth q =
  let depth = length q in
  let fill_pct = fill_percentage q in
  !telemetry_record_gauge ["ringbuffer"; q.name] "depth" (Float.of_int depth) >>= fun () ->
  !telemetry_record_gauge ["ringbuffer"; q.name] "fill_percentage" fill_pct *)

(** Adds an element to the ringbuffer. Blocks if buffer is full.
    Thread-safe and signals waiting consumers when data becomes available. *)
let push q v =
  Lwt_mutex.with_lock q.mutex (fun () ->
    let rec wait_if_full () =
      if is_full q then (
        let wait_start_time = Unix.gettimeofday () in
        Lwt_log_core.debug_f ~section "Ringbuffer %s full, waiting to push." q.name >>= fun () ->
        Lwt_condition.wait ~mutex:q.mutex q.not_full >>= fun () ->
        let wait_duration = Unix.gettimeofday () -. wait_start_time in
        Lwt.async (fun () -> !telemetry_record_timer ["ringbuffer"; q.name] "buffer_full_wait_duration" wait_duration);
        wait_if_full ()
      ) else
        Lwt.return_unit
    in
    wait_if_full () >>= fun () ->
    q.buf.(q.head land q.mask) <- Some v;
    q.head <- q.head + 1;
    Lwt_condition.signal q.not_empty ();

    (* Removed non-duration metrics - depth and fill_percentage gauges *)

    Lwt_log_core.debug_f ~section "Pushed item to ringbuffer %s, signaling not_empty." q.name |> Lwt.ignore_result;
    Lwt.return_unit
  )

(** Removes and returns the oldest element from the ringbuffer. Blocks if buffer is empty.
    Thread-safe and signals waiting producers when space becomes available. *)
let pop q =
  let start_time = Unix.gettimeofday () in
  Lwt_mutex.with_lock q.mutex (fun () ->
    let rec wait_if_empty () =
      if is_empty q then (
        let wait_start_time = Unix.gettimeofday () in
        Lwt_log_core.debug_f ~section "Ringbuffer %s empty, waiting to pop." q.name >>= fun () ->
        Lwt_condition.wait ~mutex:q.mutex q.not_empty >>= fun () ->
        let wait_duration = Unix.gettimeofday () -. wait_start_time in
        Lwt.async (fun () -> !telemetry_record_timer ["ringbuffer"; q.name] "buffer_empty_wait_duration" wait_duration);
        wait_if_empty ()
      ) else
        Lwt.return_unit
    in
    wait_if_empty () >>= fun () ->
    let idx = q.tail land q.mask in
    match q.buf.(idx) with
    | None ->
        (* This should never happen due to wait_if_empty logic above *)
        Lwt_log_core.error_f ~section "Ringbuffer.pop: Impossible state reached - encountered None at index %d while buffer expected to be non-empty." idx >>= fun () ->
        Lwt.fail (Failure "Ringbuffer.pop: Impossible state reached")
    | Some v ->
        q.buf.(idx) <- None;
        q.tail <- q.tail + 1;
        Lwt_condition.signal q.not_full ();

    (* Record timing metrics for performance analysis - simplified *)
    let duration = Unix.gettimeofday () -. start_time in
    Lwt.async (fun () ->
      !telemetry_record_timer ["ringbuffer"; q.name] "pop_duration" duration
    );

        Lwt_log_core.debug_f ~section "Popped item from ringbuffer %s, signaling not_full." q.name |> Lwt.ignore_result;
        Lwt.return v
  )
