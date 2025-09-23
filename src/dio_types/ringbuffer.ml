
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

module TelemetryInterface = struct
  let set_functions = set_telemetry_functions
end

(** Enhanced ringbuffer with telemetry collection *)
type 'a t = {
  buf : 'a option array;
  mask : int;
  mutable head : int;
  mutable tail : int;
  mutex : Lwt_mutex.t;
  not_full : unit Lwt_condition.t;
  not_empty : unit Lwt_condition.t;
  name : string;
  capacity : int;
}

(** Rounds up to the nearest power of 2 for efficient circular indexing. *)
let round_pow2 n =
  let rec aux p = if p >= n then p else aux (p * 2) in
  aux 1

(** Creates a new ringbuffer with the specified minimum capacity.
    Capacity will be rounded up to the nearest power of 2. *)
let create ?(name = "ringbuffer") cap =
  let cap = round_pow2 cap in
  {
    buf = Array.make cap None;
    mask = cap - 1;
    head = 0;
    tail = 0;
    mutex = Lwt_mutex.create ();
    not_full = Lwt_condition.create ();
    not_empty = Lwt_condition.create ();
    name;
    capacity = cap;
  }

(** Returns the current number of elements in the ringbuffer. *)
let length q = q.head - q.tail

(** Returns the current fill percentage of the ringbuffer. *)
let fill_percentage q =
  let len = length q in
  let cap = q.capacity in
  if cap = 0 then 0.0 else (Float.of_int len /. Float.of_int cap) *. 100.0

(** Returns true if the ringbuffer is at maximum capacity. *)
let is_full q = length q = q.capacity

(** Returns true if the ringbuffer contains no elements. *)
let is_empty q = q.head = q.tail

(** Record queue depth gauge - currently unused, kept for compatibility *)
(* let record_depth q =
  let depth = length q in
  let fill_pct = fill_percentage q in
  !telemetry_record_gauge ["ringbuffer"; q.name] "depth" (Float.of_int depth) >>= fun () ->
  !telemetry_record_gauge ["ringbuffer"; q.name] "fill_percentage" fill_pct *)

(** Event-driven callback system *)
type 'a consumer_callback = 'a -> unit Lwt.t

let push_with_callback q v callback =
  Lwt_mutex.with_lock q.mutex (fun () ->
    let rec wait_if_full () =
      if is_full q then (
        let wait_start_time = Unix.gettimeofday () in
        Lwt_log_core.debug_f ~section "Ringbuffer %s full, waiting to push." q.name >>= fun () ->
        Lwt_condition.wait ~mutex:q.mutex q.not_full >>= fun () ->
        let wait_duration = Unix.gettimeofday () -. wait_start_time in
        Lwt.async (fun () ->
          !telemetry_record_timer ["ringbuffer"; q.name] "buffer_full_wait_duration" wait_duration
        );
        wait_if_full ()
      ) else
        Lwt.return_unit
    in
    wait_if_full () >>= fun () ->

    q.buf.(q.head land q.mask) <- Some v;
    q.head <- q.head + 1;
    Lwt_condition.signal q.not_empty ();

    Lwt_log_core.debug_f ~section "Pushed item to ringbuffer %s, triggering callback." q.name |> Lwt.ignore_result;

    (* Immediately trigger callback with the item *)
    Lwt.async (fun () ->
      Lwt.catch (fun () -> callback v) (fun exn ->
        Lwt_log_core.error_f ~section "Callback error in ringbuffer %s: %s"
          q.name (Printexc.to_string exn)
      )
    );

    Lwt.return_unit
  )

(** Non-blocking consumer registration - processes items as they arrive *)
let create_consumer q ~name ~processor =
  let consumer_name = q.name ^ "_" ^ name ^ "_consumer" in
  let rec consume_loop () =
    Lwt.catch (fun () ->
      Lwt_mutex.with_lock q.mutex (fun () ->
        if is_empty q then
          (* Wait for data to arrive *)
          Lwt_condition.wait ~mutex:q.mutex q.not_empty
        else
          Lwt.return_unit
      ) >>= fun () ->

      (* Process all available items *)
      let rec drain_and_process count =
        Lwt_mutex.with_lock q.mutex (fun () ->
          if is_empty q then
            Lwt.return_none
          else
            let idx = q.tail land q.mask in
            match q.buf.(idx) with
            | None -> Lwt.return_none
            | Some v ->
                q.buf.(idx) <- None;
                q.tail <- q.tail + 1;
                Lwt_condition.signal q.not_full ();
                Lwt.return_some v
        ) >>= function
        | None -> Lwt.return count
        | Some item ->
            let process_start = Unix.gettimeofday () in
            processor item >>= fun () ->
            let process_duration = Unix.gettimeofday () -. process_start in

            (* Record processing metrics *)
            Lwt.async (fun () ->
              !telemetry_record_timer ["ringbuffer"; q.name; consumer_name]
                "item_processing_duration" process_duration
            );

            drain_and_process (count + 1)
      in

      let batch_start = Unix.gettimeofday () in
      drain_and_process 0 >>= fun items_processed ->
      let batch_duration = Unix.gettimeofday () -. batch_start in

      if items_processed > 0 then
        Lwt.async (fun () ->
          !telemetry_record_timer ["ringbuffer"; q.name; consumer_name]
            "batch_processing_duration" batch_duration >>= fun () ->
          !telemetry_record_counter ["ringbuffer"; q.name; consumer_name]
            "items_processed" items_processed >>= fun () ->
          if items_processed > 1 then
            !telemetry_record_timer ["ringbuffer"; q.name; consumer_name]
              "per_item_avg_duration"
              (batch_duration /. Float.of_int items_processed)
          else
            Lwt.return_unit
        );

      consume_loop ()
    ) (fun exn ->
      Lwt_log_core.error_f ~section "Consumer %s error: %s"
        consumer_name (Printexc.to_string exn) >>= fun () ->
      (* Brief pause before retrying to avoid tight error loops *)
      Lwt_unix.sleep 0.1 >>= fun () ->
      consume_loop ()
    )
  in

  (* Start the consumer asynchronously *)
  Lwt.async consume_loop;
  Lwt.async (fun () ->
    Lwt_log_core.info_f ~section "Started event-driven consumer: %s" consumer_name
  );
  ()

(** Non-blocking pop that returns None if buffer is empty *)
let try_pop q =
  Lwt_mutex.with_lock q.mutex (fun () ->
    if is_empty q then
      Lwt.return_none
    else
      let idx = q.tail land q.mask in
      match q.buf.(idx) with
      | None ->
          Lwt_log_core.error_f ~section "Ringbuffer.try_pop: Impossible state - None at non-empty index %d." idx >>= fun () ->
          Lwt.return_none
      | Some v ->
          q.buf.(idx) <- None;
          q.tail <- q.tail + 1;
          Lwt_condition.signal q.not_full ();
          Lwt.return_some v
  )

(** Pop with timeout - returns None if timeout occurs *)
let pop_with_timeout q timeout_seconds =
  let timeout_promise = Lwt_unix.sleep timeout_seconds >>= fun () -> Lwt.return_none in
  let rec poll_loop () =
    try_pop q >>= function
    | Some v -> Lwt.return_some v
    | None -> Lwt_unix.sleep 0.01 >>= poll_loop
  in
  Lwt.pick [timeout_promise; poll_loop ()]

(** Direct push without callback (maintains compatibility) *)
let push q v = push_with_callback q v (fun _ -> Lwt.return_unit)

(** Blocking pop that waits until an item is available. *)
let rec pop q =
  Lwt_mutex.with_lock q.mutex (fun () ->
    if is_empty q then
      Lwt_condition.wait ~mutex:q.mutex q.not_empty
    else
      Lwt.return_unit
  ) >>= fun () ->
  Lwt_mutex.with_lock q.mutex (fun () ->
    if is_empty q then
      Lwt.return_none
    else
      let idx = q.tail land q.mask in
      match q.buf.(idx) with
      | None ->
          Lwt_log_core.error_f ~section "Ringbuffer.pop: None at index %d despite non-empty state." idx >>= fun () ->
          Lwt.return_none
      | Some v ->
          q.buf.(idx) <- None;
          q.tail <- q.tail + 1;
          Lwt_condition.signal q.not_full ();
          Lwt.return_some v
  ) >>= function
  | Some v -> Lwt.return v
  | None -> pop q
