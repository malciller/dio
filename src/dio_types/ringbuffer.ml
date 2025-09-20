
(** Thread-safe circular buffer implementation using Lwt for concurrency control.

    Provides blocking push/pop operations with automatic capacity management.
    Uses power-of-2 sizing for efficient indexing via bitwise AND masking. *)

open Lwt.Infix

let section = Lwt_log_core.Section.make "dio_types.ringbuffer"

(** Ringbuffer type parameterized by element type 'a.
    - buf: Underlying array storing optional values (None = empty slot)
    - mask: Bitmask for efficient circular indexing (capacity - 1)
    - head/tail: Producer/consumer indices
    - mutex: Protects concurrent access
    - not_full/not_empty: Condition variables for blocking operations *)
type 'a t = {
  buf       : 'a option array;
  mask      : int;
  mutable head : int;
  mutable tail : int;
  mutex     : Lwt_mutex.t;
  not_full  : unit Lwt_condition.t;
  not_empty : unit Lwt_condition.t;
}

(** Rounds up to the nearest power of 2 for efficient circular indexing. *)
let round_pow2 n =
  let rec aux p = if p >= n then p else aux (p * 2) in
  aux 1

(** Creates a new ringbuffer with the specified minimum capacity.
    Capacity will be rounded up to the nearest power of 2. *)
let create cap =
  let cap = round_pow2 cap in
  {
    buf       = Array.make cap None;
    mask      = cap - 1;
    head      = 0;
    tail      = 0;
    mutex     = Lwt_mutex.create ();
    not_full  = Lwt_condition.create ();
    not_empty = Lwt_condition.create ();
  }

(** Returns the current number of elements in the ringbuffer. *)
let length q = q.head - q.tail

(** Returns true if the ringbuffer is at maximum capacity. *)
let is_full q  = length q = Array.length q.buf

(** Returns true if the ringbuffer contains no elements. *)
let is_empty q = q.head = q.tail

(** Adds an element to the ringbuffer. Blocks if buffer is full.
    Thread-safe and signals waiting consumers when data becomes available. *)
let push q v =
  Lwt_mutex.with_lock q.mutex (fun () ->
    let rec wait_if_full () =
      if is_full q then (
        Lwt_log_core.debug_f ~section "Ringbuffer full, waiting to push." >>= fun () -> 
        Lwt_condition.wait ~mutex:q.mutex q.not_full >>= wait_if_full
      ) else
        Lwt.return_unit
    in
    wait_if_full () >>= fun () ->
    q.buf.(q.head land q.mask) <- Some v;
    q.head <- q.head + 1;
    Lwt_condition.signal q.not_empty ();
    Lwt_log_core.debug_f ~section "Pushed item to ringbuffer, signaling not_empty." |> Lwt.ignore_result; 
    Lwt.return_unit
  )

(** Removes and returns the oldest element from the ringbuffer. Blocks if buffer is empty.
    Thread-safe and signals waiting producers when space becomes available. *)
let pop q =
  Lwt_mutex.with_lock q.mutex (fun () ->
    let rec wait_if_empty () =
      if is_empty q then (
        Lwt_log_core.debug_f ~section "Ringbuffer empty, waiting to pop." >>= fun () -> 
        Lwt_condition.wait ~mutex:q.mutex q.not_empty >>= wait_if_empty
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
        Lwt_log_core.debug_f ~section "Popped item from ringbuffer, signaling not_full." |> Lwt.ignore_result; 
        Lwt.return v
  )
