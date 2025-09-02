
(* src/types/ringbuffer.ml *)

open Lwt.Infix


let section = Lwt_log_core.Section.make "dio_types.ringbuffer" 

type 'a t = {
  buf       : 'a option array;
  mask      : int;
  mutable head : int;
  mutable tail : int;
  mutex     : Lwt_mutex.t;
  not_full  : unit Lwt_condition.t;
  not_empty : unit Lwt_condition.t;
}

let round_pow2 n =
  let rec aux p = if p >= n then p else aux (p * 2) in
  aux 1

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

let length q = q.head - q.tail

let is_full q  = length q = Array.length q.buf
let is_empty q = q.head = q.tail

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
        (* Should be unreachable due to the wait_if_empty logic *)
        Lwt_log_core.error_f ~section "Ringbuffer.pop: Impossible state reached - encountered None at index %d while buffer expected to be non-empty." idx >>= fun () -> 
        Lwt.fail (Failure "Ringbuffer.pop: Impossible state reached") 
    | Some v ->
        q.buf.(idx) <- None;
        q.tail <- q.tail + 1;
        Lwt_condition.signal q.not_full ();
        Lwt_log_core.debug_f ~section "Popped item from ringbuffer, signaling not_full." |> Lwt.ignore_result; 
        Lwt.return v
  )
