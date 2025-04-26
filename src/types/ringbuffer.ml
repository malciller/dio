
(* src/types/ringbuffer.ml *)
type 'a t = {
  buf   : 'a option array;
  mask  : int;             (* capacity - 1, when cap is power-of-2 *)
  mutable head : int;      (* next slot to write *)
  mutable tail : int;      (* next slot to read  *)
}

let round_pow2 n =
  let rec aux p = if p >= n then p else aux (p * 2) in
  aux 1

let create cap =
  let cap = round_pow2 cap in
  { buf = Array.make cap None; mask = cap - 1; head = 0; tail = 0 }

let length q = q.head - q.tail

let is_full q  = length q = Array.length q.buf
let is_empty q = q.head = q.tail

let push q v =
  if is_full q then false
  else (
    q.buf.(q.head land q.mask) <- Some v;
    q.head <- q.head + 1;
    true)

let pop_opt q =
  if is_empty q then None
  else
    let idx = q.tail land q.mask in
    match q.buf.(idx) with
    | None -> None (* impossible *)
    | Some v ->
        q.buf.(idx) <- None;
        q.tail <- q.tail + 1;
        Some v

let peek_opt q =
  if is_empty q then None
  else
    let idx = q.tail land q.mask in
    q.buf.(idx)
