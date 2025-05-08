(* src/types/primitives.ml *)

(*---------------------------------------------------------------------------
  Fixed-scale decimal implementation
  ----------------------------------
  Monetary values are stored as
    { raw : int64 ; scale : int }
  such that   value = raw / 10^scale
  • Exact arithmetic (no floats)
  • Up to 18 decimal places (fits in int64)
---------------------------------------------------------------------------*)

module Fixed : sig
  type t = { raw : int64; scale : int }

  val of_string_exn : scale:int -> string -> t
  val to_string     : t -> string
  val pp            : Format.formatter -> t -> unit

  (* -- JSON helpers for ppx_deriving_yojson ------------------------------ *)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result

  (* Calculate midpoint *)
  val midpoint : t -> t -> t

  (* Calculate powers of 10 *)
  val pow10 : int -> int64
end = struct              (* silence "unused" inside this module *)

  type t = { raw : int64; scale : int }

  (* 10^n helper (n ≤ 18) *)
  let rec pow10 n acc =
    if n = 0 then acc else pow10 (n - 1) Int64.(mul acc 10L)

  let pow10 n = pow10 n 1L

  let of_string_exn ~scale s =
    let int_part, frac_part =
      match String.split_on_char '.' s with
      | [i]    -> (i, "")
      | [i; f] -> (i, f)
      | _      -> invalid_arg "Fixed.of_string_exn: malformed decimal"
    in
    let frac_len = Stdlib.min scale (String.length frac_part) in
    let frac_str =
      String.sub frac_part 0 frac_len ^ String.make (scale - frac_len) '0'
    in
    let open Int64 in
    let frac_int = if frac_str = "" then 0L else of_string frac_str in
    let raw =
      add (mul (of_string int_part) (pow10 scale)) frac_int
    in
    { raw; scale }

  let to_string { raw; scale } =
    let open Int64 in
    let int_part = div raw (pow10 scale) |> to_string in
    if scale = 0 then int_part
    else
      let frac =
        rem raw (pow10 scale) |> to_int |> Printf.sprintf "%0*d" scale
      in
      int_part ^ "." ^ frac

  let pp fmt t = Format.fprintf fmt "%s" (to_string t)

  (* ---------------- JSON helpers ---------------- *)
  let to_yojson t = `String (to_string t)

  let of_yojson = function
    | `String s ->
        let scale =
          match String.index_opt s '.' with
          | None   -> 0
          | Some i -> String.length s - i - 1
        in
        Ok (of_string_exn ~scale s)
    | _ -> Error "Fixed.of_yojson: expected JSON string"

  (* Calculate midpoint *)
  let midpoint p1 p2 =
    if p1.scale <> p2.scale then
      invalid_arg "Fixed.midpoint: scales must match";
    let open Int64 in
    let avg_raw = div (add p1.raw p2.raw) 2L in
    { raw = avg_raw; scale = p1.scale }
end

(* Aliases for clarity – they inherit to_yojson/of_yojson *)
module Price = Fixed
module Qty   = Fixed

(*---------------------------------------------------------------------------
  Misc primitives
---------------------------------------------------------------------------*)
type timestamp = int64  [@@deriving yojson]   (* µs since epoch *)
type symbol    = string [@@deriving yojson]
type currency  = string [@@deriving yojson]


module Id = struct
  let gen () =
    let r1 = Random.bits () land 0xFFFFFF in
    let r2 = Random.bits () land 0xFFFFFF in
    Printf.sprintf "%06x%06x" r1 r2
end

(* Utility to format a float price to a string with specific precision *)
let format_float_precision (value : float) (precision : int) : string =
  try
    Printf.sprintf "%.*f" precision value
  with _ ->
    (* Fallback in case of formatting error *)
    string_of_float value