(**
 * Core primitive types and utilities for the Dio trading system
 *)

(**
 * Fixed-point decimal arithmetic for precise monetary calculations.
 * Stores values as {raw: int64, scale: int} where value = raw / 10^scale.
 * Supports exact arithmetic with up to 18 decimal places.
 *)

module Fixed : sig
  type t = { raw : int64; scale : int }

  (** Parse decimal string to fixed-point value *)
  val of_string_exn : scale:int -> string -> t

  (** Convert fixed-point value to decimal string *)
  val to_string : t -> string

  (** Pretty print fixed-point value *)
  val pp : Format.formatter -> t -> unit

  (** JSON serialization *)
  val to_yojson : t -> Yojson.Safe.t
  val of_yojson : Yojson.Safe.t -> (t, string) result

  (** Calculate arithmetic midpoint of two values *)
  val midpoint : t -> t -> t

  (** Compute 10^n as int64 *)
  val pow10 : int -> int64

  (** Value equality comparison *)
  val equal : t -> t -> bool

  (** Comparison operators *)
  val (<=) : t -> t -> bool

  (** Check if value is positive *)
  val is_positive : t -> bool

  (** Check if value is non-negative *)
  val is_non_negative : t -> bool

  (** Create zero value with given scale *)
  val zero : int -> t

  (** Create unit value (1.0) with given scale *)
  val one : int -> t
end = struct             

  type t = { raw : int64; scale : int }

  (** Compute 10^n using tail recursion *)
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

  (** JSON serialization support *)
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

  let midpoint p1 p2 =
    if p1.scale <> p2.scale then
      invalid_arg "Fixed.midpoint: scales must match";
    let open Int64 in
    let avg_raw = div (add p1.raw p2.raw) 2L in
    { raw = avg_raw; scale = p1.scale }

  let equal p1 p2 =
    p1.scale = p2.scale && p1.raw = p2.raw

  let (<=) p1 p2 =
    if p1.scale <> p2.scale then
      invalid_arg "Fixed.(<=): scales must match";
    p1.raw <= p2.raw

  let is_positive { raw; _ } = raw > 0L

  let is_non_negative { raw; _ } = raw >= 0L

  let zero scale = { raw = 0L; scale }
  
  let one scale = { raw = pow10 scale; scale }
end

(**
 * Type aliases for domain-specific fixed-point values.
 * Both inherit JSON serialization from Fixed module.
 *)
module Price = Fixed  (** Price values with fixed-point precision *)
module Qty = Fixed    (** Quantity values with fixed-point precision *)

(** Core trading types *)
type timestamp = int64 [@@deriving yojson]  (** Microseconds since Unix epoch *)
type symbol = string [@@deriving yojson]    (** Trading pair symbol (e.g., "BTC/USD") *)
type currency = string [@@deriving yojson] (** Currency code (e.g., "USD", "BTC") *)
(**
 * Unique identifier generation for orders and transactions.
 *)
module Id = struct
  (** Generate random 12-character hexadecimal ID *)
  let gen () =
    let r1 = Random.bits () land 0xFFFFFF in
    let r2 = Random.bits () land 0xFFFFFF in
    Printf.sprintf "%06x%06x" r1 r2
end

(** Format float to string with specified decimal precision *)
let format_float_precision (value : float) (precision : int) : string =
  try
    Printf.sprintf "%.*f" precision value
  with _ ->
    string_of_float value

(**
 * Transaction categories for balance tracking and cost basis calculations.
 *)
type transaction_type =
  | Trade of { order_id : string; side : [`Buy | `Sell]; price : Price.t; qty : Qty.t }
      (** Market trade execution *)
  | Deposit    (** External deposit to account *)
  | Withdrawal (** External withdrawal from account *)
  | Staking_Reward (** Staking rewards earned *)
  | Fee       (** Trading or network fees *)
  | Adjustment (** Manual balance adjustment *)
  | Unknown   (** Unclassified transaction *)
[@@deriving yojson]

(**
 * Individual transaction record for portfolio tracking.
 *)
type transaction = {
  id : string;                    (** Unique transaction identifier *)
  asset : string;                 (** Asset symbol (e.g., "BTC") *)
  amount : float;                 (** Transaction amount (positive = credit, negative = debit) *)
  timestamp : timestamp;          (** Transaction timestamp *)
  transaction_type : transaction_type; (** Transaction category *)
  cost_basis : float option;      (** USD cost per unit (for buy trades) *)
  total_cost : float option;      (** Total USD cost of transaction *)
  balance_after : float;          (** Account balance after transaction *)
} [@@deriving yojson]

(**
 * Cost basis tracking for FIFO (First In, First Out) calculations.
 *)
type cost_basis_info = {
  total_units : float;             (** Total units held *)
  total_cost_basis : float;        (** Total USD cost of all units *)
  average_cost_per_unit : float;   (** Average USD cost per unit *)
  last_updated : timestamp;        (** Last update timestamp *)
} [@@deriving yojson]