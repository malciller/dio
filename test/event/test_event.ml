open Alcotest
open Types

module P = Primitives          (* shorter alias *)
module E = Event             (* shorter alias *)

(* Helper: round-trip via Yojson ------------------------------------- *)
let json_round_trip ~pp to_json of_json v =
  match of_json (to_json v) with
  | Ok v' ->
      check (testable pp (=)) "json round-trip" (to_json v) (to_json v')
  | Error e ->
      failf "decoding failed: %s" e

(* ------------------------------------------------------------------ *)
let price = P.Price.of_string_exn ~scale:2
let qty   = P.Qty.  of_string_exn ~scale:8

(* Sample values ----------------------------------------------------- *)
let sample_tick : E.tick = {
  src    = "kraken";
  symbol = "BTC/USD";
  bid    = price "65000.12";
  ask    = price "65001.34";
  ts     = 1678886400123456L; (* Example timestamp *)
}

let sample_fill : E.fill = {
  src       = "binance";
  symbol    = "ETH/USD";
  order_id  = "order-5678";
  side      = `Buy;
  qty       = qty "1.25";
  price     = price "3250.75";
  ts        = 1678886405987654L; (* Example timestamp *)
}

(* Tests ------------------------------------------------------------- *)
let test_tick () =
  json_round_trip
    ~pp:Yojson.Safe.pp
    E.tick_to_yojson
    E.tick_of_yojson
    sample_tick

let test_fill () =
  json_round_trip
    ~pp:Yojson.Safe.pp
    E.fill_to_yojson
    E.fill_of_yojson
    sample_fill

(* Test Suite -------------------------------------------------------- *)
let () =
  run "Event"
    [ "JSON Serialization",
      [ test_case "Tick Round Trip" `Quick test_tick
      ; test_case "Fill Round Trip" `Quick test_fill
      ]
    ]
