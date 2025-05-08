open Alcotest
open Dio_types



(* Helper: round-trip via Yojson ------------------------------------- *)
let json_round_trip ~pp to_json of_json v =
  match of_json (to_json v) with
  | Ok v' ->
      check (testable pp (=)) "json round-trip" (to_json v) (to_json v')
  | Error e ->
      failf "decoding failed: %s" e

(* ------------------------------------------------------------------ *)
let price = Primitives.Price.of_string_exn ~scale:2
let qty   = Primitives.Qty.  of_string_exn ~scale:8

(* Sample values ----------------------------------------------------- *)
let sample_tick : Event.tick = 
  let bid_price = price "65000.12" in
  let ask_price = price "65001.34" in
  {
  src    = "kraken";
  symbol = "BTC/USD";
  bid    = bid_price;
  ask    = ask_price;
  current_price = Primitives.Price.midpoint bid_price ask_price;
  ts     = 1678886400123456L; (* Example timestamp *)
}

let sample_fill : Event.fill = {
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
    Event.tick_to_yojson
    Event.tick_of_yojson
    sample_tick

let test_fill () =
  json_round_trip
    ~pp:Yojson.Safe.pp
    Event.fill_to_yojson
    Event.fill_of_yojson
    sample_fill

(* Test Suite -------------------------------------------------------- *)
let () =
  run "Event"
    [ "JSON Serialization",
      [ test_case "Tick Round Trip" `Quick test_tick
      ; test_case "Fill Round Trip" `Quick test_fill
      ]
    ]
