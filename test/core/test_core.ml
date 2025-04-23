open Alcotest
open Types

module P = Primitives          (* shorter alias *)

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
let add_cmd =
  Core.Add
    { dst        = "Binance"
    ; client_id = "cli-123"
    ; symbol    = "ETH/USD"
    ; side      = Buy
    ; price     = price "3200.55"
    ; qty       = qty   "0.50000000"
    ; tif       = GTC
    ; tags      = [ `Grid ]
    }

let heartbeat_evt = Core.Heartbeat 1L

let book_evt =
  Core.Book
    { symbol = "ETH/USD"
    ; bid    = price "3199.00"
    ; ask    = price "3200.00"
    ; ts     = 2L
    }

(* Tests ------------------------------------------------------------- *)
let test_order_cmd () =
  json_round_trip
    ~pp:Yojson.Safe.pp
    Core.order_cmd_to_yojson
    Core.order_cmd_of_yojson
    add_cmd

let test_market_event () =
  List.iter (fun evt ->
      json_round_trip
        ~pp:Yojson.Safe.pp
        Core.market_event_to_yojson
        Core.market_event_of_yojson
        evt)
    [ heartbeat_evt; book_evt ]

(* ------------------------------------------------------------------ *)
let () =
  run "Core"
    [ "JSON",
      [ test_case "order_cmd"    `Quick test_order_cmd
      ; test_case "market_event" `Quick test_market_event
      ]
    ]
