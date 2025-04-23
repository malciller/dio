open Alcotest
open Types                            (* root module from dio.types *)

module F = Primitives.Fixed           (* alias for brevity *)

(* -------------------------------------------------------------------- *)
let round_trip ~scale s =
  let x = F.of_string_exn ~scale s in
  check string "round-trip" s (F.to_string x)

let test_round_trips () =
  List.iter (fun (scale, s) -> round_trip ~scale s)
    [ 0, "42"
    ; 2, "42.00"
    ; 2, "0.01"
    ; 8, "0.12345678"
    ]

let test_padding () =
  let x = F.of_string_exn ~scale:2 "1" in
  check string "pad zeros" "1.00" (F.to_string x)

let test_truncate () =
  let x = F.of_string_exn ~scale:2 "1.2399" in
  check string "truncate" "1.23" (F.to_string x)

let test_bad_input () =
  check_raises "malformed input"
    (Invalid_argument "Fixed.of_string_exn: malformed decimal")
    (fun () -> ignore (F.of_string_exn ~scale:2 "12.3.4"))

let test_zero_scale () =
  let x = F.of_string_exn ~scale:0 "42" in
  check string "scale 0" "42" (F.to_string x)

(* -------------------------------------------------------------------- *)


let () =
  run "Primitives"
    [ "Fixed",
      [ test_case "round-trips" `Quick test_round_trips
      ; test_case "scale 0"     `Quick test_zero_scale
      ; test_case "pad zeros"   `Quick test_padding
      ; test_case "truncate"    `Quick test_truncate
      ; test_case "bad input"   `Quick test_bad_input
      ]
    ]
