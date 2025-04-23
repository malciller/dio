(* src/exchange/kraken/common.ml *)

let nonce () =
  let ms = Int64.of_float (Unix.gettimeofday () *. 1000.) in
  Int64.to_string ms

(* Corrected Kraken signing function *)
let sign ~secret ~path ~body ~nonce =
  let payload = nonce ^ body in 
  let sha256_hash_raw = Digestif.SHA256.digest_string payload |> Digestif.SHA256.to_raw_string in
  let message_bytes = Bytes.cat (Bytes.of_string path) (Bytes.of_string sha256_hash_raw) in
  let decoded_secret = Base64.decode_exn secret in
  let hmac_hash_raw = Digestif.SHA512.hmac_bytes ~key:decoded_secret message_bytes |> Digestif.SHA512.to_raw_string in
  Base64.encode_string hmac_hash_raw (* Use Base64 encoding *)

let parse_json_field json (path : string) =
  Yojson.Safe.Util.member path json