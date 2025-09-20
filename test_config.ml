open Dio_types

let () =
  try
    let json = Yojson.Safe.from_file "_config.json" in
    let cfg = Config.runtime_cfg_of_yojson_exn json in
    print_endline "Config parsed successfully"
  with
  | exn -> Printf.printf "Error: %s\n" (Printexc.to_string exn)
