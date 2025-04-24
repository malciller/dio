(* src/engine/engine.ml *)
open Lwt.Infix  (* for >>= *)

module Feed = Feed

let run ~start_feed ~start_strategy ~start_router cfg =
  start_feed cfg ~on_tick:(fun _ -> ()) >>= fun () ->
  start_strategy cfg ~tick_stream:(fun () -> None) ~send_cmd:(fun _ _ -> Lwt.return_unit) >>= fun () ->
  start_router cfg ~cmd_stream:(fun () -> None) ~on_event:(fun _ _ -> Lwt.return_unit)
