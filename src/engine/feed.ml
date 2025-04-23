(* src/engine/feed.ml *)
open Lwt.Infix  (* for >>= *)

let start _cfg ~on_tick:_on_tick =
  (* Connect to market stream, subscribe, then push Core.event.tick to on_tick *)
  let rec loop () =
    (* TODO: await next raw event, normalize and call on_tick *)
    Lwt_unix.sleep 1.0 >>= loop
  in
  loop ()