(* src/engine/router.ml *)
open Lwt.Infix  (* for >>= *)

let start _cfg ~cmd_stream ~on_event =
  (* Pop Core.order_cmd from cmd_stream, execute via Exec handler, feed-back events via on_event *)
  let rec loop () =
    match cmd_stream () with
    | Some cmd ->
        (* TODO: dispatch to exchange exec handler *)
        on_event _cfg cmd >>= fun () -> loop ()
    | None -> Lwt_unix.sleep 0.01 >>= loop
  in
  loop ()
