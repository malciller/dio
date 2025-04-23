(* src/exchange/kraken/ws_exec.ml *)
open Lwt.Infix

let start _cfg ~cmd_stream ~on_event =
  (* TODO: connect to authenticated WS, handle addOrder/cancelOrder RPCs and publish fills via on_event *)
  let rec loop () =
    match cmd_stream () with
    | Some cmd ->
        (* send on WS *)
        on_event _cfg cmd >>= fun () -> loop ()
    | None -> Lwt_unix.sleep 0.01 >>= fun () -> loop ()
  in
  loop ()