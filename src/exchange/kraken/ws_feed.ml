(* src/exchange/kraken/ws_feed.ml *)
open Lwt.Infix

let connect uri =
  Websocket_lwt_unix.connect uri

let start _cfg ~on_tick:_ =
  (* TODO: connect to public WS, subscribe to book, push Event.tick via on_tick *)
  let rec loop () =
    Lwt_unix.sleep 1.0 >>= fun () -> loop ()
  in
  loop ()