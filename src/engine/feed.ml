(* src/engine/feed.ml *)
open Lwt.Infix  (* for >>= *)
open Types (* To access Event.tick type for on_tick *)

(* Import the Kraken feed module *)
module Kraken_feed = Kraken.Ws_feed

let start cfg ~on_tick:(on_tick : Event.tick -> unit Lwt.t) =
  (* Connect to market stream (Kraken WS feed) *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.feed") "Starting Kraken WebSocket feed..." >>= fun () ->
  Kraken_feed.start cfg ~on_tick