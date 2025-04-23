open Lwt.Infix
open Types           (* Access Event.tick *)
open Types.Primitives (* Access Price.to_string *)

(* Inline the Feed module's content directly *)
(* Import the Kraken feed module *)
module Kraken_feed = Kraken.Ws_feed

(* Define this function directly, copied from src/engine/feed.ml *)
let start_feed cfg ~on_tick =
  (* Connect to market stream (Kraken WS feed) *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.feed") "Starting Kraken WebSocket feed..." >>= fun () ->
  Kraken_feed.start cfg ~on_tick

(* Define the callback function for handling ticks *)
let on_tick (tick : Event.tick) : unit Lwt.t =
  Printf.printf "Tick [%s] %s: Bid=%s Ask=%s Ts=%Ld\n%!"
    tick.src
    tick.symbol
    (Price.to_string tick.bid)
    (Price.to_string tick.ask)
    tick.ts;
  Lwt.return_unit

let main () =
  (* Initialize the default RNG for crypto operations (TLS) using the Unix backend *) 
  Mirage_crypto_rng_unix.use_default ();

  (* Optional: Load .env file if needed by any part, though likely not for feed *)
  (* (try Dotenv.export ~path:"src/exchange/kraken/.env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!"); *)

  Lwt_main.run (
    Lwt.catch
      (fun () ->
        (* Start the feed engine *)
        Printf.printf "Starting feed engine...\n%!";
        let dummy_cfg = () in 
        (* Use our inlined function *)
        start_feed dummy_cfg ~on_tick
      )
      (fun exn ->
        (* Log errors from starting the feed *)
        Printf.eprintf "Error starting feed: %s\n%!" (Printexc.to_string exn);
        Lwt.return_unit
      )
  )

let () = main ()
