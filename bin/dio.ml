open Lwt.Infix
open Types           (* Access Event.tick *)
open Types.Primitives (* Access Price.to_string *)

(* Import the Kraken feed module *)
module Kraken = Kraken

(* Set up logging *)
let setup_logging () =
  Lwt_log.add_rule "*" Lwt_log_core.Info; (* Log Info level and above for all sections *)
  Lwt_log_core.default := Lwt_log.channel ~close_mode:`Keep ~channel:Lwt_io.stdout ()

(* Define the feed start function *)
let start_feed cfg ~on_tick =
  (* Connect to market stream (Kraken WS feed) *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.feed") "Starting Kraken WebSocket feed..." >>= fun () ->
  Kraken.Ws_feed.start cfg ~on_tick

(* Define the execution feed start function *)
let start_executions ~on_execution =
  (* Connect to execution stream (Kraken WS feed) *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.feed") "Starting Kraken execution feed..." >>= fun () ->
  Kraken.Ws_feed.start_executions ~on_execution

(* Define the callback function for handling ticks *)
let on_tick (tick : Event.tick) : unit Lwt.t =
  Printf.printf "Tick [%s] %s: Bid=%s Ask=%s Ts=%Ld\n%!"
    tick.src
    tick.symbol
    (Price.to_string tick.bid)
    (Price.to_string tick.ask)
    tick.ts;
  Lwt.return_unit

(* Define the callback function for handling executions *)
let on_execution (reports : Kraken.Ws_feed.execution_report list) : unit Lwt.t =
  Lwt_list.iter_s (fun (report : Kraken.Ws_feed.execution_report) ->
    let exec_type = report.exec_type in
    let order_id = report.order_id in
    let order_status = report.order_status in
    let symbol = Option.value report.symbol ~default:"unknown" in
    let side = Option.value report.side ~default:"N/A" in

    (* Extract price and quantity based on exec_type *)
    let price, qty = match exec_type with
      | "new" | "pending_new" | "canceled" | "expired" | "restated" | "status" ->
          (Option.value report.limit_price ~default:0.0, Option.value report.order_qty ~default:0.0)
      | "trade" ->
          (Option.value report.last_price ~default:0.0, Option.value report.last_qty ~default:0.0)
      | _ -> (0.0, 0.0) (* Default for unknown types *)
    in

    Lwt_io.printf "[EXEC] %s %s: type=%s status=%s side=%s price=%.8f qty=%.8f\n"
      symbol order_id exec_type order_status side price qty
  ) reports

let main () =
  (* Initialize Logging FIRST *)
  setup_logging ();

  (* Initialize the default RNG for crypto operations (TLS) using the Unix backend *)
  Mirage_crypto_rng_unix.use_default ();

  (* Load .env file to get the API token *)
  (try Dotenv.export ~path:"src/exchange/kraken/.env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!");

  Printf.printf "Starting market and execution feeds...\n%!";

  (* Start both feeds in parallel *)
  Lwt_main.run (
    Lwt.catch
      (fun () ->
        let dummy_cfg = () in 
        Lwt.join [
          start_feed dummy_cfg ~on_tick;
          start_executions ~on_execution
        ]
      )
      (fun exn ->
        (* Log errors from starting the feed *)
        Printf.eprintf "Error in feed: %s\n%!" (Printexc.to_string exn);
        Lwt.return_unit
      )
  )

let () = main ()
