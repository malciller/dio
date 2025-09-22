(** Trading command router and exchange handler.

    Routes trading commands to appropriate exchange handlers while providing
    deduplication and error handling. Supports multiple exchanges through
    pluggable handler modules.
*)
open Lwt.Infix
open Dio_types
open Lwt_log_core

(** Standard response format for order operations *)
type order_response = {
  success: bool;
  error: string option;
  result: Yojson.Safe.t option;
}

(** Order deduplication cache to prevent duplicate command processing *)
module OrderCache = struct
  let recent_orders = Hashtbl.create 1024
  let cache_timeout = 10.0

  let make_order_key = function
    | Core.Add { dst; symbol; side; _ } ->
        Printf.sprintf "add:%s:%s:%s"
          dst symbol
          (match side with Buy -> "buy" | Sell -> "sell")
    | Core.Amend { dst; order_id; _ } ->
        Printf.sprintf "amend:%s:%s"
          dst order_id
    | Core.Cancel { dst; order_id } ->
        Printf.sprintf "cancel:%s:%s" dst order_id

  let is_duplicate cmd =
    let key = make_order_key cmd in
    match Hashtbl.find_opt recent_orders key with
    | Some timestamp ->
        let now = Unix.gettimeofday () in
        if now -. timestamp > cache_timeout then (
          Hashtbl.replace recent_orders key now;
          false
        ) else true
    | None ->
        Hashtbl.add recent_orders key (Unix.gettimeofday ());
        false

  (** Remove expired cache entries to prevent memory leaks *)
  let cleanup () =
    let now = Unix.gettimeofday () in
    Hashtbl.filter_map_inplace
      (fun _ timestamp -> 
        if now -. timestamp > cache_timeout then None 
        else Some timestamp)
      recent_orders
end

(** Kraken exchange handler for routing trading commands *)
module KrakenHandler = struct
  let kraken_cmd_queue = Ringbuffer.create 1000

  (** Queue command for Kraken processing *)
  let handle_order _cfg _exec_buffer cmd =
    Ringbuffer.push kraken_cmd_queue cmd

  let rec process_kraken_commands cfg exec_buffer () =
    Ringbuffer.pop kraken_cmd_queue >>= fun cmd ->
    (* Log command details before routing *)
    (match cmd with
     | Core.Add { symbol; side; price; qty; client_id; _ } ->
         info_f ~section:(Lwt_log_core.Section.make "engine.router.kraken")
           "Sending ADD order to Kraken: %s %s @ %s qty=%s (client_id=%s)"
             (match side with Buy -> "BUY" | Sell -> "SELL")
             symbol
             (Primitives.Price.to_string price)
             (Primitives.Qty.to_string qty)
             client_id
     | Core.Amend { dst=_; order_id; symbol; new_price; new_qty; ts=_ } ->
         info_f ~section:(Lwt_log_core.Section.make "engine.router.kraken")
           "Sending AMEND order to Kraken: %s (%s) price=%s qty=%s"
             order_id
             symbol
             (Primitives.Price.to_string new_price)
             (Primitives.Qty.to_string new_qty)
     | Core.Cancel { order_id; _ } ->
         info_f ~section:(Lwt_log_core.Section.make "engine.router.kraken")
           "Sending CANCEL order to Kraken: %s" order_id
    ) >>= fun () ->
    Kraken.Kraken_outgoing_data.handle_router_command cfg cmd exec_buffer >>= fun () ->
    process_kraken_commands cfg exec_buffer ()
end

(** Initialize router with command and execution buffers *)
let start cfg ~cmd_buffer ~exec_buffer =
  let component = Telemetry_types.Router in
  Lwt.async (KrakenHandler.process_kraken_commands cfg exec_buffer);
  let rec cmd_loop () =
    OrderCache.cleanup ();
    let start_time = Unix.gettimeofday () in
    Ringbuffer.pop cmd_buffer >>= fun cmd ->
    Lwt.async (fun () -> Telemetry.increment_counter component "commands_processed" 1);
    if OrderCache.is_duplicate cmd then (
      Lwt.async (fun () -> Telemetry.increment_counter component "duplicate_commands" 1);
      debug_f ~section:(Lwt_log_core.Section.make "engine.router")
        "Dropping duplicate order: %s"
          (match cmd with
          | Add { client_id; _ } -> client_id
          | Amend { order_id; _ } -> order_id
          | Cancel { order_id; _ } -> order_id) >>= fun () ->
      cmd_loop ()
    ) else (
      let cmd_str = match cmd with
      | Add { dst; symbol; side; price; qty; tags; _ } ->
          Printf.sprintf "ADD order: %s %s %s @ %s qty=%s tags=[%s]"
            (match side with Buy -> "BUY" | Sell -> "SELL")
            symbol
            dst
            (Primitives.Price.to_string price)
            (Primitives.Qty.to_string qty)
            (String.concat ";" (List.map (function 
              | `Grid -> "grid"   
              | `Manual -> "manual" 
              | `Rebalance -> "rebalance") tags))
      | Amend { dst; order_id; new_price; new_qty; _ } ->
          Printf.sprintf "AMEND order order_id=%s on %s: price=%s qty=%s"
            order_id
            dst
            (Primitives.Price.to_string new_price)
            (Primitives.Qty.to_string new_qty)
      | Cancel { dst; order_id } ->
          Printf.sprintf "CANCEL order %s on %s" order_id dst
      in
      info_f ~section:(Lwt_log_core.Section.make "engine.router")
        "Routing command: %s" cmd_str >>= fun () ->
      (match cmd with
      | Add { dst; _ } | Amend { dst; _ } | Cancel { dst; _ } ->
          let handle_cmd () =
            match dst with
            | "kraken" ->
                KrakenHandler.handle_order cfg exec_buffer cmd >>= fun () ->
                let duration = Unix.gettimeofday () -. start_time in
                Lwt.async (fun () -> Telemetry.record_timer component "command_processing_time" duration);
                Lwt.return_unit
            | _ ->
                Lwt.async (fun () -> Telemetry.increment_counter component "unknown_exchange" 1);
                error_f ~section:(Lwt_log_core.Section.make "engine.router")
                  "Unknown exchange: %s" dst >>= fun () ->
                Lwt.return_unit
          in
          Lwt.async (fun () ->
            Lwt.catch handle_cmd (fun ex ->
              Lwt.async (fun () -> Telemetry.increment_counter component "command_errors" 1);
              error_f ~section:(Lwt_log_core.Section.make "engine.router")
                "Unhandled exception in command handler: %s" (Printexc.to_string ex) >>= fun () ->
              Lwt.return_unit
            )
          );
          cmd_loop ()
      )
    ) in
  cmd_loop ()