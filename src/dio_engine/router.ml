(** Trading command router and exchange handler.

    Routes trading commands to appropriate exchange handlers while providing
    deduplication and error handling. Supports multiple exchanges through
    pluggable handler modules.
*)
open Lwt.Infix
open Dio_types
open Lwt_log_core

(** Ringbuffer telemetry interface for this module *)
module RingbufferTelemetryInterface = struct
  let set_functions = Ringbuffer.TelemetryInterface.set_functions
end

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
    let lookup_start = Unix.gettimeofday () in
    let key = make_order_key cmd in
    let result = match Hashtbl.find_opt recent_orders key with
    | Some timestamp ->
        let now = Unix.gettimeofday () in
        if now -. timestamp > cache_timeout then (
          Hashtbl.replace recent_orders key now;
          false
        ) else true
    | None ->
        Hashtbl.add recent_orders key (Unix.gettimeofday ());
        false
    in
    let lookup_duration = Unix.gettimeofday () -. lookup_start in
    Lwt.async (fun () ->
      Telemetry.record_timer ["router"; "cache"] "lookup_duration" lookup_duration
      (* Removed cache size gauge - non-duration metric *)
    );
    result

  (** Remove expired cache entries to prevent memory leaks *)
  let cleanup () =
    let cleanup_start = Unix.gettimeofday () in
    let now = Unix.gettimeofday () in
    Hashtbl.filter_map_inplace
      (fun _ timestamp -> 
        if now -. timestamp > cache_timeout then None 
        else Some timestamp)
      recent_orders;
    let cleanup_duration = Unix.gettimeofday () -. cleanup_start in
    Lwt.async (fun () ->
      Telemetry.record_timer ["router"; "cache"] "cleanup_duration" cleanup_duration
    )
end

(** Kraken exchange handler for routing trading commands *)
module KrakenHandler = struct
  let kraken_cmd_queue = Ringbuffer.create ~name:"kraken_cmd_queue" 1000

  (** Queue command for Kraken processing *)
  let handle_order _cfg _exec_buffer cmd =
    let queue_start = Unix.gettimeofday () in
    Ringbuffer.push kraken_cmd_queue cmd >>= fun () ->
    let queue_duration = Unix.gettimeofday () -. queue_start in
    Lwt.async (fun () ->
      Telemetry.record_timer ["router"; "kraken"] "queue_push_duration" queue_duration
      (* Removed queue depth gauge - non-duration metric *)
    );
    Lwt.return_unit

  let rec process_kraken_commands cfg exec_buffer () =
    let queue_pop_start = Unix.gettimeofday () in
    Ringbuffer.pop kraken_cmd_queue >>= fun cmd ->
    let queue_pop_duration = Unix.gettimeofday () -. queue_pop_start in
    let cmd_processing_start = Unix.gettimeofday () in
    
    (* Log command details before routing *)
    let logging_start = Unix.gettimeofday () in
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
    let logging_duration = Unix.gettimeofday () -. logging_start in
    
    let kraken_api_start = Unix.gettimeofday () in
    Kraken.Kraken_outgoing_data.handle_router_command cfg cmd exec_buffer >>= fun () ->
    let kraken_api_duration = Unix.gettimeofday () -. kraken_api_start in
    let total_cmd_duration = Unix.gettimeofday () -. cmd_processing_start in
    
    (* Record comprehensive Kraken handler timing *)
    Lwt.async (fun () ->
      Telemetry.record_timer ["router"; "kraken"] "queue_pop_duration" queue_pop_duration >>= fun () ->
      Telemetry.record_timer ["router"; "kraken"] "logging_duration" logging_duration >>= fun () ->
      Telemetry.record_timer ["router"; "kraken"] "api_call_duration" kraken_api_duration >>= fun () ->
      Telemetry.record_timer ["router"; "kraken"] "total_processing_duration" total_cmd_duration
      (* Removed queue_depth_after_processing gauge - non-duration metric *)
    );
    
    process_kraken_commands cfg exec_buffer ()
end

(** Initialize router with command and execution buffers *)
let start cfg ~cmd_buffer ~exec_buffer =
  Lwt.async (KrakenHandler.process_kraken_commands cfg exec_buffer);
  let rec cmd_loop () =
    let cache_cleanup_start = Unix.gettimeofday () in
    OrderCache.cleanup ();
    let cache_cleanup_duration = Unix.gettimeofday () -. cache_cleanup_start in
    
    let cmd_buffer_pop_start = Unix.gettimeofday () in
    Ringbuffer.pop cmd_buffer >>= fun cmd ->
    let cmd_buffer_pop_duration = Unix.gettimeofday () -. cmd_buffer_pop_start in
    
    let total_processing_start = Unix.gettimeofday () in
    
    (* Record buffer and cache timing *)
    Lwt.async (fun () ->
      Telemetry.record_timer ["router"] "cmd_buffer_pop_duration" cmd_buffer_pop_duration >>= fun () ->
      Telemetry.record_timer ["router"] "cache_cleanup_duration" cache_cleanup_duration
    );
    
    let dedup_check_start = Unix.gettimeofday () in
    if OrderCache.is_duplicate cmd then (
      let dedup_check_duration = Unix.gettimeofday () -. dedup_check_start in
      let total_duration = Unix.gettimeofday () -. total_processing_start in
      Lwt.async (fun () ->
        Telemetry.record_timer ["router"] "duplicate_check_duration" dedup_check_duration >>= fun () ->
        Telemetry.record_timer ["router"] "duplicate_command_total_duration" total_duration
      );
      debug_f ~section:(Lwt_log_core.Section.make "engine.router")
        "Dropping duplicate order: %s"
          (match cmd with
          | Add { client_id; _ } -> client_id
          | Amend { order_id; _ } -> order_id
          | Cancel { order_id; _ } -> order_id) >>= fun () ->
      cmd_loop ()
    ) else (
      let dedup_check_duration = Unix.gettimeofday () -. dedup_check_start in
      
      (* Determine command type for detailed timing *)
      let cmd_type, cmd_str = match cmd with
      | Add { dst; symbol; side; price; qty; tags; _ } ->
          "add", Printf.sprintf "ADD order: %s %s %s @ %s qty=%s tags=[%s]"
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
          "amend", Printf.sprintf "AMEND order order_id=%s on %s: price=%s qty=%s"
            order_id
            dst
            (Primitives.Price.to_string new_price)
            (Primitives.Qty.to_string new_qty)
      | Cancel { dst; order_id } ->
          "cancel", Printf.sprintf "CANCEL order %s on %s" order_id dst
      in
      
      let logging_start = Unix.gettimeofday () in
      info_f ~section:(Lwt_log_core.Section.make "engine.router")
        "Routing command: %s" cmd_str >>= fun () ->
      let logging_duration = Unix.gettimeofday () -. logging_start in
      (match cmd with
      | Add { dst; _ } | Amend { dst; _ } | Cancel { dst; _ } ->
          let handle_cmd () =
            let routing_start = Unix.gettimeofday () in
            match dst with
            | "kraken" ->
                let kraken_handler_start = Unix.gettimeofday () in
                KrakenHandler.handle_order cfg exec_buffer cmd >>= fun () ->
                let kraken_handler_duration = Unix.gettimeofday () -. kraken_handler_start in
                let routing_duration = Unix.gettimeofday () -. routing_start in
                let total_duration = Unix.gettimeofday () -. total_processing_start in
                
                (* Record comprehensive timing metrics *)
                Lwt.async (fun () ->
                  Telemetry.record_timer ["router"] "duplicate_check_duration" dedup_check_duration >>= fun () ->
                  Telemetry.record_timer ["router"] "logging_duration" logging_duration >>= fun () ->
                  Telemetry.record_timer ["router"] "kraken_handler_duration" kraken_handler_duration >>= fun () ->
                  Telemetry.record_timer ["router"] "routing_duration" routing_duration >>= fun () ->
                  Telemetry.record_timer ["router"] "total_command_processing_duration" total_duration >>= fun () ->
                  (* Per-command-type timing breakdown *)
                  Telemetry.record_timer ["router"; cmd_type] "processing_duration" total_duration >>= fun () ->
                  Telemetry.record_timer ["router"; cmd_type] "kraken_handler_duration" kraken_handler_duration
                );
                Lwt.return_unit
            | _ ->
                let total_duration = Unix.gettimeofday () -. total_processing_start in
                Lwt.async (fun () ->
                  Telemetry.record_timer ["router"] "unknown_exchange_duration" total_duration >>= fun () ->
                  Telemetry.record_gauge ["router"] "unknown_exchange_count" 1.0
                );
                error_f ~section:(Lwt_log_core.Section.make "engine.router")
                  "Unknown exchange: %s" dst >>= fun () ->
                Lwt.return_unit
          in
          Lwt.async (fun () ->
            Lwt.catch handle_cmd (fun ex ->
              let error_duration = Unix.gettimeofday () -. total_processing_start in
              Lwt.async (fun () ->
                Telemetry.record_timer ["router"] "command_error_duration" error_duration >>= fun () ->
                Telemetry.record_gauge ["router"] "command_error_count" 1.0
              );
              error_f ~section:(Lwt_log_core.Section.make "engine.router")
                "Unhandled exception in command handler: %s" (Printexc.to_string ex) >>= fun () ->
              Lwt.return_unit
            )
          );
          cmd_loop ()
      )
    ) in
  cmd_loop ()