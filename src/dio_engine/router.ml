(* src/engine/router.ml *)
open Lwt.Infix  
open Dio_types
open Lwt_log_core

(* order operations *)
type order_response = {
  success: bool;
  error: string option;
  result: Yojson.Safe.t option;
}

(* Order deduplication state *)
module OrderCache = struct
  (* Cache recent orders to prevent duplicates *)
  let recent_orders = Hashtbl.create 1024
  let cache_timeout = 10.0 (* seconds *)

  (* Create a unique key for each order *)
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

  (* Check if an order is a duplicate *)
  let is_duplicate cmd =
    let key = make_order_key cmd in
    match Hashtbl.find_opt recent_orders key with
    | Some timestamp ->
        let now = Unix.gettimeofday () in
        if now -. timestamp > cache_timeout then (
          (* Entry expired, not a duplicate *)
          Hashtbl.replace recent_orders key now;
          false
        ) else true
    | None ->
        (* New order, not a duplicate *)
        Hashtbl.add recent_orders key (Unix.gettimeofday ());
        false

  (* Clean up expired entries *)
  let cleanup () =
    let now = Unix.gettimeofday () in
    Hashtbl.filter_map_inplace
      (fun _ timestamp -> 
        if now -. timestamp > cache_timeout then None 
        else Some timestamp)
      recent_orders
end

(* Exchange-specific handlers *)
module KrakenHandler = struct
   (* The queue for Kraken commands *)
  let kraken_cmd_queue = Ringbuffer.create 1000
 
  let handle_order _cfg _exec_buffer cmd =
    Ringbuffer.push kraken_cmd_queue cmd
 
  let rec process_kraken_commands cfg exec_buffer () =
    Ringbuffer.pop kraken_cmd_queue >>= fun cmd ->
    (* Re-introduce Kraken-specific logging before sending the command *)
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

let start cfg ~cmd_buffer ~exec_buffer =
  (* Start the Kraken command processing loop as an Lwt thread *)
  Lwt.async (KrakenHandler.process_kraken_commands cfg exec_buffer);
  (* Process commands from cmd_buffer and route to appropriate exchange handler *)
  let rec cmd_loop () =
    (* Periodically clean up expired cache entries *)
    OrderCache.cleanup ();

    (* Asynchronously wait for a command from the buffer *)
    Ringbuffer.pop cmd_buffer >>= fun cmd ->

    (* Check for duplicates *)
    if OrderCache.is_duplicate cmd then (
      warning_f ~section:(Lwt_log_core.Section.make "engine.router")
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

      (* Route command to appropriate exchange *)
      (match cmd with
      | Add { dst; _ } | Amend { dst; _ } | Cancel { dst; _ } ->
          let handle_cmd () =
            match dst with
            | "kraken" ->
                KrakenHandler.handle_order cfg exec_buffer cmd >>= fun () ->
                Lwt.return_unit
            | _ ->
                error_f ~section:(Lwt_log_core.Section.make "engine.router")
                  "Unknown exchange: %s" dst >>= fun () ->
                Lwt.return_unit
          in
          Lwt.async (fun () ->
            Lwt.catch handle_cmd (fun ex ->
              error_f ~section:(Lwt_log_core.Section.make "engine.router")
                "Unhandled exception in command handler: %s" (Printexc.to_string ex) >>= fun () ->
              Lwt.return_unit
            )
          );
          cmd_loop ()
      )
    )
  in
  cmd_loop ()