(* Remove warning suppression for Types_engine *)
(* [@@@warning "-33"] *) 

(* src/engine/router.ml *)
open Lwt.Infix  (* for >>= *)
open Types.Core (* Use Types.Core for config, events, etc. *)
open Types.Primitives (* For Price and Qty *)

(* Response type for order operations *)
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
    | Add { dst; symbol; side; _ } ->
        Printf.sprintf "add:%s:%s:%s"
          dst symbol 
          (match side with Buy -> "buy" | Sell -> "sell")
    | Amend { dst; order_id; _ } ->
        Printf.sprintf "amend:%s:%s" 
          dst order_id
    | Cancel { dst; order_id } ->
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
module Kraken = struct
  let handle_order cfg exec_buffer cmd =
    match cmd with
    | Add { symbol; side; price; qty; client_id; _ } ->
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.router.kraken")
          (Printf.sprintf "Sending ADD order to Kraken: %s %s @ %s qty=%s (client_id=%s)"
            (match side with Buy -> "BUY" | Sell -> "SELL")
            symbol
            (Price.to_string price)
            (Qty.to_string qty)
            client_id) >>= fun () ->
        Kraken.Ws_exec.handle_router_command cfg cmd exec_buffer
    | Amend { dst=_; order_id; symbol; new_price; new_qty; ts=_ } ->
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.router.kraken")
          (Printf.sprintf "Sending AMEND order to Kraken: %s (%s) price=%s qty=%s"
            order_id
            symbol
            (Price.to_string new_price)
            (Qty.to_string new_qty)) >>= fun () ->
        Kraken.Ws_exec.handle_router_command cfg cmd exec_buffer
    | Cancel { order_id; _ } ->
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.router.kraken")
          (Printf.sprintf "Sending CANCEL order to Kraken: %s" order_id) >>= fun () ->
        Kraken.Ws_exec.handle_router_command cfg cmd exec_buffer
end

let start cfg ~cmd_buffer ~exec_buffer =
  (* Process commands from cmd_buffer and route to appropriate exchange handler *)
  let rec cmd_loop () =
    (* Periodically clean up expired cache entries *)
    OrderCache.cleanup ();

    match Types.Ringbuffer.pop_opt cmd_buffer with
    | Some cmd ->
        (* Check for duplicates *)
        if OrderCache.is_duplicate cmd then (
          Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.router")
            (Printf.sprintf "Dropping duplicate order: %s"
              (match cmd with
              | Add { client_id; _ } -> client_id
              | Amend { order_id; _ } -> order_id
              | Cancel { order_id; _ } -> order_id)) >>= fun () ->
          cmd_loop ()
        ) else (
          (* Enhanced logging for commands *)
          let cmd_str = match cmd with
          | Add { dst; symbol; side; price; qty; tags; _ } ->
              Printf.sprintf "ADD order: %s %s %s @ %s qty=%s tags=[%s]"
                (match side with Buy -> "BUY" | Sell -> "SELL")
                symbol
                dst
                (Price.to_string price)
                (Qty.to_string qty)
                (String.concat ";" (List.map (function 
                  | `Grid -> "grid" 
                  | `Manual -> "manual" 
                  | `Rebalance -> "rebalance") tags))
          | Amend { dst; order_id; new_price; new_qty; _ } ->
              Printf.sprintf "AMEND order order_id=%s on %s: price=%s qty=%s"
                order_id
                dst
                (Price.to_string new_price)
                (Qty.to_string new_qty)
          | Cancel { dst; order_id } ->
              Printf.sprintf "CANCEL order %s on %s" order_id dst
          in
          Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.router") 
            (Printf.sprintf "Routing command: %s" cmd_str) >>= fun () -> 

          (* Route command to appropriate exchange *)
          (match cmd with
          | Add { dst; _ } | Amend { dst; _ } | Cancel { dst; _ } ->
              match dst with
              | "kraken" ->
                  (* Call handle_order (which calls handle_router_command)
                     and expect unit Lwt.t *)
                  Kraken.handle_order cfg exec_buffer cmd
              | _ ->
                  Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.router")
                    (Printf.sprintf "Unknown exchange: %s" dst) >>= fun () ->
                  (* No response to return here, just log *)
                  Lwt.return_unit
          ) >>= fun () -> (* Now expects unit Lwt.t *)
          
          cmd_loop ()
        )
    | None -> 
        Lwt_unix.sleep 0.01 >>= cmd_loop (* Sleep briefly if buffer is empty *)
  in
  
  cmd_loop () (* Run the command processing loop *)
