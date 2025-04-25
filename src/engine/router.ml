(* Remove warning suppression for Types_engine *)
(* [@@@warning "-33"] *) 

(* src/engine/router.ml *)
open Lwt.Infix  (* for >>= *)
open Types.Core (* Use Types.Core for config, events, etc. *)
open Types.Primitives (* For Price and Qty *)

let start _cfg ~cmd_buffer ~exec_buffer =
  (* TODO: Dispatch commands from cmd_buffer to appropriate exchange handler *)
  let rec cmd_loop () =
    match Types.Ringbuffer.pop_opt cmd_buffer with
    | Some cmd ->
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
        | Amend { dst; order_id; new_price; new_qty } ->
            Printf.sprintf "AMEND order %s on %s: price=%s qty=%s"
              order_id
              dst
              (match new_price with Some p -> Price.to_string p | None -> "unchanged")
              (match new_qty with Some q -> Qty.to_string q | None -> "unchanged")
        | Cancel { dst; order_id } ->
            Printf.sprintf "CANCEL order %s on %s" order_id dst
        in
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.router") 
          (Printf.sprintf "Routing command: %s" cmd_str)
        >>= fun () -> 
        (* TODO: dispatch cmd to exchange exec handler based on cmd.dst *) 
        cmd_loop ()
    | None -> 
        Lwt_unix.sleep 0.01 >>= cmd_loop (* Sleep briefly if buffer is empty *)
  in

  (* TODO: Optionally process execution reports from exec_buffer *)
  let rec exec_loop () =
    match Types.Ringbuffer.pop_opt exec_buffer with
    | Some event ->
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.router")
          (Printf.sprintf "Router received exec event: %s" (Yojson.Safe.to_string (market_event_to_yojson event)))
        >>= fun () -> 
        (* TODO: Handle execution event (e.g., update internal state) *) 
        exec_loop ()
    | None -> 
        Lwt_unix.sleep 0.01 >>= exec_loop (* Sleep briefly if buffer is empty *)
  in
  
  Lwt.join [cmd_loop () ; exec_loop ()] (* Run both loops concurrently *)
