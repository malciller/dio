(* Remove warning suppression for Types_engine *)
(* [@@@warning "-33"] *) 

(* src/engine/router.ml *)
open Lwt.Infix  (* for >>= *)
open Types.Core (* Use Types.Core for config, events, etc. *)



let start _cfg ~cmd_buffer ~exec_buffer =
  (* TODO: Dispatch commands from cmd_buffer to appropriate exchange handler *)
  let rec cmd_loop () =
    match Types.Ringbuffer.pop_opt cmd_buffer with
    | Some cmd ->
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.router") 
          (Printf.sprintf "Routing command: %s" (Yojson.Safe.to_string (order_cmd_to_yojson cmd)))
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
