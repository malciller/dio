(* src/engine/supervisor.ml *)
open Lwt.Infix  (* for >>= *)
open Types (* For Event.tick *)

(* Supervision helper: restart a fiber on failure, with logging and delay *)
let supervise name fiber_fun =
  let section = Lwt_log_core.Section.make ("engine.supervisor." ^ name) in
  let rec loop () =
    Lwt.catch
      (fun () ->
        Lwt_log_core.info ~section ("Starting component: " ^ name) >>= fun () ->
        fiber_fun () >>= fun () ->
        Lwt_log_core.warning ~section ("Component exited normally: " ^ name) >>= fun () ->
        Lwt_unix.sleep 1.0 >>= loop  (* Restart even on normal exit *)
      )
      (fun exn ->
        Lwt_log_core.error ~section
          (Printf.sprintf "Component %s failed: %s. Restarting in 5s..." name (Printexc.to_string exn)) >>= fun () ->
        Lwt_unix.sleep 5.0 >>= loop
      )
  in
  loop ()

(* Updated signature to accept both runtime_cfg and core_cfg *)
let start ~feed 
    ~(strategy : Core.strategy) 
    ~(router : Core.router) 
    ~(tick_buffer: Event.tick Ringbuffer.t) 
    ~(exec_buffer: Core.market_event Ringbuffer.t) 
    ~(cmd_buffer: Core.order_cmd Ringbuffer.t) 
    (runtime_cfg : Config.runtime_cfg) (* Add runtime_cfg *)
    (core_cfg : Core.config) =        (* Keep core_cfg *)
  (* Coordinator: launch the three main fibers and supervise them *)
  let feed_fut = supervise "feed" (fun () -> feed) in
  let strat_fut = supervise "strategy" (fun () -> strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in 
  let router_fut = supervise "router" (fun () -> router.start core_cfg ~cmd_buffer ~exec_buffer) in

  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.supervisor") "Starting all components under supervision..." >>= fun () ->
  Lwt.join [feed_fut; strat_fut; router_fut]
