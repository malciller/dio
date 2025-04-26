(* src/engine/supervisor.ml *)
open Lwt.Infix  (* for >>= *)
open Types (* For Event.tick *)
open Types.Core (* Contains config, strategy, router, order_cmd etc. *)
open Types.Config (* For runtime_cfg *)



(* Updated signature to accept both runtime_cfg and core_cfg *)
let start ~feed 
    ~(strategy : strategy) 
    ~(router : router) 
    ~(tick_buffer: Event.tick Ringbuffer.t) 
    ~(exec_buffer: market_event Ringbuffer.t) 
    ~(cmd_buffer: order_cmd Ringbuffer.t) 
    (runtime_cfg : runtime_cfg) (* Add runtime_cfg *)
    (core_cfg : config) =        (* Keep core_cfg *)
  (* Coordinator: launch the three main fibers and wait for them *)
  let feed_fut = feed in
  (* Strategy needs both configs - signature update in Types.Core required *)
  let strat_fut = strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer in 
  (* Router likely only needs core_cfg *)
  let router_fut = router.start core_cfg ~cmd_buffer ~exec_buffer in (* Pass core_cfg *)
  
  (* Log startup of components *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.supervisor") "Starting all components..." >>= fun () ->
  
  (* Run all components concurrently and handle any errors *)
  Lwt.catch
    (fun () -> Lwt.join [feed_fut; strat_fut; router_fut])
    (fun exn ->
      Lwt_log_core.error ~section:(Lwt_log_core.Section.make "engine.supervisor")
        (Printf.sprintf "Error in supervisor: %s" (Printexc.to_string exn)) >>= fun () ->
      Lwt.fail exn
    )
