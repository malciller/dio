(* src/engine/supervisor.ml *)
open Lwt.Infix  (* for >>= *)
open Types_engine (* Contains config, strategy, router types *)
open Feed (* For config *)
open Types
open Types.Core

(* The function signature now expects types from Types_engine.ml and buffers *)
let start ~feed 
    ~(strategy : strategy) 
    ~(router : router) 
    ~(tick_buffer: Event.tick Ringbuffer.t) 
    ~(exec_buffer: market_event Ringbuffer.t) 
    ~(cmd_buffer: order_cmd Ringbuffer.t) 
    (cfg : config) =
  (* Coordinator: launch the three main fibers and wait for them *)
  let feed_fut = feed in
  let strat_fut = strategy.start cfg ~tick_buffer ~cmd_buffer in (* Pass buffers *)
  let router_fut = router.start cfg ~cmd_buffer ~exec_buffer in (* Pass buffers *)
  
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
