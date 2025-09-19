(* src/engine/supervisor.ml *)
open Lwt.Infix 
open Dio_types 


(* restart a fiber on failure, with logging and delay *)
let supervise name fiber_fun =
  let section = Lwt_log_core.Section.make ("engine.supervisor." ^ name) in
  let rec loop () =
    Lwt.catch
      (fun () ->
        Lwt_log_core.info ~section ("Starting component: " ^ name) >>= fun () ->
        fiber_fun () >>= fun () ->
        Lwt_log_core.warning ~section ("Component exited normally: " ^ name) >>= fun () ->
        Lwt_unix.sleep 1.0 >>= loop 
      )
      (fun exn ->
        Lwt_log_core.error ~section
          (Printf.sprintf "Component %s failed: %s. Restarting in 5s..." name (Printexc.to_string exn)) >>= fun () ->
        Lwt_unix.sleep 5.0 >>= loop
      )
  in
  loop ()

let start ~(feed_initializer_fn : unit -> unit Lwt.t)
    ~(grid_strategy : Core.grid_strategy)
    ~(orderbook_strategy : Core.orderbook_strategy)
    ~(arbitrage_strategy : Core.arbitrage_strategy)
    ~(router : Core.router)
    ~(tick_buffer: Event.tick Ringbuffer.t)
    ~(exec_buffer: Core.market_event Ringbuffer.t)
    ~(cmd_buffer: Core.order_cmd Ringbuffer.t)
    (runtime_cfg : Config.runtime_cfg)
    (core_cfg : Config.engine_config) =
  (* launch the five main fibers and supervise them *)
  let feed_fut = supervise "feed" feed_initializer_fn in
  let grid_strat_fut = supervise "grid_strategy" (fun () -> grid_strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in
  let orderbook_strat_fut = supervise "orderbook_strategy" (fun () -> orderbook_strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in
  let arbitrage_strat_fut = supervise "arbitrage_strategy" (fun () -> arbitrage_strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in
  let router_fut = supervise "router" (fun () -> router.start core_cfg ~cmd_buffer ~exec_buffer) in
  let balance_fetcher _ = Kraken.Kraken_balances.wait_for_balances () in
  let discord_webhook_fut = supervise "discord_webhook" (fun () -> Discord_webhook.start balance_fetcher core_cfg) in

  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.supervisor") "Starting all components under supervision..." >>= fun () ->
  Lwt.join [feed_fut; grid_strat_fut; orderbook_strat_fut; arbitrage_strat_fut; router_fut; discord_webhook_fut]
