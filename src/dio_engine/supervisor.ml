(** Supervisor module for managing concurrent trading engine components.

    Provides fault-tolerant supervision of long-running fibers with automatic
    restart on failure, logging, and configurable delays. *)

open Lwt.Infix
open Dio_types
open Telemetry


(** Supervises a fiber with automatic restart on failure.

    Logs component lifecycle events and implements exponential backoff
    for failed components. Normal exits restart after 1s, failures after 5s. *)
let supervise name fiber_fun =
  let section = Lwt_log_core.Section.make ("engine.supervisor." ^ name) in
  let restart_count = ref 0 in

  let rec loop () =
    Lwt.catch
      (fun () ->
        Lwt_log_core.info ~section ("Starting component: " ^ name) >>= fun () ->
        fiber_fun () >>= fun () ->
        (* Component exited normally - only count meaningful failures, not exits *)
        Lwt_log_core.warning ~section ("Component exited normally: " ^ name) >>= fun () ->
        Lwt_unix.sleep 1.0 >>= loop
      )
      (fun exn ->
        (* Component failed *)
        incr restart_count;
        Lwt.async (fun () ->
          record_counter ["system"; "supervisor"] (name ^ "_failures") 1 >>= fun () ->
          record_gauge ["system"; "supervisor"] (name ^ "_restart_count") (Float.of_int !restart_count)
        );
        Lwt_log_core.error ~section
          (Printf.sprintf "Component %s failed: %s. Restarting in 5s..." name (Printexc.to_string exn)) >>= fun () ->
        Lwt_unix.sleep 5.0 >>= loop
      )
  in
  loop ()

(** Launches and supervises all core trading engine components.

    Starts the feed, three trading strategies, router, and Discord webhook
    under individual supervision. All components share access to tick,
    execution, and command buffers for inter-component communication.

    @param feed_initializer_fn Function to initialize market data feed
    @param grid_strategy Grid trading strategy implementation
    @param orderbook_strategy Orderbook-based trading strategy
    @param arbitrage_strategy Arbitrage trading strategy
    @param router Order routing and execution component
    @param tick_buffer Shared buffer for market tick data
    @param exec_buffer Shared buffer for market execution events
    @param cmd_buffer Shared buffer for order commands
    @param runtime_cfg Runtime configuration parameters
    @param core_cfg Engine configuration parameters
    @return Lwt promise that resolves when all components terminate *)
let start ~(feed_initializer_fn : unit -> unit Lwt.t)
    ~(grid_strategy : Core.grid_strategy)
    ~(orderbook_strategy : Core.orderbook_strategy)
    ~(vmm_strategy : Core.vmm_strategy)
    ~(arbitrage_strategy : Core.arbitrage_strategy)
    ~(router : Core.router)
    ~(tick_buffer: Event.tick Ringbuffer.t)
    ~(exec_buffer: Core.market_event Ringbuffer.t)
    ~(cmd_buffer: Core.order_cmd Ringbuffer.t)
    (runtime_cfg : Config.runtime_cfg)
    (core_cfg : Config.engine_config) =
  
    let feed_fut = supervise "feed" feed_initializer_fn in
  
  let grid_strat_fut = supervise "grid_strategy" (fun () -> grid_strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in
  
  let orderbook_strat_fut = supervise "orderbook_strategy" (fun () -> orderbook_strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in
  
  let vmm_strat_fut = supervise "vmm_strategy" (fun () -> vmm_strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in
  
  let arbitrage_strat_fut = supervise "arbitrage_strategy" (fun () -> arbitrage_strategy.start runtime_cfg core_cfg ~tick_buffer ~cmd_buffer ~exec_buffer) in
  
  let router_fut = supervise "router" (fun () -> router.start core_cfg ~cmd_buffer ~exec_buffer) in
  
  let fee_cache_symbols =
    core_cfg.Config.symbols
    |> List.map String.uppercase_ascii
    |> List.sort_uniq String.compare
  in
  let fee_cache_fut =
    supervise "fee_cache" (fun () ->
      let rec loop () =
        (match fee_cache_symbols with
        | [] -> Lwt.return_unit
        | symbols ->
            Lwt_log_core.debug
              ~section:(Lwt_log_core.Section.make "engine.supervisor.fee_cache")
              (Printf.sprintf "Refreshing Kraken fee cache for %d symbol(s)" (List.length symbols))
            >>= fun () ->
            Kraken.Kraken_fee_cache.ensure_pairs core_cfg symbols)
        >>= fun () ->
        Lwt_unix.sleep 900.0 >>= loop
      in
      loop ()
    )
  in
  (** Fetches and aggregates all account balances from Kraken.

      Combines spot, earn, and liquid staking balances into a single
      hash table for reporting purposes. *)
  let balance_fetcher _ =
    Kraken.Kraken_balances.wait_for_balances () >|= fun (spot_balances, earn_balances, liquid_balances, _) ->
    let all_balances = Hashtbl.create 32 in
    Hashtbl.iter (fun asset balance -> Hashtbl.replace all_balances asset balance) spot_balances;
    Hashtbl.iter (fun asset balance ->
      let current = Hashtbl.find_opt all_balances asset |> Option.value ~default:0.0 in
      Hashtbl.replace all_balances asset (current +. balance)
    ) earn_balances;
    Hashtbl.iter (fun asset balance ->
      let current = Hashtbl.find_opt all_balances asset |> Option.value ~default:0.0 in
      Hashtbl.replace all_balances asset (current +. balance)
    ) liquid_balances;
    all_balances
  in
  let discord_webhook_fut = supervise "discord_webhook" (fun () -> Discord_webhook.start balance_fetcher core_cfg) in

  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.supervisor") "Starting all components under supervision..." >>= fun () ->
  Lwt.join [feed_fut; grid_strat_fut; orderbook_strat_fut; vmm_strat_fut; arbitrage_strat_fut; router_fut; discord_webhook_fut; fee_cache_fut]
