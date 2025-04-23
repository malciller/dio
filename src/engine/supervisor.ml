(* src/engine/supervisor.ml *)
let start ~feed:start_feed ~strategy:start_strategy ~router:start_router cfg =
  (* Coordinator: launch the three main fibers and wait for them *)
  let feed_fut   = start_feed cfg in
  let strat_fut  = start_strategy cfg in
  let router_fut = start_router cfg in
  Lwt.join [feed_fut; strat_fut; router_fut]
