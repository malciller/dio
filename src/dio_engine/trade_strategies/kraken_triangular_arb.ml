(* src/dio_engine/trade_strategies/kraken_triangular_arb.ml *)

open Lwt.Infix
open Dio_types
open Lwt_log_core

let section = Section.make "kraken_triangular_arb"

(* Configuration constants *)
let profit_threshold_pct = 0.001  (* 0.1% minimum profit *)
let max_cycle_length = 4         (* Maximum arbitrage cycle length *)
let taker_fee_pct = 0.0035       (* 0.35% taker fee for most pairs *)
let maker_fee_pct = 0.0020       (* 0.20% maker fee for most pairs *)
let stable_fee_pct = 0.0020      (* 0.20% fee for USD stablecoins *)

(* Graph edge representing exchange rate with fees *)
type exchange_edge = {
  from_asset: string;
  to_asset: string;
  rate: float;              (* Exchange rate (to_asset per from_asset) *)
  fee_rate: float;          (* Trading fee rate *)
  capacity: float;          (* Maximum tradeable volume in from_asset *)
  pair: string;             (* Trading pair symbol *)
}

(* Arbitrage cycle information *)
type arbitrage_cycle = {
  path: string list;        (* Asset path: [A, B, C, A] *)
  profit_pct: float;        (* Expected profit percentage *)
  trade_sizes: float list;  (* Trade sizes for each leg *)
  bottleneck_volume: float; (* Limiting volume in base asset *)
}

(* Graph node for Bellman-Ford *)
type graph_node = {
  asset: string;
  edges: exchange_edge list;
}

(* Bellman-Ford state *)
type bf_state = {
  distances: (string, float) Hashtbl.t;
  predecessors: (string, string) Hashtbl.t;
  negative_cycles: arbitrage_cycle list;
}

(* Extract base and quote assets from trading pair *)
let extract_assets pair_symbol =
  let pair = String.uppercase_ascii pair_symbol in
  (* Handle common Kraken asset codes *)
  let asset_map = [
    ("XXBT", "ZUSD"); ("XETH", "ZUSD"); ("SOL", "ZUSD");
    ("ADA", "ZUSD"); ("TRX", "ZUSD"); ("USDG", "ZUSD");
    ("USDR", "ZUSD"); ("USDT", "ZUSD"); ("USDC", "ZUSD")
  ] in

  let rec find_asset_code = function
    | [] -> (pair, "ZUSD") (* fallback *)
    | (base, quote)::rest ->
        if String.starts_with ~prefix:base pair &&
           String.ends_with ~suffix:quote pair then
          (base, quote)
        else
          find_asset_code rest
  in
  find_asset_code asset_map

(* Get appropriate fee rate for a trading pair *)
let get_fee_rate pair_symbol =
  let pair = String.uppercase_ascii pair_symbol in
  if String.contains pair 'G' && String.contains pair 'U' && String.contains pair 'D' ||
     String.contains pair 'R' && String.contains pair 'U' && String.contains pair 'D' then
    stable_fee_pct  (* USDG/USDR pairs have low fees *)
  else
    taker_fee_pct   (* Standard taker fee for crypto pairs *)

(* Build exchange graph from orderbook data *)
let build_exchange_graph symbols : (string, graph_node) Hashtbl.t Lwt.t =
  let graph = Hashtbl.create 32 in

  Lwt_list.iter_s (fun symbol ->
    match Kraken.Kraken_orderbook.get_orderbook symbol with
    | None ->
        warning_f ~section "No orderbook data for %s" symbol >>= fun () ->
        Lwt.return_unit
    | Some orderbook ->
        (* Extract base and quote assets *)
        let base_asset, quote_asset = extract_assets symbol in

        (* Get top bid/ask prices *)
        match orderbook.bids, orderbook.asks with
        | top_bid :: _, top_ask :: _ ->
            let bid_price = top_bid.price in
            let ask_price = top_ask.price in
            let fee_rate = get_fee_rate symbol in

            (* Create edges: base->quote and quote->base *)
            let base_to_quote_edge = {
              from_asset = base_asset;
              to_asset = quote_asset;
              rate = ask_price;  (* Sell base for quote *)
              fee_rate;
              capacity = top_ask.qty;
              pair = symbol;
            } in

            let quote_to_base_edge = {
              from_asset = quote_asset;
              to_asset = base_asset;
              rate = 1.0 /. bid_price;  (* Buy base with quote *)
              fee_rate;
              capacity = top_bid.qty *. bid_price; (* Capacity in quote asset *)
              pair = symbol;
            } in

            (* Add to graph nodes *)
            let add_edge_to_node asset edge =
              let node = match Hashtbl.find_opt graph asset with
                | None -> { asset; edges = [] }
                | Some n -> n
              in
              Hashtbl.replace graph asset { node with edges = edge :: node.edges }
            in

            add_edge_to_node base_asset base_to_quote_edge;
            add_edge_to_node quote_asset quote_to_base_edge;

            info_f ~section "Added edges for %s: %s->%s@%.8f, %s->%s@%.8f"
              symbol base_asset quote_asset ask_price quote_asset base_asset (1.0 /. bid_price)
        | _ ->
            warning_f ~section "No bid/ask data for %s" symbol
  ) symbols >>= fun () ->

  info_f ~section "Built exchange graph with %d nodes" (Hashtbl.length graph) >>= fun () ->
  Lwt.return graph

(* Calculate effective exchange rate after fees *)
let effective_rate edge =
  edge.rate *. (1.0 -. edge.fee_rate)

(* Initialize Bellman-Ford state *)
let init_bf_state assets =
  let distances = Hashtbl.create (List.length assets) in
  let predecessors = Hashtbl.create (List.length assets) in

  (* Set distance to 0 for all assets (log of 1.0) *)
  List.iter (fun asset ->
    Hashtbl.replace distances asset 0.0;
    Hashtbl.replace predecessors asset ""
  ) assets;

  { distances; predecessors; negative_cycles = [] }

(* Bellman-Ford relaxation step *)
let relax_edge bf_state edge =
  let current_dist = Hashtbl.find_opt bf_state.distances edge.from_asset |> Option.value ~default:0.0 in
  let neighbor_dist = Hashtbl.find_opt bf_state.distances edge.to_asset |> Option.value ~default:0.0 in
  let edge_weight = -. (Float.log (effective_rate edge)) in  (* Negative log for arbitrage *)

  if current_dist +. edge_weight < neighbor_dist then (
    Hashtbl.replace bf_state.distances edge.to_asset (current_dist +. edge_weight);
    Hashtbl.replace bf_state.predecessors edge.to_asset edge.from_asset;
    true  (* Relaxation occurred *)
  ) else
    false

(* Extract cycle from predecessors *)
let extract_cycle predecessors start_asset =
  let rec build_path current path visited =
    if Hashtbl.mem visited current then
      (* Found cycle, extract it *)
  let rec find_cycle_start lst idx =
    match lst with
    | [] -> None
    | hd :: _ when hd = current -> Some idx
    | _ :: tl -> find_cycle_start tl (idx + 1)
      in
      match find_cycle_start path 0 with
      | Some idx ->
          let rec drop n lst = match n, lst with
            | 0, lst -> lst
            | n, _::tl when n > 0 -> drop (n-1) tl
            | _ -> []
          in
          let cycle = drop idx path in
          cycle @ [current]
      | None -> []
    else (
      Hashtbl.replace visited current true;
      match Hashtbl.find_opt predecessors current with
      | Some pred when pred <> "" -> build_path pred (current :: path) visited
      | _ -> []
    )
  in
  let visited = Hashtbl.create 16 in
  build_path start_asset [] visited

(* Detect negative cycles using Bellman-Ford algorithm *)
let detect_arbitrage_cycles graph : arbitrage_cycle list Lwt.t =
  let assets = Hashtbl.fold (fun asset _ acc -> asset :: acc) graph [] in
  let bf_state = init_bf_state assets in
  let all_edges = Hashtbl.fold (fun _ node acc ->
    node.edges @ acc
  ) graph [] in

  debug_f ~section "Running Bellman-Ford with %d assets and %d edges"
    (List.length assets) (List.length all_edges) >>= fun () ->

  (* Relax all edges |V|-1 times *)
  for _ = 1 to List.length assets - 1 do
    List.iter (fun edge -> ignore (relax_edge bf_state edge)) all_edges
  done;

  (* Check for negative cycles in Nth iteration *)
  let cycles = ref [] in
  List.iter (fun edge ->
    let current_dist = Hashtbl.find_opt bf_state.distances edge.from_asset |> Option.value ~default:0.0 in
    let neighbor_dist = Hashtbl.find_opt bf_state.distances edge.to_asset |> Option.value ~default:0.0 in
    let edge_weight = -. (Float.log (effective_rate edge)) in

    if current_dist +. edge_weight < neighbor_dist then (
      (* Found negative cycle *)
      let cycle_path = extract_cycle bf_state.predecessors edge.to_asset in
      if cycle_path <> [] && List.length cycle_path >= 3 then (
        let profit_pct = Float.exp (-. (List.fold_left (fun acc asset ->
          match Hashtbl.find_opt bf_state.distances asset with
          | Some dist -> acc +. dist
          | None -> acc
        ) 0.0 cycle_path)) -. 1.0 in

        if profit_pct > profit_threshold_pct then (
          let cycle = {
            path = cycle_path @ [List.hd cycle_path];  (* Close the cycle *)
            profit_pct;
            trade_sizes = [];  (* Will be calculated later *)
            bottleneck_volume = 0.0;  (* Will be calculated later *)
          } in
          cycles := cycle :: !cycles
        )
      )
    )
  ) all_edges;

  info_f ~section "Detected %d profitable arbitrage cycles" (List.length !cycles) >>= fun () ->
  Lwt.return !cycles

(* Calculate optimal trade sizes for a cycle *)
let calculate_trade_sizes cycle graph =
  let path = cycle.path in
  if List.length path < 4 then cycle else  (* Need at least 3 edges + closing *)

  (* Find the bottleneck capacity along the cycle, normalized to the start asset *)
  let rec find_bottleneck remaining_path min_capacity current_rate =
    match remaining_path with
    | [_] -> min_capacity
    | current :: next :: rest ->
        (match Hashtbl.find_opt graph current with
         | Some node ->
             (match List.find_opt (fun edge -> edge.to_asset = next) node.edges with
              | Some edge ->
                  let normalized_capacity = edge.capacity *. current_rate in
                  let next_rate = current_rate *. effective_rate edge in
                  find_bottleneck (next :: rest) (min min_capacity normalized_capacity) next_rate
              | None -> min_capacity)
         | None -> min_capacity)
    | [] -> min_capacity
  in

  let bottleneck = find_bottleneck path max_float 1.0 in
  let trade_size = min bottleneck 1.0 in  (* Limit to 1 unit for safety *)

  { cycle with
    trade_sizes = List.init (List.length path - 1) (fun _ -> trade_size);
    bottleneck_volume = bottleneck }

(* Validate cycle for execution (balances, min sizes, etc.) *)
let validate_cycle cycle graph =
  (* Check if all required edges exist and have sufficient capacity *)
  let path = cycle.path in
  let rec check_path remaining_path =
    match remaining_path with
    | [_] -> true  (* End of cycle *)
    | current :: next :: rest ->
        (match Hashtbl.find_opt graph current with
         | Some node ->
             List.exists (fun edge ->
               edge.to_asset = next &&
               edge.capacity >= cycle.bottleneck_volume
             ) node.edges
         | None -> false) && check_path (next :: rest)
    | [] -> true
  in

  check_path path &&
  cycle.bottleneck_volume > 0.001  (* Minimum trade size *)

(* Create orders for executing an arbitrage cycle *)
let create_arbitrage_orders cycle graph cmd_buffer =
  let path = cycle.path in

  let rec create_orders remaining_path current_size =
    match remaining_path with
    | [_] -> Lwt.return_unit  (* Cycle complete *)
    | current :: next :: rest ->
        (match Hashtbl.find_opt graph current with
         | Some node ->
             (match List.find_opt (fun edge -> edge.to_asset = next) node.edges with
              | Some edge ->
                  (* Determine order side and quantity *)
                  let base_asset, quote_asset = extract_assets edge.pair in
                  let side, order_qty, next_size =
                    if edge.from_asset = quote_asset && edge.to_asset = base_asset then
                      (* Buy base with quote *)
                      let qty = current_size /. edge.rate in
                      (Core.Buy, qty, qty)
                    else if edge.from_asset = base_asset && edge.to_asset = quote_asset then
                      (* Sell base for quote *)
                      let next_size = current_size *. effective_rate edge in
                      (Core.Sell, current_size, next_size)
                    else
                      (* Should not happen with a well-formed graph *)
                      (Core.Buy, 0.0, 0.0)
                  in

                  let price_str = Printf.sprintf "%.8f" edge.rate in
                  let price = Primitives.Price.of_string_exn ~scale:8 price_str in
                  let qty_str = Printf.sprintf "%.8f" order_qty in
                  let qty = Primitives.Qty.of_string_exn ~scale:8 qty_str in

                  let order_cmd = Core.Add {
                    dst = "kraken";
                    client_id = Printf.sprintf "arb_%s_%d" edge.pair (Random.int 1000000);
                    symbol = edge.pair;
                    side;
                    price;
                    qty;
                    tif = Core.GTC;
                    tags = [`Manual];  (* Tag as arbitrage trade *)
                  } in

                  info_f ~section "Creating arbitrage order: %s %s %.8f@%s"
                    edge.pair
                    (match side with Buy -> "BUY" | Sell -> "SELL")
                    order_qty
                    price_str >>= fun () ->

                  Ringbuffer.push cmd_buffer order_cmd >>= fun () ->
                  create_orders (next :: rest) next_size
              | None ->
                  warning_f ~section "No edge found from %s to %s" current next >>= fun () ->
                  Lwt.return_unit)
         | None ->
             warning_f ~section "No node found for asset %s" current >>= fun () ->
             Lwt.return_unit)
    | [] -> Lwt.return_unit
  in

  create_orders path cycle.bottleneck_volume

(* Main triangular arbitrage strategy *)
let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config)
    ~tick_buffer:_ ~cmd_buffer ~exec_buffer:_ =

  info_f ~section "Starting triangular arbitrage strategy" >>= fun () ->

  (* Get all active trading symbols from config *)
  let active_symbols = List.map (fun (asset: Config.asset_cfg) -> asset.symbol) runtime_cfg.assets in

  info_f ~section "Monitoring symbols for arbitrage: [%s]"
    (String.concat ", " active_symbols) >>= fun () ->

  (* Wait for instrument data to be loaded before starting arbitrage *)
  Kraken.Kraken_incoming_data.wait_for_instruments () >>= fun () ->
  info_f ~section "Instrument data loaded, starting arbitrage detection" >>= fun () ->

  (* Main arbitrage detection loop *)
  let rec arbitrage_loop () =
    (* Build exchange graph from current orderbook data *)
    build_exchange_graph active_symbols >>= fun graph ->

    (* Detect arbitrage cycles *)
    detect_arbitrage_cycles graph >>= fun cycles ->

    (* Process profitable cycles *)
    Lwt_list.iter_s (fun cycle ->
      let validated_cycle = calculate_trade_sizes cycle graph in

      if validate_cycle validated_cycle graph then (
        info_f ~section "Executing arbitrage cycle: %s (profit: %.4f%%, volume: %.8f)"
          (String.concat " -> " validated_cycle.path)
          (validated_cycle.profit_pct *. 100.0)
          validated_cycle.bottleneck_volume >>= fun () ->

        create_arbitrage_orders validated_cycle graph cmd_buffer
      ) else (
        debug_f ~section "Cycle validation failed: %s"
          (String.concat " -> " validated_cycle.path) >>= fun () ->
        Lwt.return_unit
      )
    ) cycles >>= fun () ->

    (* Wait before next cycle check *)
    Lwt_unix.sleep 1.0 >>= fun () ->
    arbitrage_loop ()
  in

  arbitrage_loop ()
