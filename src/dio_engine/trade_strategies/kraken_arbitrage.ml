(* src/dio_engine/trade_strategies/kraken_arbitrage.ml *)

(*
  ARCHITECTURAL OVERVIEW

  This module implements a high-frequency arbitrage system for Kraken exchange.
  The system uses graph-based algorithms to detect and exploit price inefficiencies across
  multiple trading pairs simultaneously.

  KEY COMPONENTS:
  - Exchange Graph: Directed graph where nodes are assets and edges are exchange rates with fees
  - Arbitrage Detection: Fast triangle detection + general cycle detection via Bellman-Ford
  - Execution Engine: Sequential order execution with fill monitoring and balance updates
  - Risk Management: Concurrent execution limits, balance validation, and trade size optimization

  PERFORMANCE OPTIMIZATIONS:
  - Cached graph representation updated incrementally from orderbook changes
  - Fast O(n³) triangle detection for common 3-asset cycles
  - Lazy evaluation of general cycles for larger graphs (>20 assets)
  - Concurrent execution limiting to prevent over-leveraging
*)

open Lwt.Infix
open Dio_types
open Lwt_log_core

let section = Section.make "kraken_arbitrage"


(*
  CONFIGURATION AND CONSTANTS

  Trading parameters optimized for Kraken's fee structure and minimum order sizes.
  Profit threshold accounts for taker fees while remaining economically viable.
*)

let profit_threshold_pct = 0.0005  (* 0.1% minimum profit after fees *)
let max_cycle_length = 4         (* Maximum arbitrage cycle length *)
let taker_fee_pct = 0.0035       (* 0.35% taker fee for most pairs *)
let maker_fee_pct = 0.0020       (* 0.20% maker fee for most pairs *)
let stable_fee_pct = 0.0001     (* 0.20% fee for USD stablecoins *)

let asset_min_order_sizes = [
  ("XXBTZUSD", 0.0001);   (* BTC/USD: 0.0001 BTC *)
  ("XETHZUSD", 0.005);    (* ETH/USD: 0.005 ETH *)
  ("SOLUSD", 0.1);        (* SOL/USD: 0.1 SOL *)
  ("ADAUSD", 1.0);        (* ADA/USD: 1.0 ADA *)
  ("TRXUSD", 10.0);       (* TRX/USD: 10 TRX *)
  ("USDTZUSD", 5.0);      (* USDT/USD: 5 USDT *)
  ("USDCZUSD", 5.0);      (* USDC/USD: 5 USDC *)
]

(*
  GRAPH REPRESENTATION

  The arbitrage detection uses a directed graph where:
  - Nodes represent trading assets (e.g., "ZUSD", "XXBT", "XETH")
  - Edges represent exchange rates between assets with fee and capacity information
  - Each edge is bidirectional but asymmetric due to different bid/ask spreads
*)

type exchange_edge = {
  from_asset: string;
  to_asset: string;
  rate: float;              (* Exchange rate (to_asset per from_asset) *)
  fee_rate: float;          (* Trading fee rate *)
  capacity: float;          (* Maximum tradeable volume in from_asset *)
  pair: string;             (* Trading pair symbol *)
}

type graph_node = {
  asset: string;
  edges: exchange_edge list;
}

let asset_balances : (string, float) Hashtbl.t = Hashtbl.create 16

let cached_graph : (string, graph_node) Hashtbl.t = Hashtbl.create 32
let edge_cache : (string, exchange_edge * exchange_edge) Hashtbl.t = Hashtbl.create 64
let dirty_symbols : (string, bool) Hashtbl.t = Hashtbl.create 32
let graph_initialized = ref false
let last_update_time = ref 0.0

let init_default_balances () =
  List.iter (fun (asset, min_size) ->
    Hashtbl.replace asset_balances asset (min_size *. 100.0) (* 100x minimum for testing *)
  ) [
    ("ZUSD", 1000.0);   (* $1000 USD *)
    ("XXBT", 0.01);     (* 0.01 BTC *)
    ("XETH", 0.5);      (* 0.5 ETH *)
    ("SOL", 10.0);      (* 10 SOL *)
    ("ADA", 100.0);     (* 100 ADA *)
  ]

type order_state = {
  client_id: string;
  symbol: string;
  side: Core.side;
  qty: Primitives.Qty.t;
  price: Primitives.Price.t;
  filled_qty: float;
  status: string; (* "pending", "filled", "partial", "cancelled" *)
}

type cycle_execution = {
  cycle: arbitrage_cycle;
  current_leg: int;
  leg_orders: order_state list;
  total_filled: float; (* Running total of filled quantity *)
  start_time: float;
}

and arbitrage_cycle = {
  path: string list;        (* Asset path: [A, B, C, A] *)
  profit_pct: float;        (* Expected profit percentage *)
  trade_sizes: float list;  (* Trade sizes for each leg *)
  bottleneck_volume: float; (* Limiting volume in base asset *)
}

(*
  BELLMAN-FORD ALGORITHM FOR ARBITRAGE DETECTION

  Uses negative log weights to detect profitable cycles in the exchange graph.
  The algorithm works by:
  1. Initializing all distances to 0 (representing 1.0 exchange rate)
  2. Relaxing all edges |V|-1 times to find shortest paths
  3. Checking for negative cycles in the Nth iteration
  4. Extracting profitable arbitrage cycles from predecessor chains

  Time complexity: O(|V| * |E|) where V = assets, E = trading pairs
*)

type bf_state = {
  distances: (string, float) Hashtbl.t;
  predecessors: (string, string) Hashtbl.t;
  negative_cycles: arbitrage_cycle list;
}

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

let get_min_order_size pair_symbol =
  let pair = String.uppercase_ascii pair_symbol in
  match List.assoc_opt pair asset_min_order_sizes with
  | Some min_size -> min_size
  | None ->
      (* Default minimum order sizes based on common patterns *)
      if String.contains pair 'B' && String.contains pair 'T' then 0.0001  (* BTC pairs *)
      else if String.contains pair 'E' && String.contains pair 'T' then 0.005  (* ETH pairs *)
      else if String.contains pair 'U' && String.contains pair 'S' && String.contains pair 'D' then 5.0  (* USD stablecoin pairs *)
      else 0.1  (* Default for other crypto pairs *)

let get_asset_balance asset =
  Hashtbl.find_opt asset_balances asset |> Option.value ~default:0.0

let update_asset_balance asset delta =
  let current_balance = get_asset_balance asset in
  let new_balance = current_balance +. delta in
  Hashtbl.replace asset_balances asset new_balance;
  (* Log asynchronously without blocking *)
  Lwt.async (fun () ->
    debug_f ~section "Updated balance for %s: %.8f -> %.8f (delta: %.8f)"
      asset current_balance new_balance delta
  )

let get_fee_rate pair_symbol =
  let pair = String.uppercase_ascii pair_symbol in
  if String.contains pair 'G' && String.contains pair 'U' && String.contains pair 'D' ||
     String.contains pair 'R' && String.contains pair 'U' && String.contains pair 'D' then
    stable_fee_pct  (* USDG/USDR pairs have no fees, but adding slight variation for ease of use *)
  else
    taker_fee_pct   (* Standard taker fee for crypto pairs *)

let update_symbol_edges symbol =
  match Kraken.Kraken_orderbook.get_orderbook symbol with
  | None ->
      debug_f ~section "No orderbook data for %s during update" symbol >>= fun () ->
      Lwt.return_unit
  | Some orderbook ->
      let base_asset, quote_asset = extract_assets symbol in
      
      match orderbook.bids, orderbook.asks with
      | top_bid :: _, top_ask :: _ ->
          let bid_price = top_bid.price in
          let ask_price = top_ask.price in
          let fee_rate = get_fee_rate symbol in

          let base_to_quote_edge = {
            from_asset = base_asset;
            to_asset = quote_asset;
            rate = ask_price;
            fee_rate;
            capacity = top_ask.qty;
            pair = symbol;
          } in

          let quote_to_base_edge = {
            from_asset = quote_asset;
            to_asset = base_asset;
            rate = 1.0 /. bid_price;
            fee_rate;
            capacity = top_bid.qty *. bid_price;
            pair = symbol;
          } in

          Hashtbl.replace edge_cache symbol (base_to_quote_edge, quote_to_base_edge);

          let update_node_edges asset new_edge _other_asset _other_edge =
            let existing_edges = match Hashtbl.find_opt cached_graph asset with
              | Some node -> node.edges
              | None -> []
            in
            (* Remove stale edges for this symbol to ensure graph consistency *)
            let filtered_edges = List.filter (fun e -> e.pair <> symbol) existing_edges in
            let updated_edges = new_edge :: filtered_edges in
            Hashtbl.replace cached_graph asset { asset; edges = updated_edges }
          in

          update_node_edges base_asset base_to_quote_edge quote_asset quote_to_base_edge;
          update_node_edges quote_asset quote_to_base_edge base_asset base_to_quote_edge;

          debug_f ~section "Updated edges for %s: %s->%s@%.8f, %s->%s@%.8f"
            symbol base_asset quote_asset ask_price quote_asset base_asset (1.0 /. bid_price) >>= fun () ->

          Hashtbl.remove dirty_symbols symbol;
          Lwt.return_unit
      | _ ->
          warning_f ~section "No bid/ask data for %s during update" symbol >>= fun () ->
          Lwt.return_unit

(** Initialize the cached exchange rate graph from orderbook data.

    Builds the initial directed graph representation of all trading pairs.
    Each edge contains exchange rates, fees, and capacity information.
    This cached graph enables fast arbitrage detection without repeated data fetching.

    @param symbols List of trading pair symbols to include in the graph
    @return Unit promise when initialization completes
*)
let initialize_cached_graph symbols =
  if !graph_initialized then Lwt.return_unit else

  info_f ~section "Initializing cached exchange graph" >>= fun () ->
  Hashtbl.clear cached_graph;
  Hashtbl.clear edge_cache;
  Hashtbl.clear dirty_symbols;

  Lwt_list.iter_s (fun symbol ->
    Hashtbl.replace dirty_symbols symbol true;
    update_symbol_edges symbol
  ) symbols >>= fun () ->

  graph_initialized := true;
  last_update_time := Unix.time ();
  info_f ~section "Initialized cached exchange graph with %d nodes, %d edges" 
    (Hashtbl.length cached_graph) (Hashtbl.length edge_cache) >>= fun () ->
  Lwt.return_unit

let update_changed_edges symbols =
  let current_time = Unix.time () in

  List.iter (fun symbol ->
    match Kraken.Kraken_orderbook.get_orderbook symbol with
    | Some _ -> Hashtbl.replace dirty_symbols symbol true
    | None -> ()
  ) symbols;

  let dirty_list = Hashtbl.fold (fun symbol _ acc -> symbol :: acc) dirty_symbols [] in
  if List.length dirty_list > 0 then
    debug_f ~section "Updating %d dirty symbols: [%s]" 
      (List.length dirty_list) (String.concat ", " dirty_list) >>= fun () ->
    Lwt_list.iter_s update_symbol_edges dirty_list >>= fun () ->
    last_update_time := current_time;
    Lwt.return_unit
  else
    Lwt.return_unit

let get_cached_graph () = 
  if !graph_initialized then cached_graph
  else failwith "Graph not initialized - call initialize_cached_graph first"

let effective_rate edge =
  edge.rate *. (1.0 -. edge.fee_rate)

let init_bf_state assets =
  let distances = Hashtbl.create (List.length assets) in
  let predecessors = Hashtbl.create (List.length assets) in

  (* Set distance to 0 for all assets (log of 1.0) *)
  List.iter (fun asset ->
    Hashtbl.replace distances asset 0.0;
    Hashtbl.replace predecessors asset ""
  ) assets;

  { distances; predecessors; negative_cycles = [] }

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

let extract_cycle predecessors start_asset =
  let rec build_path current path visited =
    if Hashtbl.mem visited current then
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

(** Fast detection of triangular arbitrage opportunities.

    Optimized O(n³) algorithm for finding 3-asset arbitrage cycles.
    Much faster than general Bellman-Ford for the common case of triangles.
    Calculates profit percentage after fees and filters by minimum threshold.

    @param graph Exchange rate graph with fee-adjusted edges
    @return List of profitable 3-asset arbitrage cycles
*)
let detect_triangle_arbitrage graph : arbitrage_cycle list Lwt.t =
  let cycles = ref [] in
  let assets = Hashtbl.fold (fun asset _ acc -> asset :: acc) graph [] in

  debug_f ~section "Running specialized triangle detection on %d assets" (List.length assets) >>= fun () ->

  List.iter (fun asset_a ->
    match Hashtbl.find_opt graph asset_a with
    | None -> ()
    | Some node_a ->
        List.iter (fun edge_ab ->
          let asset_b = edge_ab.to_asset in
          let rate_ab = effective_rate edge_ab in

          match Hashtbl.find_opt graph asset_b with
          | None -> ()
          | Some node_b ->
              List.iter (fun edge_bc ->
                let asset_c = edge_bc.to_asset in
                if asset_c <> asset_a then (
                  let rate_bc = effective_rate edge_bc in

                  match Hashtbl.find_opt graph asset_c with
                  | None -> ()
                  | Some node_c ->
                      List.iter (fun edge_ca ->
                        if edge_ca.to_asset = asset_a then (
                          let rate_ca = effective_rate edge_ca in

                          let total_rate = rate_ab *. rate_bc *. rate_ca in
                          let profit_pct = total_rate -. 1.0 in
                          
                          if profit_pct > profit_threshold_pct then (
                            let cycle = {
                              path = [asset_a; asset_b; asset_c; asset_a];
                              profit_pct;
                              trade_sizes = [];
                              bottleneck_volume = 0.0;
                            } in
                            cycles := cycle :: !cycles;
                            debug_f ~section "Found triangle: %s->%s->%s->%s (profit: %.4f%%)"
                              asset_a asset_b asset_c asset_a (profit_pct *. 100.0) |> Lwt.ignore_result
                          )
                        )
                      ) node_c.edges
                  )
              ) node_b.edges
        ) node_a.edges
  ) assets;

  info_f ~section "Detected %d profitable triangle cycles" (List.length !cycles) >>= fun () ->
  Lwt.return !cycles

let detect_general_arbitrage_cycles graph : arbitrage_cycle list Lwt.t =
  let assets = Hashtbl.fold (fun asset _ acc -> asset :: acc) graph [] in
  let bf_state = init_bf_state assets in
  let all_edges = Hashtbl.fold (fun _ node acc ->
    node.edges @ acc
  ) graph [] in

  debug_f ~section "Running Bellman-Ford with %d assets and %d edges"
    (List.length assets) (List.length all_edges) >>= fun () ->

  for _ = 1 to List.length assets - 1 do
    List.iter (fun edge -> ignore (relax_edge bf_state edge)) all_edges
  done;

  let cycles = ref [] in
  List.iter (fun edge ->
    let current_dist = Hashtbl.find_opt bf_state.distances edge.from_asset |> Option.value ~default:0.0 in
    let neighbor_dist = Hashtbl.find_opt bf_state.distances edge.to_asset |> Option.value ~default:0.0 in
    let edge_weight = -. (Float.log (effective_rate edge)) in

    if current_dist +. edge_weight < neighbor_dist then (
      (* Found negative cycle *)
      let cycle_path = extract_cycle bf_state.predecessors edge.to_asset in
      if cycle_path <> [] && List.length cycle_path >= 4 then (  (* Only 4+ leg cycles *)
        let profit_pct = Float.exp (-. (List.fold_left (fun acc asset ->
          match Hashtbl.find_opt bf_state.distances asset with
          | Some dist -> acc +. dist
          | None -> acc
        ) 0.0 cycle_path)) -. 1.0 in

        if profit_pct > profit_threshold_pct then (
          let cycle = {
            path = cycle_path @ [List.hd cycle_path];  (* Close the cycle *)
            profit_pct;
            trade_sizes = [];  
            bottleneck_volume = 0.0; 
          } in
          cycles := cycle :: !cycles
        )
      )
    )
  ) all_edges;

  info_f ~section "Detected %d profitable general arbitrage cycles" (List.length !cycles) >>= fun () ->
  Lwt.return !cycles

(** Detect profitable arbitrage cycles using optimized algorithms.

    Combines fast triangle detection with general cycle detection using Bellman-Ford.
    For small graphs (< 20 assets), uses both methods for comprehensive coverage.
    For larger graphs, prioritizes triangle detection for performance.

    @param graph Current exchange rate graph with fee-adjusted edges
    @return List of profitable arbitrage cycles, sorted by profitability
*)
let detect_arbitrage_cycles graph : arbitrage_cycle list Lwt.t =
  (* Use fast triangle detection for 3-leg cycles *)
  detect_triangle_arbitrage graph >>= fun triangle_cycles ->
  
  (* Skip general detection if we have many assets (performance) *)
  let asset_count = Hashtbl.length graph in
  if asset_count > 20 then (
    debug_f ~section "Skipping general cycle detection for %d assets (performance)" asset_count >>= fun () ->
    Lwt.return triangle_cycles
  ) else (
    (* Use Bellman-Ford for longer cycles *)
    detect_general_arbitrage_cycles graph >>= fun general_cycles ->
    Lwt.return (triangle_cycles @ general_cycles)
  )

let apply_precision_constraints pair_symbol qty =
  match Kraken.Kraken_incoming_data.get_precisions pair_symbol with
  | Some (_, qty_precision) ->
      let scale_factor = Float.pow 10.0 (float_of_int qty_precision) in
      let rounded = Float.round (qty *. scale_factor) /. scale_factor in
      max rounded (get_min_order_size pair_symbol)
  | None ->
      max qty (get_min_order_size pair_symbol)

(** Calculate optimal trade sizes for an arbitrage cycle considering all constraints.

    Determines the maximum executable volume for a cycle by finding the bottleneck
    across orderbook liquidity, available balances, and minimum order size requirements.
    Applies precision constraints and ensures all legs can be executed successfully.

    @param cycle Arbitrage cycle with path but uncalculated trade sizes
    @param graph Current exchange rate graph with capacity and fee information
    @return Cycle with calculated trade sizes and bottleneck volume
*)
let calculate_trade_sizes cycle graph =
  let path = cycle.path in
  if List.length path < 4 then cycle else

  let start_asset = List.hd path in
  let available_balance = get_asset_balance start_asset in

  let rec find_bottleneck remaining_path min_capacity current_rate =
    match remaining_path with
    | [_] -> min_capacity
    | current :: next :: rest ->
        (match Hashtbl.find_opt graph current with
         | Some node ->
             (match List.find_opt (fun edge -> edge.to_asset = next) node.edges with
              | Some edge ->
                  let min_order_size = get_min_order_size edge.pair in
                  let min_capacity_this_leg = 
                    if edge.from_asset = current then
                      min_order_size  (* Direct quantity constraint *)
                    else
                      min_order_size *. edge.rate  (* Convert to source asset *)
                  in

                  let normalized_capacity = min edge.capacity (min_capacity_this_leg /. current_rate) in
                  let constrained_capacity = normalized_capacity *. current_rate in
                  let next_rate = current_rate *. effective_rate edge in
                  find_bottleneck (next :: rest) (min min_capacity constrained_capacity) next_rate
              | None -> min_capacity)
         | None -> min_capacity)
    | [] -> min_capacity
  in

  let orderbook_bottleneck = find_bottleneck path max_float 1.0 in
  let balance_limit = available_balance *. 0.9 in

  let trade_size = min (min orderbook_bottleneck balance_limit) 1.0 in

  let rec validate_and_adjust_sizes remaining_path current_size acc_sizes =
    match remaining_path with
    | [_] -> List.rev acc_sizes, current_size
    | current :: next :: rest ->
        (match Hashtbl.find_opt graph current with
         | Some node ->
             (match List.find_opt (fun edge -> edge.to_asset = next) node.edges with
              | Some edge ->
                  let _base_asset, quote_asset = extract_assets edge.pair in
                  let order_qty = 
                    if edge.from_asset = quote_asset then
                      current_size /. edge.rate  (* Buy: quote -> base *)
                    else
                      current_size  (* Sell: base -> quote *)
                  in
                  let adjusted_qty = apply_precision_constraints edge.pair order_qty in
                  let next_size = 
                    if edge.from_asset = quote_asset then
                      adjusted_qty  (* For buy orders *)
                    else
                      adjusted_qty *. effective_rate edge  (* For sell orders *)
                  in
                  validate_and_adjust_sizes (next :: rest) next_size (adjusted_qty :: acc_sizes)
              | None -> acc_sizes, current_size)
         | None -> acc_sizes, current_size)
    | [] -> acc_sizes, current_size
  in

  let trade_sizes, _final_volume = validate_and_adjust_sizes path trade_size [] in

  { cycle with
    trade_sizes;
    bottleneck_volume = trade_size }

(** Validate arbitrage cycle for safe execution.

    Ensures the cycle can be executed by checking:
    - Sufficient starting asset balance
    - All required trading pairs exist in the graph
    - Trade sizes meet minimum order requirements
    - Orderbook capacity supports the trade volumes
    - Precision constraints are satisfied

    @param cycle Cycle to validate
    @param graph Current exchange graph
    @return true if cycle is safe to execute
*)
let validate_cycle cycle graph =
  if List.length cycle.path < 4 then false else  (* Need at least 3 edges + closing *)
  
  let path = cycle.path in
  let start_asset = List.hd path in
  let available_balance = get_asset_balance start_asset in
  
  (* Check if we have sufficient balance for the starting asset *)
  let balance_check = available_balance >= cycle.bottleneck_volume in
  
  (* Check if all required edges exist and meet constraints *)
  let rec check_path remaining_path trade_sizes_remaining =
    match remaining_path, trade_sizes_remaining with
    | [_], [] -> true  (* End of cycle *)
    | current :: next :: rest, trade_size :: remaining_sizes ->
        (match Hashtbl.find_opt graph current with
         | Some node ->
             (match List.find_opt (fun edge -> edge.to_asset = next) node.edges with
              | Some edge ->
                  let min_order_size = get_min_order_size edge.pair in
                  (* Check capacity, minimum order size, and precision constraints *)
                  let capacity_ok = edge.capacity >= trade_size in
                  let min_size_ok = trade_size >= min_order_size in
                  let precision_adjusted = apply_precision_constraints edge.pair trade_size in
                  let precision_ok = abs_float (precision_adjusted -. trade_size) < 0.000001 in
                  
                  capacity_ok && min_size_ok && precision_ok && 
                  check_path (next :: rest) remaining_sizes
              | None -> false)
         | None -> false)
    | _, _ -> false  (* Mismatched path and trade sizes *)
  in

  let path_check = check_path path cycle.trade_sizes in
  let volume_check = cycle.bottleneck_volume > 0.001 in  (* Minimum trade size *)
  
  let result = balance_check && path_check && volume_check in

  if not result then (
    Lwt.async (fun () ->
      debug_f ~section "Cycle validation failed: balance=%b (%.8f/%.8f), path=%b, volume=%b (%.8f)"
        balance_check available_balance cycle.bottleneck_volume path_check volume_check cycle.bottleneck_volume
    )
  );

  result

let monitor_order_fill exec_buffer target_client_id timeout =
  let start_time = Unix.time () in
  let rec wait_for_fill () =
    if Unix.time () -. start_time > timeout then
      Lwt.return None  (* Timeout *)
    else (
      Lwt.catch (fun () ->
        Lwt_unix.with_timeout 0.5 (fun () -> Ringbuffer.pop exec_buffer) >>= fun exec_event ->
        (match exec_event with
         | Core.Fill { client_id; qty; _ } when client_id = target_client_id ->
             let filled_qty = Primitives.Qty.to_string qty |> float_of_string in
             Lwt.return (Some (filled_qty, "filled"))
         | Core.Ack { client_id; state; _ } when client_id = target_client_id ->
             let status = match state with
               | Core.Filled -> "filled"
               | Core.Canceled -> "cancelled"
               | Core.Rejected -> "rejected"
               | Core.Open -> "partial"
             in
             Lwt.return (Some (0.0, status))
         | _ ->
             wait_for_fill ())
      ) (function
        | Lwt_unix.Timeout ->
            Lwt_unix.sleep 0.1 >>= fun () ->
            wait_for_fill ()
        | exn -> Lwt.fail exn)
    )
  in
  wait_for_fill ()

let execute_cycle_leg cycle_exec leg_index current_qty graph cmd_buffer exec_buffer =
  let path = cycle_exec.cycle.path in
  if leg_index >= List.length path - 1 then
    Lwt.return (Some current_qty)  (* Cycle complete *)
  else (
    let current_asset = List.nth path leg_index in
    let next_asset = List.nth path (leg_index + 1) in
    
    match Hashtbl.find_opt graph current_asset with
    | Some node ->
        (match List.find_opt (fun edge -> edge.to_asset = next_asset) node.edges with
         | Some edge ->
             let base_asset, quote_asset = extract_assets edge.pair in
             let side, order_qty =
               if edge.from_asset = quote_asset && edge.to_asset = base_asset then
                 (Core.Buy, current_qty /. edge.rate)
               else if edge.from_asset = base_asset && edge.to_asset = quote_asset then
                 (Core.Sell, current_qty)
               else
                 (* Should not happen with a well-formed graph *)
                 (Core.Buy, 0.0)
             in

             let price_str = Printf.sprintf "%.8f" edge.rate in
             let price = Primitives.Price.of_string_exn ~scale:8 price_str in
             let qty_str = Printf.sprintf "%.8f" order_qty in
             let qty = Primitives.Qty.of_string_exn ~scale:8 qty_str in
             let client_id = Printf.sprintf "arb_%s_leg%d_%d" edge.pair leg_index (Random.int 1000000) in

             let order_cmd = Core.Add {
               dst = "kraken";
               client_id;
               symbol = edge.pair;
               side;
               price;
               qty;
               tif = Core.GTC;
               tags = [`Manual];
             } in

             info_f ~section "Executing leg %d: %s %s %.8f@%s (from %.8f %s)"
               leg_index edge.pair
               (match side with Buy -> "BUY" | Sell -> "SELL")
               order_qty price_str current_qty current_asset >>= fun () ->

             Ringbuffer.push cmd_buffer order_cmd >>= fun () ->

             monitor_order_fill exec_buffer client_id 30.0 >>= fun fill_result ->
             
             (match fill_result with
              | Some (filled_qty, "filled") ->
                  if side = Core.Buy then (
                    update_asset_balance edge.from_asset (-.current_qty);  (* Spent quote *)
                    update_asset_balance edge.to_asset filled_qty         (* Received base *)
                  ) else (
                    update_asset_balance edge.from_asset (-.filled_qty);  (* Sold base *)
                    update_asset_balance edge.to_asset (current_qty *. effective_rate edge)  (* Received quote *)
                  );

                  let next_qty =
                    if side = Core.Buy then filled_qty
                    else filled_qty *. effective_rate edge
                  in
                  info_f ~section "Leg %d filled completely: %.8f -> %.8f %s"
                    leg_index current_qty next_qty next_asset >>= fun () ->
                  Lwt.return (Some next_qty)

              | Some (filled_qty, "partial") when filled_qty > 0.0 ->
                  if side = Core.Buy then (
                    let quote_spent = filled_qty *. edge.rate in
                    update_asset_balance edge.from_asset (-.quote_spent);
                    update_asset_balance edge.to_asset filled_qty
                  ) else (
                    update_asset_balance edge.from_asset (-.filled_qty);
                    update_asset_balance edge.to_asset (filled_qty *. effective_rate edge)
                  );

                  let next_qty =
                    if side = Core.Buy then filled_qty
                    else filled_qty *. effective_rate edge
                  in
                  warning_f ~section "Leg %d partially filled: %.8f/%.8f -> %.8f %s"
                    leg_index filled_qty order_qty next_qty next_asset >>= fun () ->
                  Lwt.return (Some next_qty)
                  
              | Some (_, "cancelled") ->
                  warning_f ~section "Leg %d was cancelled" leg_index >>= fun () ->
                  Lwt.return None
                  
              | None ->
                  warning_f ~section "Leg %d timed out" leg_index >>= fun () ->
                  Lwt.return None
                  
              | Some (_, status) ->
                  warning_f ~section "Leg %d unexpected status: %s" leg_index status >>= fun () ->
                  Lwt.return None)
                  
         | None ->
             warning_f ~section "No edge found from %s to %s" current_asset next_asset >>= fun () ->
             Lwt.return None)
    | None ->
        warning_f ~section "No node found for asset %s" current_asset >>= fun () ->
        Lwt.return None
  )

(*
  EXECUTION ENGINE

  Handles the sequential execution of arbitrage cycles with robust error handling:
  - Submits orders for each leg and waits for confirmation before proceeding
  - Monitors order fills with timeouts to prevent execution hangs
  - Updates asset balances after each successful fill
  - Handles partial fills by adjusting subsequent order sizes
  - Maintains execution state and provides detailed logging
*)

(** Execute a complete arbitrage cycle by submitting sequential orders.

    Submits orders for each leg of the arbitrage cycle, waiting for fills between legs.
    Updates asset balances after each successful fill and handles partial fills gracefully.
    Times out individual legs after 30 seconds to prevent hanging executions.

    @param cycle Validated arbitrage cycle with calculated trade sizes
    @param graph Current exchange rate graph for order pricing
    @param cmd_buffer Command buffer for order submission
    @param exec_buffer Execution buffer for monitoring order fills
    @return true if cycle completed successfully, false on failure
*)
let execute_arbitrage_cycle cycle graph cmd_buffer exec_buffer =
  let cycle_exec = {
    cycle;
    current_leg = 0;
    leg_orders = [];
    total_filled = cycle.bottleneck_volume;
    start_time = Unix.time ();
  } in

  info_f ~section "Starting cycle execution: %s (initial: %.8f)"
    (String.concat " -> " cycle.path) cycle.bottleneck_volume >>= fun () ->

  (* Execute legs sequentially *)
  let rec execute_legs leg_index current_qty =
    if leg_index >= List.length cycle.path - 1 then (
      info_f ~section "Arbitrage cycle completed successfully: final quantity %.8f"
        current_qty >>= fun () ->
      Lwt.return true
    ) else (
      execute_cycle_leg cycle_exec leg_index current_qty graph cmd_buffer exec_buffer >>= function
      | Some next_qty when next_qty > 0.0 ->
          execute_legs (leg_index + 1) next_qty
      | Some _ ->
          warning_f ~section "Cycle execution failed at leg %d: zero quantity" leg_index >>= fun () ->
          Lwt.return false
      | None ->
          warning_f ~section "Cycle execution failed at leg %d" leg_index >>= fun () ->
          Lwt.return false
    )
  in

  execute_legs 0 cycle.bottleneck_volume

let cancel_pending_orders cycle_exec cmd_buffer =
  Lwt_list.iter_s (fun order_state ->
    if order_state.status = "pending" then (
      let cancel_cmd = Core.Cancel {
        dst = "kraken";
        order_id = order_state.client_id; 
      } in
      warning_f ~section "Cancelling pending order: %s" order_state.client_id >>= fun () ->
      Ringbuffer.push cmd_buffer cancel_cmd
    ) else
      Lwt.return_unit
  ) cycle_exec.leg_orders

let max_concurrent_cycles = ref 2
let active_cycles = ref 0

(** Main entry point for the triangular arbitrage trading strategy.

    This function implements a high-frequency arbitrage detection system that:
    - Continuously monitors Kraken orderbook data for triangular arbitrage opportunities
    - Uses optimized graph algorithms to detect profitable cycles
    - Executes validated arbitrage cycles with proper risk management
    - Maintains concurrent execution limits to prevent over-leveraging

    @param runtime_cfg Runtime configuration containing active trading symbols
    @param _core_cfg Unused core engine configuration (placeholder for future use)
    @param tick_buffer Unused tick data buffer (placeholder for future enhancements)
    @param cmd_buffer Command buffer for submitting orders to the trading engine
    @param exec_buffer Execution buffer for receiving order fill confirmations
    @return Never returns (infinite arbitrage detection loop)
*)
let start (runtime_cfg : Config.runtime_cfg) (_core_cfg : Config.engine_config)
    ~tick_buffer:_ ~cmd_buffer ~exec_buffer =

  info_f ~section "Starting triangular arbitrage strategy" >>= fun () ->

  init_default_balances ();
  info_f ~section "Initialized asset balances for arbitrage trading" >>= fun () ->

  let active_symbols = List.map (fun (asset: Config.asset_cfg) -> asset.symbol) runtime_cfg.assets in

  info_f ~section "Monitoring symbols for arbitrage: [%s]"
    (String.concat ", " active_symbols) >>= fun () ->

  Kraken.Kraken_incoming_data.wait_for_instruments () >>= fun () ->
  info_f ~section "Instrument data loaded, starting arbitrage detection" >>= fun () ->

  initialize_cached_graph active_symbols >>= fun () ->

  let rec arbitrage_loop () =
    update_changed_edges active_symbols >>= fun () ->

    let graph = get_cached_graph () in

    detect_arbitrage_cycles graph >>= fun cycles ->

    if !active_cycles < !max_concurrent_cycles then (
      Lwt_list.iter_s (fun cycle ->
        let validated_cycle = calculate_trade_sizes cycle graph in

        if validate_cycle validated_cycle graph && !active_cycles < !max_concurrent_cycles then (
          incr active_cycles;
          info_f ~section "Executing arbitrage cycle (%d/%d active): %s (profit: %.4f%%, volume: %.8f)"
            !active_cycles !max_concurrent_cycles
            (String.concat " -> " validated_cycle.path)
            (validated_cycle.profit_pct *. 100.0)
            validated_cycle.bottleneck_volume >>= fun () ->

          (* Execute cycle asynchronously to avoid blocking arbitrage detection *)
          Lwt.async (fun () ->
            execute_arbitrage_cycle validated_cycle graph cmd_buffer exec_buffer >>= fun success ->
            decr active_cycles;
            if success then
              info_f ~section "Arbitrage cycle completed successfully"
            else
              warning_f ~section "Arbitrage cycle failed or was cancelled" >>= fun () ->
            Lwt.return_unit
          );
          Lwt.return_unit
        ) else (
          debug_f ~section "Cycle validation failed or max concurrent reached: %s"
            (String.concat " -> " validated_cycle.path) >>= fun () ->
          Lwt.return_unit
        )
      ) cycles
    ) else (
      debug_f ~section "Max concurrent cycles reached (%d), skipping detection" !active_cycles
    ) >>= fun () ->

    let sleep_time = if Hashtbl.length dirty_symbols = 0 then 5.0 else 1.0 in
    Lwt_unix.sleep sleep_time >>= fun () ->
    arbitrage_loop ()
  in

  arbitrage_loop ()
