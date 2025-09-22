(**
 * Terminal-based trading dashboard for real-time cryptocurrency portfolio monitoring.
 *
 * Provides a live view of:
 * - Active trading assets with price ladders and order book visualization
 * - Portfolio balances and performance metrics
 * - System logs and status indicators
 * - Strategy assignments and trading activity
 *
 * Features interactive controls for toggling views and real-time data updates.
 *)

open Lwt.Infix
open Notty
open Notty.A
open Dio_types
module Stats = Stats
module M = State.SMap
module StringSet = Set.Make(String)

let section = Lwt_log_core.Section.make "dashboard"

let is_stablecoin asset =
  let stablecoins = ["USD"; "USDT"; "USDC"; "USDG"; "USDR"] in
  List.mem asset stablecoins

(* Mutex to prevent race conditions in dashboard state updates *)
let state_mutex = Lwt_mutex.create ()

(** Terminal UI color palette and text styles for consistent visual theming *)
let rgb_of_255 ~r ~g ~b = A.rgb ~r:(r*5/255) ~g:(g*5/255) ~b:(b*5/255)

(** Base text styles *)
let style_primary_text    = A.fg (rgb_of_255 ~r:200 ~g:200 ~b:200)
let style_neutral_text    = A.fg (rgb_of_255 ~r:200 ~g:200 ~b:200)

(** Trading-specific styles *)
let style_buy_order_text  = A.fg (rgb_of_255 ~r:0 ~g:255 ~b:100) ++ A.st A.bold
let style_sell_order_text = A.fg (rgb_of_255 ~r:255 ~g:100 ~b:100) ++ A.st A.bold
let style_current_price_text= A.fg (rgb_of_255 ~r:0 ~g:200 ~b:200) ++ A.st A.bold ++ A.st A.underline

(** Performance indicators *)
let style_profit_text     = A.fg (rgb_of_255 ~r:50 ~g:255 ~b:100) ++ A.st A.bold
let style_loss_text       = A.fg (rgb_of_255 ~r:255 ~g:100 ~b:100) ++ A.st A.bold

(** UI element styles *)
let style_header_border   = A.fg (rgb_of_255 ~r:0 ~g:150 ~b:150) ++ A.st A.bold
let style_highlight_text  = A.fg (rgb_of_255 ~r:255 ~g:200 ~b:100) ++ A.st A.bold
let style_warning_text    = A.fg (rgb_of_255 ~r:255 ~g:150 ~b:50) ++ A.st A.bold
let style_success_text    = A.fg (rgb_of_255 ~r:100 ~g:255 ~b:150) ++ A.st A.bold
let style_logs_accent_text= A.fg (rgb_of_255 ~r:255 ~g:200 ~b:100) ++ A.st A.bold

(** Strategy-specific styles *)
let style_strat_grid      = A.fg (rgb_of_255 ~r:130 ~g:180 ~b:255) ++ A.st A.bold
let style_strat_gmm       = A.fg (rgb_of_255 ~r:100 ~g:220 ~b:100) ++ A.st A.bold
let style_strat_arb       = A.fg (rgb_of_255 ~r:255 ~g:120 ~b:255) ++ A.st A.bold
let style_strat_vmm       = A.fg (rgb_of_255 ~r:255 ~g:220 ~b:100) ++ A.st A.bold
let style_strat_monitor   = A.fg (rgb_of_255 ~r:170 ~g:170 ~b:170)

(** Status indicators *)
let style_active_indicator = A.fg (rgb_of_255 ~r:50 ~g:255 ~b:150) ++ A.st A.bold
let style_inactive_indicator = A.fg (rgb_of_255 ~r:150 ~g:150 ~b:150)
let style_error_indicator  = A.fg (rgb_of_255 ~r:255 ~g:100 ~b:100) ++ A.st A.bold

(** Log level styles *)
let style_log_info = A.fg (rgb_of_255 ~r:100 ~g:255 ~b:150)
let style_log_warning = A.fg (rgb_of_255 ~r:255 ~g:200 ~b:100)
let style_log_error = A.fg (rgb_of_255 ~r:255 ~g:100 ~b:100)
let style_log_debug = style_primary_text

(** Composite styles for common UI elements *)
let style_asset_name = style_current_price_text ++ A.st A.bold ++ A.st A.underline
let style_header_title_art = style_header_border ++ A.st A.bold
let style_header_info_text = style_primary_text
let style_keybinding_bracket = style_header_border
let style_keybinding_text = style_primary_text

(** Enhanced Unicode visual symbols for modern dashboard elements *)
let spr_power_pellet  = I.string style_logs_accent_text "●"  (* Bullet - log highlights *)
let spr_buy_order     = I.string style_buy_order_text "▼"    (* Down Triangle - buy orders *)
let spr_sell_order    = I.string style_sell_order_text "▲"   (* Up Triangle - sell orders *)
let spr_price_now frame =
  let blink = (frame / 8) mod 2 = 0 in
  let style = if blink then
    style_current_price_text ++ A.st A.bold ++ A.st A.underline
  else
    style_current_price_text ++ A.st A.bold in
  I.string style "●" (* Solid Bullet - current price indicator *)
let spr_profit        = I.string style_profit_text "▲"      (* Profit indicator *)
let spr_loss          = I.string style_loss_text "▼"        (* Loss indicator *)
let spr_neutral       = I.string style_neutral_text "─"     (* Neutral indicator *)
let spr_success_text  = I.string style_success_text "●"    (* Success/Low volatility indicator *)
let spr_active        = I.string style_active_indicator "●"  (* Active status *)
let spr_inactive      = I.string style_inactive_indicator "○" (* Inactive status *)
let spr_warning       = I.string style_warning_text "▲"     (* Warning status *)
let spr_error         = I.string style_error_indicator "▼"  (* Error status *)
let spr_grid          = I.string style_highlight_text "G"    (* Grid strategy - single letter *)
let spr_orderbook     = I.string style_highlight_text "M"    (* Market Maker strategy *)
let spr_arbitrage     = I.string style_highlight_text "A"    (* Arbitrage strategy *)
    
(** Additional modern visual elements *)
let spr_bullet        = I.string style_primary_text "•"
let spr_arrow_right   = I.string style_highlight_text "→"
let spr_arrow_up      = I.string style_profit_text "↑"
let spr_arrow_down    = I.string style_loss_text "↓"
let spr_bar_full      = I.string style_success_text "█"
let spr_bar_half      = I.string style_warning_text "▓"
let spr_bar_empty     = I.string style_neutral_text "░"
let spr_separator     = I.string style_header_border "│"
let spr_divider       = I.string style_header_border "─"
let spr_corner_tl     = I.string style_header_border "┌"
let spr_corner_tr     = I.string style_header_border "┐"
let spr_corner_bl     = I.string style_header_border "└"
let spr_corner_br     = I.string style_header_border "┘"
let spr_tee_down      = I.string style_header_border "┬"
let spr_tee_up        = I.string style_header_border "┴"
let spr_tee_right     = I.string style_header_border "├"
let spr_tee_left      = I.string style_header_border "┤"

let style_of_log_level level_str =
  match String.lowercase_ascii level_str with
  | "info" -> style_log_info
  | "warning" -> style_log_warning
  | "error" -> style_log_error
  | _ -> style_log_debug


(* ─── helpers ─────────────────────────────────────────────── *)
let fmt_runtime start =
  let secs = int_of_float (Unix.gettimeofday () -. start) in
  Printf.sprintf "%02d:%02d:%02d"
    (secs / 3600) (secs mod 3600 / 60) (secs mod 60)

let get_term_dimensions () =
  match Notty_unix.winsize Unix.stdout with
  | Some (w, h) -> (h, w)
  | None -> (24, 80)

(** Create a horizontal progress bar with visual segments *)
let create_progress_bar ~width ~filled ~style_full ~style_empty =
  let filled_chars = max 0 (min width (int_of_float (float_of_int width *. filled))) in
  let empty_chars = width - filled_chars in
  let filled_bar = String.concat "" (List.init filled_chars (fun _ -> "█")) in
  let empty_bar = String.concat "" (List.init empty_chars (fun _ -> "░")) in
  I.hcat [
    I.string style_full filled_bar;
    I.string style_empty empty_bar
  ]

(** Create a mini sparkline chart from price data *)
let create_sparkline ~width ~height ~prices ~current_price =
  if prices = [] then
    I.string style_neutral_text (String.concat "" (List.init width (fun _ -> "─")))
  else
    let min_price = List.fold_left min (List.hd prices) prices in
    let max_price = List.fold_left max (List.hd prices) prices in
    let price_range = if max_price = min_price then 1.0 else max_price -. min_price in
    let normalize_price p = (p -. min_price) /. price_range in

    let current_idx = if List.length prices > 0 then
      let current_norm = normalize_price current_price in
      max 0 (min (width - 1) (int_of_float (current_norm *. float_of_int (width - 1))))
    else 0 in

    let spark_chars = List.mapi (fun i p ->
      let norm = normalize_price p in
      let pos = int_of_float (norm *. float_of_int (height - 1)) in
      let char = if i = current_idx then "●" else
                 if pos = height - 1 then "▀" else
                 if pos = 0 then "▄" else "─" in
      if i = current_idx then I.string style_current_price_text char
      else I.string style_neutral_text char
    ) prices in

    I.hcat spark_chars

(** Maintain a simple in-memory rolling price history per asset for trend display *)
let price_history_tbl : (string, float list) Hashtbl.t = Hashtbl.create 32
let max_history_points = 120

let update_price_history asset price =
  let existing = Option.value ~default:[] (Hashtbl.find_opt price_history_tbl asset) in
  let new_list =
    match existing with
    | last :: _ when Float.abs (last -. price) < 1e-9 -> existing
    | _ -> price :: existing
  in
  let rec take_first n lst =
    if n <= 0 then [] else match lst with [] -> [] | h :: t -> h :: take_first (n - 1) t
  in
  let trimmed = if List.length new_list > max_history_points then take_first max_history_points new_list else new_list in
  Hashtbl.replace price_history_tbl asset trimmed

let get_price_history asset =
  match Hashtbl.find_opt price_history_tbl asset with
  | Some lst -> List.rev lst  (* oldest -> newest for left-to-right rendering *)
  | None -> []

(** Render a single-row block-character sparkline that is visually clear *)
let create_block_sparkline ~width ~prices =
  let prices =
    if prices = [] then []
    else if List.length prices <= width then prices
    else (
      let total = List.length prices in
      let stride = (float_of_int total) /. (float_of_int width) in
      let rec sample i acc =
        if i >= width then List.rev acc
        else
          let idx = int_of_float (floor ((float_of_int i) *. stride)) in
          match List.nth_opt prices (min (total - 1) idx) with
          | Some v -> sample (i + 1) (v :: acc)
          | None -> sample (i + 1) acc
      in
      sample 0 []
    )
  in
  let levels = [| "▁"; "▂"; "▃"; "▄"; "▅"; "▆"; "▇"; "█" |] in
  let img_of_prices ps =
    match ps with
    | [] -> I.string style_neutral_text (String.concat "" (List.init (max 0 width) (fun _ -> "─")))
    | _ ->
        let min_p = List.fold_left min (List.hd ps) ps in
        let max_p = List.fold_left max (List.hd ps) ps in
        let range = if max_p = min_p then 1.0 else max_p -. min_p in
        let to_lvl p = int_of_float (min 7.0 (max 0.0 (((p -. min_p) /. range) *. 7.0))) in
        let first = List.hd ps in
        let last = List.hd (List.rev ps) in
        let trend_up = last -. first >= 0.0 in
        let rec build acc idx rest =
          match rest with
          | [] -> List.rev acc
          | [p] ->
              let lvl = to_lvl p in
              let ch = levels.(lvl) in
              let style = if trend_up then style_profit_text else style_loss_text in
              build (I.string style ch :: acc) (idx + 1) []
          | p :: xs ->
              let lvl = to_lvl p in
              let ch = levels.(lvl) in
              build (I.string style_neutral_text ch :: acc) (idx + 1) xs
        in
        I.hcat (build [] 0 ps)
  in
  img_of_prices prices

(** Render a multi-row (e.g., 2-row) block-character sparkline for higher vertical clarity *)
let create_block_sparkline_multi ~width ~rows ~prices =
  let clamp_rows = max 1 rows in
  let prices =
    if prices = [] then []
    else if List.length prices <= width then prices
    else (
      let total = List.length prices in
      let stride = (float_of_int total) /. (float_of_int width) in
      let rec sample i acc =
        if i >= width then List.rev acc
        else
          let idx = int_of_float (floor ((float_of_int i) *. stride)) in
          match List.nth_opt prices (min (total - 1) idx) with
          | Some v -> sample (i + 1) (v :: acc)
          | None -> sample (i + 1) acc
      in
      sample 0 []
    )
  in
  let img_of_prices ps =
    match ps with
    | [] -> I.string style_neutral_text (String.concat "" (List.init (max 0 width) (fun _ -> "─")))
    | _ ->
        let min_p = List.fold_left min (List.hd ps) ps in
        let max_p = List.fold_left max (List.hd ps) ps in
        let range = if max_p = min_p then 1.0 else max_p -. min_p in
        let halves_total = clamp_rows * 2 in
        let to_halves p = int_of_float (min (float_of_int halves_total) (max 0.0 (((p -. min_p) /. range) *. float_of_int halves_total))) in
        let first = List.hd ps in
        let last = List.hd (List.rev ps) in
        let trend_up = last -. first >= 0.0 in
        let color_for_idx idx = if idx = width - 1 then (if trend_up then style_profit_text else style_loss_text) else style_neutral_text in
        let build_rows () =
          let cols = List.mapi (fun idx p -> (idx, to_halves p)) ps in
          let build_row row_idx =
            let row_from_bottom = row_idx in
            let top_row = clamp_rows - 1 in
            let is_top = row_from_bottom = top_row in
            let char_for_col idx halves =
              (* Distribute halves from bottom to top *)
              let halves_for_bottom = min 2 halves in
              let halves_remaining = max 0 (halves - halves_for_bottom) in
              let halves_for_this_row =
                if is_top then min 2 halves_remaining else halves_for_bottom
              in
              match halves_for_this_row with
              | 0 -> I.string style_neutral_text " "
              | 1 -> I.string (color_for_idx idx) (if is_top then "▀" else "▄")
              | _ -> I.string (color_for_idx idx) "█"
            in
            I.hcat (List.map (fun (idx, h) -> char_for_col idx h) cols)
          in
          (* Build from top to bottom for visual order *)
          let rec loop r acc = if r < 0 then acc else loop (r - 1) (build_row r :: acc) in
          List.rev (loop (clamp_rows - 1) [])
        in
        I.vcat (build_rows ())
  in
  img_of_prices prices

(** Create a compact status indicator with icon and text *)
let create_status_indicator ~icon ~text ~style =
  I.hcat [icon; I.string style " "; I.string style text]

(** Format percentage change with appropriate styling *)
let format_percentage_change pct =
  let style = if pct > 2.0 then style_warning_text  (* High volatility *)
              else if pct > 1.0 then style_neutral_text  (* Medium volatility *)
              else style_success_text  (* Low volatility *)
  in
  I.string style (Printf.sprintf "%.2f%%" pct)

(** Format quantity with intelligent trailing zero trimming *)
let format_quantity qty =
  let qty_str = Printf.sprintf "%.8f" qty in
  (* Trim trailing zeros after decimal point *)
  let trimmed = if String.contains qty_str '.' then
    let rec trim_zeros s =
      let len = String.length s in
      if len > 0 && s.[len-1] = '0' then
        let new_s = String.sub s 0 (len-1) in
        if String.contains new_s '.' then trim_zeros new_s else new_s
      else s
    in
    trim_zeros qty_str
  else
    qty_str
  in
  (* Ensure we don't trim the decimal point if there are no decimal digits *)
  if String.ends_with ~suffix:"." trimmed then
    String.sub trimmed 0 (String.length trimmed - 1)
  else
    trimmed

(** Render ASCII price ladder visualization showing order distribution around current price *)
let price_ladder ~ladder_width current_price buy_orders sell_orders frame =
  let ladder = Array.make ladder_width (I.string A.empty " ") in
  let min_display_price, max_display_price = 
    let all_relevant_prices = current_price :: (List.map fst buy_orders) @ (List.map fst sell_orders) in
    match all_relevant_prices with
    | [] -> (current_price, current_price) 
    | p::ps -> List.fold_left (fun (min_acc, max_acc) pr -> (min min_acc pr, max max_acc pr)) (p,p) ps
  in
  let price_to_index price =
    let price_range = max_display_price -. min_display_price in
    if price_range = 0. then
      if ladder_width > 0 then ladder_width / 2 else 0 
    else
      let scale_factor = if ladder_width > 0 then float_of_int (ladder_width - 1) else 0.0 in
      let idx = int_of_float (((price -. min_display_price) /. price_range) *. scale_factor) in
      if ladder_width > 0 then max 0 (min (ladder_width - 1) idx) else 0 
  in
  List.iter (fun (price,_) -> 
    let idx = price_to_index price in
    if idx < ladder_width && idx >= 0 then ladder.(idx) <- spr_buy_order
  ) buy_orders;
  List.iter (fun (price,_) -> 
    let idx = price_to_index price in
    if idx < ladder_width && idx >= 0 then ladder.(idx) <- spr_sell_order
  ) sell_orders;
  let current_idx = price_to_index current_price in
  if current_idx < ladder_width && current_idx >= 0 then ladder.(current_idx) <- spr_price_now frame;
  I.hcat (Array.to_list ladder)

(** Format price with appropriate decimal precision based on asset trading rules *)
let format_price asset price =
  match Kraken.Kraken_incoming_data.get_price_precision asset with
  | Some prec -> Printf.sprintf "%.*f" prec price
  | None -> Printf.sprintf "%.2f" price

let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n-1) xs

let get_strategy_info asset =
  let open State in
  match get_global_strategy_assignment asset with
  | Some Grid -> (Grid, "GRID")
  | Some Orderbook -> (Orderbook, "GMM")
  | Some Arbitrage -> (Arbitrage, "ARB")
  | Some Monitor -> (Monitor, "MONITOR")
  | Some VMM -> (VMM, "VMM")
  | None ->
      (* Fallback: check if there are actual orders for this asset *)
      let open_orders = Kraken.Kraken_incoming_data.get_all_open_orders () in
      let has_orders = Hashtbl.fold (fun _ order acc ->
        acc || String.equal order.Kraken.Kraken_common_types.order_symbol asset
      ) open_orders false in
      if has_orders then (Arbitrage, "ARB") else (Monitor, "MONITOR")

let style_of_strategy = function
  | State.Grid -> style_strat_grid
  | State.Orderbook -> style_strat_gmm
  | State.Arbitrage -> style_strat_arb
  | State.VMM -> style_strat_vmm
  | State.Monitor -> style_strat_monitor


(** Portfolio balance information for a single asset *)
type balance_info = {
  asset: string;                    (** Asset symbol (e.g., "BTC", "ETH") *)
  total_balance: float;             (** Total holdings: spot + earn + liquid staking *)
  reconciliation_balance: float;    (** Spot + earn balances for reconciliation *)
  total_value_usd: float;           (** Current USD value of total_balance *)
  accumulated_balance: float;       (** Balance excluding pending sell orders *)
  accumulated_value_usd: float;     (** USD value of accumulated_balance *)
  unrealized_value_usd: float;      (** Unrealized P&L from pending orders *)
}

(** Dashboard UI state for rendering and user interaction *)
type dashboard_state = {
  show_logs: bool;                  (** Whether to display system logs panel *)
  frame: int;                       (** Animation frame counter for blinking effects *)
  balances: balance_info list;      (** Current portfolio balances *)
  show_balances: bool;              (** Whether to display balances panel *)
  show_assets: bool;                (** Whether to display assets section *)
  active_assets: string list;       (** Cached list of actively traded assets *)
  order_data: (string, (float * float) list * (float * float) list) Hashtbl.t;
                                  (** Cached order book data: asset -> (buy_orders, sell_orders) *)
  term_dimensions: int * int;       (** Cached terminal dimensions (height, width) *)
  cached_logs: string list;         (** Recent system logs to prevent excessive polling *)
  last_log_count: int;              (** Previous log count to detect new entries *)
}

let initial_state = {
  show_logs = true;
  frame = 0;
  balances = [];
  show_balances = true;
  show_assets = true;
  active_assets = [];
  order_data = Hashtbl.create 16;
  term_dimensions = (24, 80);  (* Default dimensions *)
  cached_logs = [];
  last_log_count = 0;
}

let rec intersperse sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse sep xs

(** Compact asset row for two-column layout - much more condensed *)
let compact_row_of_asset asset _frame state =
  let _, term_width = state.term_dimensions in
  let current_price_opt = Stats.get_price asset in
  let all_buy_orders_for_symbol, all_sell_orders_for_symbol =
    match Hashtbl.find_opt state.order_data asset with
    | Some (buy_orders, sell_orders) -> (buy_orders, sell_orders)
    | None -> ([], [])
  in

  let strategy, strategy_name = get_strategy_info asset in
  let order_count = List.length all_buy_orders_for_symbol + List.length all_sell_orders_for_symbol in

  (* Compact single-line display *)
  let content_width = term_width - 2 in  (* Account for borders *)

  match current_price_opt with
  | Some cp_val ->
      let current_f = Float.of_string (Primitives.Price.to_string cp_val) in

      let current_price_img = I.string (style_current_price_text ++ A.st A.bold)
        (format_price asset current_f) in
      let asset_name = Printf.sprintf "%-6s" asset in
      let _price_str = Printf.sprintf "$%.2f" current_f in
      let orders_str = string_of_int order_count in
      let strat_str = strategy_name in

      (* Get the best pending buy and sell prices with distance indicators *)
      let pending_buy_price = if all_buy_orders_for_symbol <> [] then
        let best_bid = List.fold_left (fun acc (p, _) ->
          match acc with None -> Some p | Some best -> Some (max best p)
        ) None all_buy_orders_for_symbol in
        match best_bid with
        | Some price ->
            let distance = current_f -. price in
            let distance_pct = if current_f <> 0.0 then (distance /. current_f) *. 100.0 else 0.0 in
            let (distance_str, distance_style) = if abs_float distance_pct < 0.01 then
              (Printf.sprintf "(0%%)", style_neutral_text)
            else if distance_pct > 0.0 then
              (Printf.sprintf "(-%.2f%%)" distance_pct, style_loss_text)  (* Red for positive distance - need to go down to execute buy *)
            else
              (Printf.sprintf "(+%.2f%%)" (-.distance_pct), style_profit_text) in  (* Green for negative distance - need to go up to execute buy *)
            I.hcat [
              I.string style_buy_order_text (format_price asset price);
              I.string distance_style distance_str
            ]
        | None -> I.string style_neutral_text "--"
      else
        I.string style_neutral_text "--"
      in

      let closest_sell_price = if all_sell_orders_for_symbol <> [] then
        let best_ask = List.fold_left (fun acc (p, _) ->
          match acc with None -> Some p | Some best -> Some (min best p)
        ) None all_sell_orders_for_symbol in
        match best_ask with
        | Some price ->
            let distance = price -. current_f in
            let distance_pct = if current_f <> 0.0 then (distance /. current_f) *. 100.0 else 0.0 in
            let (distance_str, distance_style) = if abs_float distance_pct < 0.01 then
              (Printf.sprintf "(0%%)", style_neutral_text)
            else if distance_pct < 0.0 then
              (Printf.sprintf "(-%.2f%%)" (-.distance_pct), style_loss_text)  (* Red for negative distance - need to go down to execute sell *)
            else
              (Printf.sprintf "(+%.2f%%)" distance_pct, style_profit_text) in  (* Green for positive distance - need to go up to execute sell *)
            I.hcat [
              I.string style_sell_order_text (format_price asset price);
              I.string distance_style distance_str
            ]
        | None -> I.string style_neutral_text "--"
      else
        I.string style_neutral_text "--"
      in

      let available_content_width = content_width - 4 in  (* Reserve space for separators *)

      (* Dynamic spacing based on available width *)
      let spacing = if available_content_width > 80 then " │ " else " │" in
      let slash_spacing = if available_content_width > 80 then " / " else " /" in

      let combined_content = I.hcat [
        I.string style_asset_name asset_name;
        I.string style_neutral_text spacing;
        pending_buy_price;
        I.string style_neutral_text slash_spacing;
        current_price_img;
        I.string style_neutral_text slash_spacing;
        closest_sell_price;
        I.string style_neutral_text spacing;
        I.string style_logs_accent_text orders_str;
        I.string style_neutral_text spacing;
        I.string (style_of_strategy strategy) strat_str
      ] in
      I.hcat [
        I.string style_header_border "┃";
        combined_content;
        I.void (content_width - I.width combined_content) 1;
        I.string style_header_border "┃"
      ]
  | None ->
      let available_content_width = content_width - 4 in  (* Reserve space for separators *)
      let spacing = if available_content_width > 80 then " │ " else " │" in

      let no_data_content = I.hcat [
        I.string style_warning_text (Printf.sprintf "%-6s" asset);
        I.string style_neutral_text spacing;
        I.string style_warning_text "--/--";
        I.string style_neutral_text spacing;
        I.string style_warning_text "0";
        I.string style_neutral_text spacing;
        I.string style_warning_text "--"
      ] in
      I.hcat [
        I.string style_header_border "┃";
        no_data_content;
        I.void (content_width - I.width no_data_content) 1;
        I.string style_header_border "┃"
      ]

(** Enhanced modern asset row with compact, information-dense display *)
let row_of_asset asset frame state =
  let _, term_width = state.term_dimensions in
  let current_price_opt = Stats.get_price asset in
  let all_buy_orders_for_symbol, all_sell_orders_for_symbol =
    match Hashtbl.find_opt state.order_data asset with
    | Some (buy_orders, sell_orders) -> (buy_orders, sell_orders)
    | None -> ([], [])
  in

  let strategy, strategy_name = get_strategy_info asset in
  let order_count = List.length all_buy_orders_for_symbol + List.length all_sell_orders_for_symbol in

  (* Calculate price statistics and distance to furthest order *)
  let current_price, volatility_pct, price_trend =
    match current_price_opt with
    | Some cp_val ->
        let current_f = Float.of_string (Primitives.Price.to_string cp_val) in
        let all_prices = List.map fst all_buy_orders_for_symbol @ List.map fst all_sell_orders_for_symbol in
        let volatility = if all_prices = [] then 0.0 else
          let min_price = List.fold_left min (List.hd all_prices) all_prices in
          let max_price = List.fold_left max (List.hd all_prices) all_prices in
          (max_price -. min_price) /. current_f *. 100.0 in
        update_price_history asset current_f;
        (current_f, volatility, if volatility > 2.0 then spr_warning else if volatility > 1.0 then spr_neutral else spr_success_text)
    | None -> (0.0, 0.0, spr_neutral)
  in

  (* Compact header with asset info and strategy *)
  let asset_header = I.hcat [
    I.string style_asset_name (Printf.sprintf "%-6s" asset);
    spr_separator;
    I.string (style_of_strategy strategy) strategy_name;
    spr_separator;
    create_status_indicator ~icon:spr_active ~text:(string_of_int order_count) ~style:style_logs_accent_text;
    spr_separator;
    price_trend;
    format_percentage_change volatility_pct;
  ] in

  (* Enhanced price display with current price and order book summary *)
  let price_display = match current_price_opt with
    | Some cp_val ->
        let current_f = Float.of_string (Primitives.Price.to_string cp_val) in
        let buy_summary = if all_buy_orders_for_symbol <> [] then
          let total_buy_qty = List.fold_left (fun acc (_, qty) -> acc +. qty) 0.0 all_buy_orders_for_symbol in
          let best_bid = List.fold_left (fun acc (p, _) ->
            match acc with None -> Some p | Some best -> Some (max best p)
          ) None all_buy_orders_for_symbol in
          match best_bid with
          | Some bid_price ->
              let distance = current_f -. bid_price in
              let distance_pct = if current_f <> 0.0 then (distance /. current_f) *. 100.0 else 0.0 in
              let (distance_str, distance_style) = if abs_float distance_pct < 0.01 then
                (Printf.sprintf "(0%%)", style_neutral_text)
              else if distance_pct > 0.0 then
                (Printf.sprintf "(-%.2f%%)" distance_pct, style_loss_text)  (* Red for positive distance - need to go down to execute buy *)
              else
                (Printf.sprintf "(+%.2f%%)" (-.distance_pct), style_profit_text) in  (* Green for negative distance - need to go up to execute buy *)
              I.hcat [
                spr_buy_order;
                I.string style_buy_order_text (Printf.sprintf "%.4f" bid_price);
                I.string style_neutral_text (Printf.sprintf "(%s)" (format_quantity total_buy_qty));
                I.string distance_style distance_str
              ]
          | None -> I.string style_neutral_text "No bids"
        else
          I.string style_neutral_text "No bids"
        in

        let sell_summary = if all_sell_orders_for_symbol <> [] then
          let total_sell_qty = List.fold_left (fun acc (_, qty) -> acc +. qty) 0.0 all_sell_orders_for_symbol in
          let best_ask = List.fold_left (fun acc (p, _) ->
            match acc with None -> Some p | Some best -> Some (min best p)
          ) None all_sell_orders_for_symbol in
          match best_ask with
          | Some ask_price ->
              let distance = ask_price -. current_f in
              let distance_pct = if current_f <> 0.0 then (distance /. current_f) *. 100.0 else 0.0 in
              let (distance_str, distance_style) = if abs_float distance_pct < 0.01 then
                (Printf.sprintf "(0%%)", style_neutral_text)
              else if distance_pct < 0.0 then
                (Printf.sprintf "(-%.2f%%)" (-.distance_pct), style_loss_text)  (* Red for negative distance - need to go down to execute sell *)
              else
                (Printf.sprintf "(+%.2f%%)" distance_pct, style_profit_text) in  (* Green for positive distance - need to go up to execute sell *)
              I.hcat [
                spr_sell_order;
                I.string style_sell_order_text (Printf.sprintf "%.4f" ask_price);
                I.string style_neutral_text (Printf.sprintf "(%s)" (format_quantity total_sell_qty));
                I.string distance_style distance_str
              ]
          | None -> I.string style_neutral_text "No asks"
        else
          I.string style_neutral_text "No asks"
        in

        let current_price_img = I.string (style_current_price_text ++ A.st A.bold)
          (format_price asset current_f) in

        I.vcat [
          I.hcat [spr_bullet; I.string style_neutral_text " Price: "; current_price_img];
          I.hcat [spr_bullet; I.string style_neutral_text " Bids:  "; buy_summary];
          I.hcat [spr_bullet; I.string style_neutral_text " Asks:  "; sell_summary]
        ]
    | None ->
        I.string style_warning_text "No price data available"
  in

  (* Create a compact order book visualization *)
  let order_book_viz = match current_price_opt with
    | Some _ ->
        let buy_levels = List.length all_buy_orders_for_symbol in
        let sell_levels = List.length all_sell_orders_for_symbol in
        let total_levels = max 1 (buy_levels + sell_levels) in

        (* Create a compact horizontal bar showing order distribution *)
        let bar_width = min 20 (term_width / 4) in
        let buy_ratio = if total_levels > 0 then float_of_int buy_levels /. float_of_int total_levels else 0.5 in
        let progress_bar = create_progress_bar
          ~width:bar_width
          ~filled:buy_ratio
          ~style_full:style_buy_order_text
          ~style_empty:style_sell_order_text in

        I.hcat [
          spr_bullet;
          I.string style_neutral_text " Orderbook: ";
          progress_bar;
          I.string style_neutral_text (Printf.sprintf " %d/%d" buy_levels sell_levels)
        ]
    | None ->
        I.string style_neutral_text ""
  in

  let content_width = term_width - 2 in
  let left_section = I.vcat [
    asset_header;
    price_display;
    order_book_viz
  ] in

  let right_section = match current_price_opt with
    | Some _ ->
        let available = content_width - I.width left_section - 4 in
        let ladder_width = min 25 available in
        let ladder_img =
          if ladder_width > 10 then
            price_ladder ~ladder_width current_price all_buy_orders_for_symbol all_sell_orders_for_symbol frame
          else I.empty
        in
        let history = get_price_history asset in
        let trend_width = max 20 (available - max 0 (I.width ladder_img) - 3) in
        let trend_img =
          if trend_width > 10 && history <> [] then (
            let line = I.hcat [ I.string style_neutral_text " "; create_block_sparkline ~width:trend_width ~prices:history ] in
            let spacer = I.void (I.width line) 1 in
            I.vcat [ line; spacer ]
          ) else I.empty
        in
        I.vcat [ladder_img; trend_img]
    | None -> I.empty
  in

  let combined_content = if I.width right_section > 0 then
    I.hcat [left_section; spr_separator; right_section]
  else
    left_section in

  (* Create modern border styling *)
  I.hcat [
    I.string style_header_border "┃";
    combined_content;
    I.void (content_width - I.width combined_content) 1;
    I.string style_header_border "┃"
  ]

(** Get all assets currently being traded or configured for strategies *)
let get_all_active_assets () =
  let orderbook_assets = Kraken.Kraken_incoming_data.get_all_open_orders ()
    |> Hashtbl.to_seq_values
    |> List.of_seq
    |> List.map (fun order -> order.Kraken.Kraken_common_types.order_symbol)
    |> List.sort_uniq String.compare
  in
  let configured_symbols = State.get_all_symbols() in
  let all_assets = orderbook_assets @ configured_symbols
    |> List.sort_uniq String.compare
  in
  all_assets

let find_index pred lst =
  let rec aux i = function
    | [] -> None
    | x :: xs -> if pred x then Some i else aux (i + 1) xs
  in
  aux 0 lst

let compare_assets active_assets a b =
  let get_priority asset =
    match find_index ((=) asset) active_assets with
    | Some idx -> idx
    | None -> List.length active_assets
  in
  compare (get_priority a) (get_priority b)

(** Fetch and calculate portfolio balances from Kraken API, including P&L metrics *)
let get_balance_info () : balance_info list Lwt.t =
  Kraken.Kraken_balances.wait_for_balances () >>= fun (spot_balances, earn_balances, liquid_balances, _) ->
  let open_orders = Kraken.Kraken_incoming_data.get_all_open_orders () in

  let all_assets =
    let keyset = ref StringSet.empty in
    Hashtbl.iter (fun k _ -> keyset := StringSet.add k !keyset) spot_balances;
    Hashtbl.iter (fun k _ -> keyset := StringSet.add k !keyset) earn_balances;
    Hashtbl.iter (fun k _ -> keyset := StringSet.add k !keyset) liquid_balances;
    StringSet.elements !keyset
  in

  let balance_info_list_lwt = Lwt_list.fold_left_s (fun acc asset ->
    let price_usd_lwt =
      if asset = "USD" then Lwt.return_some 1.0
      else
        let pair = asset ^ "/USD" in
        match Stats.get_price pair with
        | Some p -> Lwt.return_some (Float.of_string (Primitives.Price.to_string p))
        | None ->
            let pair_usdt = asset ^ "/USDT" in
            match Stats.get_price pair_usdt with
            | Some p -> Lwt.return_some (Float.of_string (Primitives.Price.to_string p))
            | None ->
                (* Fallback for stablecoins: use 1.0 if no market data available *)
                if is_stablecoin asset then Lwt.return_some 1.0
                else Lwt.return_none
    in

    price_usd_lwt >|= function
    | None -> acc
    | Some price_usd ->
        let spot_balance = Hashtbl.find_opt spot_balances asset |> Option.value ~default:0.0 in
        let earn_balance = Hashtbl.find_opt earn_balances asset |> Option.value ~default:0.0 in
        let liquid_balance = Hashtbl.find_opt liquid_balances asset |> Option.value ~default:0.0 in


        let total_balance = spot_balance +. earn_balance +. liquid_balance in
        if total_balance < 0.000001 then acc else

        let reconciliation_balance = spot_balance +. earn_balance in
        let total_value_usd = total_balance *. price_usd in

        let qty_on_sell_orders =
          let open_orders = Kraken.Kraken_incoming_data.get_all_open_orders () in
          Hashtbl.fold (fun _ (order : Kraken.Kraken_common_types.order) acc ->
            let base_of_order =
              match String.split_on_char '/' order.order_symbol with
              | base :: _ -> Some base
              | _ -> None
            in
            match base_of_order with
            | Some base when base = asset && order.side = Some Core.Sell ->
                acc +. order.qty
            | _ ->
                acc
          ) open_orders 0.0
        in

        (* Accumulated balance is total balance less assets held in open sell orders *)
        let raw_accumulated_balance = total_balance -. qty_on_sell_orders in

        if raw_accumulated_balance < 0.0 then
          Lwt.async (fun () ->
            let msg =
              (Printf.sprintf "%s negative accumulated value detected! total_balance=%.8f, qty_on_sell_orders=%.8f, raw_accumulated_balance=%.8f"
                asset total_balance qty_on_sell_orders raw_accumulated_balance)
            in
            Lwt_log_core.error ~section:(Lwt_log_core.Section.make "dashboard") msg
          );

        let accumulated_balance = raw_accumulated_balance in

        let pnl_accumulated_balance = accumulated_balance in
        let accumulated_value_usd =
          if asset = "USD" || asset = "USDG" || asset = "USDC" || asset = "USDR" || asset = "EURR" then 0.0
          else accumulated_balance *. price_usd in


        (* Calculate unrealized value based on pending sell orders *)
        let unrealized_value_usd =
          let open_sell_orders =
            Hashtbl.fold (fun _ (order : Kraken.Kraken_common_types.order) acc ->
              let base_of_order =
                match String.split_on_char '/' order.order_symbol with
                | base :: _ -> Some base
                | _ -> None
              in
              match base_of_order with
              | Some base when base = asset && order.side = Some Core.Sell ->
                  order :: acc
              | _ ->
                  acc
            ) open_orders []
          in
          let unrealized_value_on_orders = List.fold_left (fun acc (o:Kraken.Kraken_common_types.order) -> acc +. (o.qty *. o.limit_price)) 0.0 open_sell_orders in
          let highest_sell_price = List.fold_left (fun max_p (o:Kraken.Kraken_common_types.order) -> max max_p o.limit_price) 0.0 open_sell_orders in
          let unrealized_value_accumulated =
            if open_sell_orders <> [] then
              pnl_accumulated_balance *. highest_sell_price
            else
              (* If no sell orders, unrealized value of accumulated balance is just its current accumulated value *)
              accumulated_value_usd
          in
          let total_unrealized =
            if open_sell_orders <> [] then
              unrealized_value_on_orders +. unrealized_value_accumulated
            else
              unrealized_value_accumulated
          in

          (* Debug unrealized value calculation *)
          let _ = if asset = "EURR" then
            Lwt.ignore_result (
              Lwt_log_core.debug ~section:(Lwt_log_core.Section.make "dashboard")
                (Printf.sprintf "EURR UNREALIZED DEBUG: orders_val=%.8f, accum_val=%.8f, total=%.8f, highest_price=%.4f, accum_balance=%.8f"
                  unrealized_value_on_orders unrealized_value_accumulated total_unrealized highest_sell_price pnl_accumulated_balance)
            )
          in

          if asset = "USD" && total_unrealized = 0.0 then total_value_usd else total_unrealized
        in

        let info = {
          asset;
          total_balance;
          reconciliation_balance;
          total_value_usd;
          accumulated_balance = pnl_accumulated_balance;
          accumulated_value_usd;
          unrealized_value_usd;
        } in
        info :: acc
  ) [] all_assets
  in

  balance_info_list_lwt >|= fun balance_info_list ->
  List.sort (fun b1 b2 -> compare b1.asset b2.asset) balance_info_list

(** Render tabular portfolio balances with current values and performance metrics *)
let render_balances_section (balances: balance_info list) term_width =
  let content_width = term_width - 2 in
  if balances = [] then I.empty
  else
    let horiz_border_char_str_for_balances = "\u{2500}" in
    let create_horizontal_fill width char_str =
      String.concat "" (List.init (max 0 width) (fun _ -> char_str))
    in

    (* Define minimum widths *)
    let min_asset = 8 in
    let min_total = 14 in
    let min_value = 14 in
    let min_accum = 18 in
    let min_unreal = 18 in

    let num_borders = 4 in  (* 4 │ separators *)


    let available_for_columns = content_width - num_borders in
    let min_for_columns = min_asset + min_total + min_value + min_accum + min_unreal in
    let extra_space = max 0 (available_for_columns - min_for_columns) in
    let num_expandable = 5 in  (* asset, total, value, accum, unreal *)
    let extra_per = extra_space / num_expandable in
    let extra_rem = extra_space mod num_expandable in

    let asset_w = min_asset + extra_per + (if 1 <= extra_rem then 1 else 0) in
    let total_w = min_total + extra_per + (if 2 <= extra_rem then 1 else 0) in
    let value_w = min_value + extra_per + (if 3 <= extra_rem then 1 else 0) in
    let accum_w = min_accum + extra_per + (if 4 <= extra_rem then 1 else 0) in
    let unreal_w = min_unreal + extra_per + (if 5 <= extra_rem then 1 else 0) in

    (* Ensure minimums if screen too small *)
    let asset_w = max min_asset asset_w in
    let total_w = max min_total total_w in
    let value_w = max min_value value_w in
    let accum_w = max min_accum accum_w in
    let unreal_w = max min_unreal unreal_w in

    (* Header *)
    let header = I.hcat [
      I.string style_header_border "┃";
      I.string (style_highlight_text ++ A.st A.bold) (Printf.sprintf "%-*s" asset_w "Asset");
      I.string style_header_border "│";
      I.string style_primary_text (Printf.sprintf "%*s" total_w "Balance");
      I.string style_header_border "│";
      I.string style_primary_text (Printf.sprintf "%*s" value_w "Current Value");
      I.string style_header_border "│";
      I.string style_primary_text (Printf.sprintf "%*s" accum_w "Accumulated Value");
      I.string style_header_border "│";
      I.string style_primary_text (Printf.sprintf "%*s" unreal_w "Unrealized Value");
      I.string style_header_border "┃";
    ] in

    let rows = List.map (fun info ->
      let value_style = if info.total_value_usd > 0.0 then style_success_text
                        else if info.total_value_usd < 0.0 then style_loss_text
                        else style_neutral_text in
      let accumulated_style =
        if abs_float info.accumulated_value_usd < 0.01 then style_neutral_text
        else if info.accumulated_value_usd > 0.0 then style_success_text
        else style_loss_text in
      let unrealized_display =
        let dollar_str = Printf.sprintf "$%.2f" info.unrealized_value_usd in
        if info.total_value_usd = 0.0 then
          I.string style_neutral_text (Printf.sprintf "%*s" unreal_w dollar_str)
        else
          let perc = ((info.unrealized_value_usd -. info.total_value_usd) /. info.total_value_usd) *. 100.0 in
          let perc_style = if abs_float perc < 0.01 then style_neutral_text else if perc >= 0.0 then style_profit_text else style_loss_text in
          let perc_str = Printf.sprintf "%+.2f%%" perc in
          let dollar_len = String.length dollar_str in
          let perc_len = String.length perc_str in
          let available_for_perc = max 0 (unreal_w - dollar_len - 1) in (* -1 for space *)
          if available_for_perc >= perc_len then
            (* Both fit: show "$X.XX (+Y.YY%)" *)
            I.hcat [
              I.string style_primary_text dollar_str;
              I.string style_neutral_text " ";
              I.string perc_style (Printf.sprintf "%*s" available_for_perc perc_str)
            ]
          else
            (* Doesn't fit, show colored dollar amount *)
            let display_style = if perc >= 0.0 then style_profit_text else style_loss_text in
            I.string display_style (Printf.sprintf "%*s" unreal_w dollar_str)
      in
      I.hcat [
        I.string style_header_border "┃";
        I.string style_asset_name (Printf.sprintf "%-*s" asset_w info.asset);
        I.string style_header_border "│";
        I.string style_primary_text (Printf.sprintf "%*s" total_w (Printf.sprintf "%s %s" (format_quantity info.total_balance) info.asset));
        I.string style_header_border "│";
        I.string value_style (Printf.sprintf "%*s" value_w (Printf.sprintf "$%.2f" info.total_value_usd));
        I.string style_header_border "│";
        I.string accumulated_style (Printf.sprintf "%*s" accum_w (Printf.sprintf "$%.2f" info.accumulated_value_usd));
        I.string style_header_border "│";
        unrealized_display;
        I.string style_header_border "┃";
      ]
    ) balances in

    let total_portfolio_value = List.fold_left (fun acc b -> acc +. b.total_value_usd) 0.0 balances in
    let total_accumulated_value = List.fold_left (fun acc b -> acc +. b.accumulated_value_usd) 0.0 balances in
    let total_unrealized_value = List.fold_left (fun acc b -> acc +. b.unrealized_value_usd) 0.0 balances in

    let total_unrealized_display =
      let dollar_str = Printf.sprintf "$%.2f" total_unrealized_value in
      if total_portfolio_value = 0.0 then
        I.string (style_neutral_text ++ A.st A.bold) (Printf.sprintf "%*s" unreal_w dollar_str)
      else
        let perc = ((total_unrealized_value -. total_portfolio_value) /. total_portfolio_value) *. 100.0 in
        let perc_style = if abs_float perc < 0.01 then (style_neutral_text ++ A.st A.bold) else if perc >= 0.0 then (style_highlight_text ++ A.st A.bold) else (style_loss_text ++ A.st A.bold) in
        let perc_str = Printf.sprintf "%+.2f%%" perc in
        let dollar_len = String.length dollar_str in
        let perc_len = String.length perc_str in
        let available_for_perc = max 0 (unreal_w - dollar_len - 1) in (* -1 for space *)
        if available_for_perc >= perc_len then
          (* Both fit: show "$X.XX (+Y.YY%)" *)
          I.hcat [
            I.string (style_highlight_text ++ A.st A.bold) dollar_str;
            I.string (style_neutral_text ++ A.st A.bold) " ";
            I.string perc_style (Printf.sprintf "%*s" available_for_perc perc_str)
          ]
        else
          (* Doesn't fit, show colored dollar amount *)
          let display_style = if perc >= 0.0 then (style_highlight_text ++ A.st A.bold) else (style_loss_text ++ A.st A.bold) in
          I.string display_style (Printf.sprintf "%*s" unreal_w dollar_str)
    in
    let total_value_style = if total_portfolio_value > 0.0 then (style_success_text ++ A.st A.bold)
                            else if total_portfolio_value < 0.0 then (style_loss_text ++ A.st A.bold)
                            else (style_neutral_text ++ A.st A.bold) in
    let total_accumulated_style =
      if abs_float total_accumulated_value < 0.01 then (style_neutral_text ++ A.st A.bold)
      else if total_accumulated_value > 0.0 then (style_success_text ++ A.st A.bold)
      else (style_loss_text ++ A.st A.bold) in
    let total_row =
      I.hcat [
        I.string style_header_border "┃";
        I.string (style_highlight_text ++ A.st A.bold) (Printf.sprintf "%-*s" asset_w "TOTAL");
        I.string style_header_border "│";
        I.string style_neutral_text (Printf.sprintf "%*s" total_w "");
        I.string style_header_border "│";
        I.string total_value_style (Printf.sprintf "%*s" value_w (Printf.sprintf "$%.2f" total_portfolio_value));
        I.string style_header_border "│";
        I.string total_accumulated_style (Printf.sprintf "%*s" accum_w (Printf.sprintf "$%.2f" total_accumulated_value));
        I.string style_header_border "│";
        total_unrealized_display;
        I.string style_header_border "┃";
      ] in

    let sep = I.string style_header_border (
      "┣" ^ create_horizontal_fill asset_w horiz_border_char_str_for_balances ^
      "┿" ^ create_horizontal_fill total_w horiz_border_char_str_for_balances ^
      "┿" ^ create_horizontal_fill value_w horiz_border_char_str_for_balances ^
      "┿" ^ create_horizontal_fill accum_w horiz_border_char_str_for_balances ^
      "┿" ^ create_horizontal_fill unreal_w horiz_border_char_str_for_balances ^
      "┫"
    ) in
    let top_border = I.string style_header_border ("┏" ^ (create_horizontal_fill (term_width - 2) horiz_border_char_str_for_balances) ^ "┓") in
    let bottom_border = I.string style_header_border ("┗" ^ (create_horizontal_fill (term_width - 2) horiz_border_char_str_for_balances) ^ "┛") in

    I.vcat ([top_border; header; sep] @ intersperse sep rows @ [sep; total_row; bottom_border])

(** Main dashboard rendering function - composes all UI sections into final display *)
let render state =
  let open I in
  let term_height, term_width = state.term_dimensions in
  let content_width = term_width - 4 in

  let horiz_border_char_str = "\u{2501}" in
  let create_horizontal_fill width char_str =
    String.concat "" (List.init (max 0 width) (fun _ -> char_str))
  in

  let asset_rows = List.map (fun asset -> row_of_asset asset state.frame state) state.active_assets in
  let compact_asset_rows = List.map (fun asset -> compact_row_of_asset asset state.frame state) state.active_assets in
  let total_assets = List.length asset_rows in

  let dio_label_text = " Dio " in
  let dio_label_style = style_highlight_text ++ A.st A.bold in
  let dio_label_img = I.string dio_label_style dio_label_text in
  let performance_indicator = I.string style_success_text (Printf.sprintf "[%d assets]" total_assets) in
  let left_label = I.hcat [dio_label_img; performance_indicator] in

  let key_bracket_style = style_keybinding_bracket in
  let key_text_style = style_keybinding_text in
  let text_style = style_header_info_text in
  let key_bindings_img = I.hcat [
      I.string key_bracket_style "["; I.string key_text_style "B"; I.string key_bracket_style "]";
      I.string text_style "alance ";
      I.string key_bracket_style "│";
      I.string key_bracket_style " ["; I.string key_text_style "A"; I.string key_bracket_style "]";
      I.string text_style "sset ";
      I.string key_bracket_style "│";
      I.string key_bracket_style " ["; I.string key_text_style "L"; I.string key_bracket_style "]";
      I.string text_style "og ";
      I.string key_bracket_style "│";
      I.string key_bracket_style " ["; I.string key_text_style "Q"; I.string key_bracket_style "]";
      I.string text_style "uit";
    ] in

  let runtime_str = fmt_runtime Stats.start_ts in
  let (status_indicator, status_text) = (I.string style_success_text ">", "LIVE") in
  let runtime_img = I.hcat [
    I.string (style_logs_accent_text ++ A.st A.bold) runtime_str;
    I.string style_neutral_text " ";
    status_indicator;
    I.string style_neutral_text (" " ^ status_text)
  ] in




  let header_components = [left_label; runtime_img; key_bindings_img] in
  let separator = I.string style_header_border " │ " in
  let header_content = I.hcat (intersperse separator header_components) in
  let inner_width = term_width - 2 in
  let content_w = I.width header_content in
  let fill_w = max 0 (inner_width - content_w) in
  let new_header = I.hcat [
    I.string style_header_border "┏";
    header_content;
    I.string style_header_border (create_horizontal_fill fill_w horiz_border_char_str);
    I.string style_header_border "┓";
  ] in

  let top_asset_box_line = 
    I.string style_header_border (Printf.sprintf "\u{250F}%s\u{2513}" (create_horizontal_fill (term_width - 2) horiz_border_char_str))
  in
  let inter_asset_box_line =
    I.string style_header_border (Printf.sprintf "\u{2523}%s\u{252B}" (create_horizontal_fill (term_width - 2) horiz_border_char_str))
  in
  let bottom_asset_box_line = 
    match asset_rows with
    | [] -> I.empty
    | _ -> I.string style_header_border (Printf.sprintf "\u{2517}%s\u{251B}" (create_horizontal_fill (term_width - 2) horiz_border_char_str))
  in
  let asset_rows_section_with_boxing =
    if not state.show_assets then
      I.empty
    else
      match asset_rows with
      | [] -> I.empty
      | _ ->
          (* Use compact layout when both balances and assets are shown *)
          let should_use_compact = state.show_balances && state.balances <> [] in
          if should_use_compact then
            (* Use compact asset rows for single-column layout when both sections are shown *)
            let interspersed_rows = intersperse inter_asset_box_line compact_asset_rows in
            I.vcat ([top_asset_box_line] @ interspersed_rows @ [bottom_asset_box_line])
          else
            (* Use single-column layout when only assets are shown or balances are hidden *)
            let interspersed_rows = intersperse inter_asset_box_line asset_rows in
            I.vcat ([top_asset_box_line] @ interspersed_rows @ [bottom_asset_box_line])
  in

  let balances_section_image =
    if state.show_balances then
      render_balances_section state.balances term_width
    else
      I.empty
  in
  let balances_section_height = height balances_section_image in

  let new_asset_rows_height = height asset_rows_section_with_boxing in

  let other_height = 1 (* header *) + 1 (* void after header *) +
                   (if state.show_balances && state.balances <> [] then balances_section_height + 1 (* void after balances *) else  0) +
                   (if state.show_assets then 1 (* void before assets *) + new_asset_rows_height + 1 (* void after assets *) else 0) in
let logs_height =
  if state.show_logs then
    max 0 (term_height - other_height)
  else 0
in

let logs_section_image =
  if not state.show_logs || logs_height <= 0 then I.void 0 0
  else
    let logs_header_text = I.hcat [
      I.string (style_logs_accent_text ++ A.st A.bold) "System Logs ";
      I.string style_neutral_text "(";
      I.string style_success_text (string_of_int (List.length state.cached_logs));
      I.string style_neutral_text " entries)"
    ] in
    let logs_header_img =
      I.hcat [
        I.string style_header_border "┏━"; logs_header_text;
        I.string style_header_border (create_horizontal_fill (term_width - (width logs_header_text) - 3) horiz_border_char_str);
        I.string style_header_border "┓"
      ]
    in
    let logs_header_h = height logs_header_img in
    let num_log_lines_to_take = max 0 (logs_height - logs_header_h) in
    let log_messages =
      List.map (fun msg ->
        let re = Str.regexp "\\[\\([0-9:]+\\)\\]\\[\\([^|]+\\)|\\([^]]+\\)\\] \\(.*\\)" in
        if Str.string_match re msg 0 then
          let timestamp = Str.matched_group 1 msg in
          let section = Str.matched_group 2 msg in
          let level = Str.matched_group 3 msg in
          let message = Str.matched_group 4 msg in
          let style = style_of_log_level level in

          let bullet_img = I.string style "● " in
          let ts_img = I.string style (Printf.sprintf "[%s]" timestamp) in
          let section_level_img = I.string style (Printf.sprintf "[%s|%s] " section level) in
          let prefix_img = I.hcat [bullet_img; ts_img; section_level_img] in
          let prefix_width = I.width prefix_img in

          let message_img = I.string style_log_debug message in
          let final_img = I.hcat [prefix_img; message_img] in

          if I.width final_img > content_width then
            let available_width = content_width - prefix_width in
            if available_width > 0 then
              let first_line_msg = String.sub message 0 (min available_width (String.length message)) in
              let rest_of_msg = String.sub message (String.length first_line_msg) (String.length message - String.length first_line_msg) in
              let first_line_img = I.hcat [prefix_img; I.string style_log_debug first_line_msg] in

              let rec wrap_text remaining_text acc =
                if String.length remaining_text <= content_width then
                  (I.string style_log_debug remaining_text) :: acc
                else
                  let line = String.sub remaining_text 0 content_width in
                  let rest = String.sub remaining_text content_width (String.length remaining_text - content_width) in
                  wrap_text rest ((I.string style_log_debug line) :: acc)
              in
              let wrapped_lines = if String.length rest_of_msg > 0 then wrap_text rest_of_msg [] |> List.rev else [] in
              I.vcat (first_line_img :: wrapped_lines)
            else
              I.hsnap ~align:`Left content_width final_img
          else
            final_img
        else
          (* Fallback for messages that don't match the expected format *)
          let style = style_of_log_level "debug" in (* Default to debug style *)
          I.hcat [I.string style "● "; I.string style msg]
      ) (take num_log_lines_to_take state.cached_logs)
    in
    let logs_body_content = I.vcat log_messages in
    let logs_full_content = I.vcat [logs_header_img; logs_body_content] in
    let cropped_logs_content =
      if height logs_full_content > logs_height then 
        vsnap ~align:`Top logs_height logs_full_content 
      else 
        logs_full_content 
    in
    if width cropped_logs_content < content_width then
      I.hcat [cropped_logs_content; I.void (content_width - width cropped_logs_content) (height cropped_logs_content)]
    else
      cropped_logs_content
in
I.vcat [
  new_header;
  I.void term_width 1;
  balances_section_image;
  (if state.show_balances && state.balances <> [] then I.void term_width 1 else I.empty);
  (if state.show_assets then asset_rows_section_with_boxing else I.empty);
  (if state.show_assets then I.void term_width 1 else I.empty);
  logs_section_image;
  I.void term_width 1
]

(** Main dashboard application entry point - initializes terminal and starts event loops *)
let start ~on_quit:(on_quit: unit -> unit Lwt.t) () : Notty_lwt.Term.t =
  let term_instance = Notty_lwt.Term.create ~mouse:false () in
  let state = ref initial_state in
  let handle_event = function
    | `Key (`ASCII 'l', []) | `Key (`ASCII 'L', []) ->
        state := { !state with show_logs = not !state.show_logs };
        Lwt.return_unit
    | `Key (`ASCII 'q', []) | `Key (`ASCII 'Q', []) ->
        on_quit ()
    | `Key (`ASCII 'b', []) | `Key (`ASCII 'B', []) ->
        state := { !state with show_balances = not !state.show_balances };
        Lwt.return_unit
    | `Key (`ASCII 'a', []) | `Key (`ASCII 'A', []) ->
        state := { !state with show_assets = not !state.show_assets };
        Lwt.return_unit
    | _ -> Lwt.return_unit
  in
(* Main update loop - fetches fresh data and re-renders dashboard every second *)
let rec tick () =
  get_balance_info () >>= fun balances ->
  let new_frame = !state.frame + 1 in
  let active_assets_unsorted = get_all_active_assets () in
  let active_assets = List.sort (compare_assets active_assets_unsorted) active_assets_unsorted in
  let term_dimensions = get_term_dimensions () in  (* Cache terminal dimensions *)

  (* Cache order data for all active assets to prevent polling during render *)
  let order_data = Hashtbl.create 16 in
  let all_orders = Kraken.Kraken_incoming_data.get_all_open_orders () in
  List.iter (fun asset ->
    let orders_for_asset =
      Hashtbl.to_seq_values all_orders
      |> List.of_seq
      |> List.filter (fun order -> String.equal order.Kraken.Kraken_common_types.order_symbol asset)
    in
    let buy_orders, sell_orders =
      List.partition (fun order -> order.Kraken.Kraken_common_types.side = Some Core.Buy) orders_for_asset
    in
    let to_price_qty order = (order.Kraken.Kraken_common_types.limit_price, order.Kraken.Kraken_common_types.qty) in
    let buy_data = List.map to_price_qty buy_orders in
    let sell_data = List.map to_price_qty sell_orders in
    Hashtbl.replace order_data asset (buy_data, sell_data)
  ) active_assets;

  (* Stream logs in real-time - always update cache to ensure fresh logs *)
  let cached_logs = !Stats.dashboard_logs in
  let last_log_count = List.length cached_logs in

  (* Use mutex to prevent race conditions during state updates *)
  Lwt_mutex.with_lock state_mutex (fun () ->
    state := { !state with frame = new_frame; balances; active_assets; order_data; term_dimensions; cached_logs; last_log_count };
    Lwt.return_unit
  ) >>= fun () ->

  (* Initialize transaction history on first tick if balances are available *)
  (if new_frame = 1 && balances <> [] then
    Kraken.Kraken_balances.initialize_transaction_history ()
  else
    Lwt.return_unit
  ) >>= fun () ->

  (* Render with mutex protection *)
  Lwt_mutex.with_lock state_mutex (fun () ->
    Notty_lwt.Term.image term_instance (render !state)
  ) >>= fun () ->
  Lwt.pause () >>= fun () ->
  let sleep_time = 1.0 in
  Lwt_unix.sleep sleep_time >>= tick
  in
  let rec input_loop () =
    Lwt.catch (fun () ->
      Lwt_stream.get (Notty_lwt.Term.events term_instance) >>= function
      | Some event -> handle_event event >>= input_loop
      | None -> 
          Lwt_log_core.info ~section:(Lwt_log_core.Section.make "dashboard") "Input event stream closed." >>= fun () ->
          on_quit () 
    )
    (fun ex -> 
      Lwt_log_core.error ~section:(Lwt_log_core.Section.make "dashboard") 
        (Printf.sprintf "Error in dashboard input handling: %s. Stopping input loop." (Printexc.to_string ex))
      >>= fun () -> on_quit () 
    )
  in
  Lwt.async tick;
  Lwt.async input_loop; 
  term_instance