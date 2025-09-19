open Lwt.Infix
open Notty
open Notty.A
open Dio_types
module Stats = Stats
module M = State.SMap
module StringSet = Set.Make(String)

let is_stablecoin asset =
  let stablecoins = ["USD"; "USDT"; "USDC"; "USDG"; "USDR"] in
  List.mem asset stablecoins

(* ─── Enhanced Color Palette & Styles ───────────────────────────────────────── *)
(* Professional dark theme with neon accents *)
let rgb_of_255 ~r ~g ~b = A.rgb ~r:(r*5/255) ~g:(g*5/255) ~b:(b*5/255)
let style_primary_text    = A.fg (rgb_of_255 ~r:200 ~g:200 ~b:200)
let style_buy_order_text  = A.fg (rgb_of_255 ~r:0 ~g:255 ~b:100) ++ A.st A.bold
let style_sell_order_text = A.fg (rgb_of_255 ~r:255 ~g:100 ~b:100) ++ A.st A.bold
let style_current_price_text= A.fg (rgb_of_255 ~r:0 ~g:200 ~b:200) ++ A.st A.bold ++ A.st A.underline
let style_header_border   = A.fg (rgb_of_255 ~r:0 ~g:150 ~b:150) ++ A.st A.bold
let style_logs_accent_text= A.fg (rgb_of_255 ~r:255 ~g:200 ~b:100) ++ A.st A.bold

(* New professional colors *)
let style_profit_text     = A.fg (rgb_of_255 ~r:50 ~g:255 ~b:100) ++ A.st A.bold
let style_loss_text       = A.fg (rgb_of_255 ~r:255 ~g:100 ~b:100) ++ A.st A.bold
let style_neutral_text    = A.fg (rgb_of_255 ~r:200 ~g:200 ~b:200)
let style_highlight_text  = A.fg (rgb_of_255 ~r:255 ~g:200 ~b:100) ++ A.st A.bold
let style_warning_text    = A.fg (rgb_of_255 ~r:255 ~g:150 ~b:50) ++ A.st A.bold
let style_success_text    = A.fg (rgb_of_255 ~r:100 ~g:255 ~b:150) ++ A.st A.bold

(* Status indicator colors *)
let style_active_indicator = A.fg (rgb_of_255 ~r:50 ~g:255 ~b:150) ++ A.st A.bold
let style_inactive_indicator = A.fg (rgb_of_255 ~r:150 ~g:150 ~b:150)
let style_error_indicator  = A.fg (rgb_of_255 ~r:255 ~g:100 ~b:100) ++ A.st A.bold

(* Combined styles *)
let style_asset_name = style_current_price_text ++ A.st A.bold ++ A.st A.underline
let style_header_title_art = style_header_border ++ A.st A.bold
let style_header_info_text = style_primary_text 
let style_keybinding_bracket = style_header_border
let style_keybinding_text = style_primary_text

(* ─── Enhanced Professional Unicode Sprites ────────────────────────────────────── *)
let spr_power_pellet  = I.string style_logs_accent_text "*"  (* Star - for highlights *)
let spr_buy_order     = I.string style_buy_order_text "\u{25B2}"    (* ▲ Up Triangle - for buys *)
let spr_sell_order    = I.string style_sell_order_text "\u{25BC}"   (* ▼ Down Triangle - for sells *)
let spr_price_now frame =
  let blink = (frame / 10) mod 2 = 0 in
  let style = if blink then
    style_current_price_text ++ A.st A.bold ++ A.st A.underline
  else
    style_current_price_text ++ A.st A.bold in
  I.string style "⦿" (* Circled Bullet - for current price *)
let spr_profit        = I.string style_profit_text "+"      (* Profit indicator *)
let spr_loss          = I.string style_loss_text "-"        (* Loss indicator *)
let spr_neutral       = I.string style_neutral_text "."     (* Neutral indicator *)
let spr_active        = I.string style_active_indicator "[A]" (* Active *)
let spr_inactive      = I.string style_inactive_indicator "[I]" (* Inactive *)
let spr_warning       = I.string style_warning_text "[W]"     (* Warning *)
let spr_error         = I.string style_error_indicator "[E]"  (* Error *)
let spr_grid          = I.string style_highlight_text "[G]"   (* Grid strategy *)
let spr_orderbook     = I.string style_highlight_text "[M]"   (* Market Maker strategy *)
let spr_arbitrage     = I.string style_highlight_text "[A]"    (* Arbitrage strategy *)


(* ─── helpers ─────────────────────────────────────────────── *)
let fmt_runtime start =
  let secs = int_of_float (Unix.gettimeofday () -. start) in
  Printf.sprintf "%02d:%02d:%02d"
    (secs / 3600) (secs mod 3600 / 60) (secs mod 60)

let get_term_dimensions () =
  match Notty_unix.winsize Unix.stdout with
  | Some (w, h) -> (h, w)
  | None -> (24, 80)

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

let format_price asset price =
  match Kraken.Kraken_incoming_data.get_price_precision asset with
  | Some prec -> Printf.sprintf "%.*f" prec price
  | None -> Printf.sprintf "%.2f" price

let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n-1) xs

let get_strategy_indicator asset =
  match State.get_global_strategy_assignment asset with
  | Some State.Grid -> "GRID"
  | Some State.Orderbook -> "MM"
  | Some State.Arbitrage -> "ARB"
  | Some State.Monitor -> "MONITOR"
  | None ->
      (* Fallback: check if there are actual orders for this asset *)
      let open_orders = Kraken.Kraken_incoming_data.get_all_open_orders () in
      let has_orders = Hashtbl.fold (fun _ order acc ->
        acc || String.equal order.Kraken.Kraken_common_types.order_symbol asset
      ) open_orders false in
      if has_orders then "ARB" else "MONITOR"

let row_of_asset asset frame =
  let open I in
  let _, term_width = get_term_dimensions () in
  let current_price_opt = Stats.get_price asset in
  let all_buy_orders_for_symbol, all_sell_orders_for_symbol = Stats.get_orders_for_symbol asset in
  let strategy_indicator = get_strategy_indicator asset in
  let closest_buy_for_info = match current_price_opt with
    | Some current_price_val ->
        let current = Float.of_string (Primitives.Price.to_string current_price_val) in
        List.fold_left (fun acc (price, _) ->
          if price <= current then
            match acc with
            | None -> Some price
            | Some p -> Some (max p price)
          else acc
        ) None all_buy_orders_for_symbol
    | None -> None
  in
  let closest_sell_for_info = match current_price_opt with
    | Some current_price_val ->
        let current = Float.of_string (Primitives.Price.to_string current_price_val) in
        List.fold_left (fun acc (price, _) ->
          if price >= current then
            match acc with
            | None -> Some price
            | Some p -> Some (min p price)
          else acc
        ) None all_sell_orders_for_symbol
    | None -> None
  in
  let format_opt_price asset opt_price =
    match opt_price with
    | Some price -> format_price asset price
    | None -> "-.--"
  in
  let buy_perc_str =
    match current_price_opt, closest_buy_for_info with
    | Some cp_prim, Some buy_p_float ->
        let current_f = Float.of_string (Primitives.Price.to_string cp_prim) in
        if current_f > 0.0 then (
           (* Calculate diff of buy relative to current price *)
          let diff = ((buy_p_float -. current_f) /. current_f) *. 100.0 in
          let style = if diff >= 0.0 then style_profit_text else style_loss_text in
          I.hcat [I.string style (Printf.sprintf "%+.2f%%" diff)]
        ) else I.string style_neutral_text " --"
    | _ -> I.string style_neutral_text " --"
  in
  let sell_perc_str =
    match current_price_opt, closest_sell_for_info with
    | Some cp_prim, Some sell_p_float ->
        let current_f = Float.of_string (Primitives.Price.to_string cp_prim) in
        if current_f > 0.0 then (
          let diff = ((sell_p_float -. current_f) /. current_f) *. 100.0 in
          let style = if diff >= 0.0 then style_profit_text else style_loss_text in
          I.hcat [I.string style (Printf.sprintf "%+.2f%%" diff)]
        ) else I.string style_neutral_text " --"
    | _ -> I.string style_neutral_text " --"
  in
  let buy_price_str = format_opt_price asset closest_buy_for_info in
  let sell_price_str = format_opt_price asset closest_sell_for_info in
  let asset_label_img = I.string style_asset_name (Printf.sprintf "%-7s" asset) in
  let strategy_img = I.string style_logs_accent_text (Printf.sprintf "[%s]" strategy_indicator) in
  let buy_price_img = I.hcat [I.string style_buy_order_text buy_price_str; buy_perc_str] in
  let curr_price_img = I.string (style_current_price_text ++ A.st A.bold) (match current_price_opt with 
    | Some p -> format_price asset (Float.of_string (Primitives.Price.to_string p))
    | None -> "-.--")
  in
  let sell_price_img = I.hcat [I.string style_sell_order_text sell_price_str; sell_perc_str] in
  let sell_count_img = I.string style_logs_accent_text (Printf.sprintf "◎%2d" (List.length all_sell_orders_for_symbol)) in
  let info_pane_items = [
    asset_label_img;
    I.string style_header_border " ";
    strategy_img;
    I.string style_header_border " │ ";
    I.string style_buy_order_text "B:"; buy_price_img;
    I.string style_header_border " │ ";
    I.string style_current_price_text "P:"; curr_price_img;
    I.string style_header_border " │ ";
    I.string style_sell_order_text "S:"; sell_price_img;
  ] in
  let combined_info_text = I.hcat info_pane_items in
  let content_width_for_panes = term_width - 4 in 
  let info_pane_line = hcat [
    I.string style_header_border "┃ ";
    combined_info_text;
    void (content_width_for_panes - I.width combined_info_text) 1; 
    I.string style_header_border " ┃";
  ] in
  let ladder_buy_orders_for_ladder_display, ladder_sell_orders_for_ladder_display = 
    match current_price_opt with
    | Some cp_val ->
        let current_f = Float.of_string (Primitives.Price.to_string cp_val) in
        let closest_buy_list =
          all_buy_orders_for_symbol 
          |> List.filter (fun (p, _) -> p <= current_f)
          |> List.sort (fun (p1, _) (p2, _) -> compare p2 p1) 
          |> (fun l -> match l with [] -> [] | h :: _ -> [h]) 
        in
        let closest_sell_list =
          all_sell_orders_for_symbol 
          |> List.filter (fun (p, _) -> p >= current_f)
          |> List.sort (fun (p1, _) (p2, _) -> compare p1 p2) 
          |> (fun l -> match l with [] -> [] | h :: _ -> [h]) 
        in
        (closest_buy_list, closest_sell_list)
    | None -> ([], [])
  in
  let ladder_img_content =
    match current_price_opt with
    | Some current_price_val ->
        let orders_count_img_for_ladder_line = sell_count_img in 
        let separator_for_ladder_line = I.string style_header_border " │ " in
        let width_for_ladder_visual =
          max 10 (content_width_for_panes - (I.width orders_count_img_for_ladder_line + I.width separator_for_ladder_line))
        in
        let actual_ladder_visualization = price_ladder
          ~ladder_width:width_for_ladder_visual
          (Float.of_string (Primitives.Price.to_string current_price_val))
          ladder_buy_orders_for_ladder_display
          ladder_sell_orders_for_ladder_display
          frame
        in
        I.hcat [orders_count_img_for_ladder_line; separator_for_ladder_line; actual_ladder_visualization]
    | None ->
        let no_data_img = I.string style_sell_order_text "No price data" in
        I.hsnap ~align:`Left content_width_for_panes no_data_img 
  in
  let ladder_pane_line = hcat [
    I.string style_header_border "┃ ";
    ladder_img_content; 
    void (content_width_for_panes - I.width ladder_img_content) 1; 
    I.string style_header_border " ┃";
  ] in
  I.vcat [info_pane_line; ladder_pane_line]

type balance_info = {
  asset: string;
  total_balance: float;
  total_value_usd: float;
  accumulated_balance: float;
  accumulated_value_usd: float;
  unrealized_value_usd: float;
}

type dashboard_state = {
  show_logs: bool;
  frame: int;
  balances: balance_info list;
  show_balances: bool;
}

let initial_state = {
  show_logs = true;
  frame = 0;
  balances = [];
  show_balances = true;
}

let rec intersperse sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse sep xs

(* Get all active trading symbols from order data and strategies *)
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

let compare_assets a b =
  let active_assets = get_all_active_assets () in
  let get_priority asset =
    match find_index ((=) asset) active_assets with
    | Some idx -> idx
    | None -> List.length active_assets
  in
  compare (get_priority a) (get_priority b)

let get_balance_info () : balance_info list Lwt.t =

  Kraken.Kraken_balances.wait_for_balances () >>= fun balances ->
  let open_orders = Kraken.Kraken_incoming_data.get_all_open_orders () in
  let on_sell_orders : (string, Kraken.Kraken_common_types.order list) Hashtbl.t = Hashtbl.create 16 in
  let get_base_asset s =
    try String.sub s 0 (String.index s '/')
    with Not_found -> s in
  Hashtbl.iter (fun _order_id (order: Kraken.Kraken_common_types.order) ->
    match order.side with
    | Some Core.Sell ->
        let base_asset = get_base_asset order.order_symbol in
        let current_orders = Option.value ~default:[] (Hashtbl.find_opt on_sell_orders base_asset) in
        Hashtbl.replace on_sell_orders base_asset (order :: current_orders)
    | _ -> ()
  ) open_orders;

  let balance_info_list_lwt = Hashtbl.fold (fun asset total_balance acc_lwt ->
    acc_lwt >>= fun acc ->
    let price_usd_lwt =
      if asset = "USD" || asset = "USDT" || asset = "USDC" then Lwt.return_some 1.0
      else
        let pair = asset ^ "/USD" in
        match Stats.get_price pair with
        | Some p -> Lwt.return_some (Float.of_string (Primitives.Price.to_string p))
        | None ->
            let pair_usdt = asset ^ "/USDT" in
            match Stats.get_price pair_usdt with
            | Some p -> Lwt.return_some (Float.of_string (Primitives.Price.to_string p))
            | None -> Lwt.return_none
    in

    price_usd_lwt >|= function
    | None -> acc
    | Some price_usd ->
        if total_balance < 0.000001 then acc else
        let sell_orders_for_asset = Option.value ~default:[] (Hashtbl.find_opt on_sell_orders asset) in
        let on_orders_balance = List.fold_left (fun acc (o:Kraken.Kraken_common_types.order) -> acc +. o.qty) 0.0 sell_orders_for_asset in

        (* Use transaction history for better cost basis calculation *)
        let accumulated_cost_opt = Transaction_history.get_accumulated_cost asset (total_balance -. on_orders_balance) in

        let accumulated_balance = total_balance -. on_orders_balance in

        (* Calculate accumulated value using cost basis if available, otherwise fallback to current price
           USD accumulated value must always be 0.0 regardless of cost basis or price. *)
        let accumulated_value_usd =
          if asset = "USD" then 0.0
          else (
            match accumulated_cost_opt with
            | Some cost -> cost
            | None -> accumulated_balance *. price_usd
          )
        in

        (* Calculate unrealized value based on pending sell orders *)
        let unrealized_value_usd =
          let unrealized_value_on_orders = List.fold_left (fun acc (o:Kraken.Kraken_common_types.order) -> acc +. (o.qty *. o.limit_price)) 0.0 sell_orders_for_asset in
          let highest_sell_price = List.fold_left (fun max_p (o:Kraken.Kraken_common_types.order) -> max max_p o.limit_price) 0.0 sell_orders_for_asset in
          let unrealized_value_accumulated =
            if highest_sell_price > 0.0 then
              accumulated_balance *. highest_sell_price
            else
              accumulated_balance *. price_usd
          in
          unrealized_value_on_orders +. unrealized_value_accumulated
        in

        let info = {
          asset;
          total_balance;
          total_value_usd = total_balance *. price_usd;
          accumulated_balance;
          accumulated_value_usd;
          unrealized_value_usd;
        } in
        info :: acc
  ) balances (Lwt.return [])
  in

  balance_info_list_lwt >|= fun balance_info_list ->
  List.sort (fun b1 b2 -> compare b1.asset b2.asset) balance_info_list

let render_balances_section (balances: balance_info list) term_width =
  let open I in
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
      I.string style_primary_text (Printf.sprintf "%*s" total_w "Total");
      I.string style_header_border "│";
      I.string style_primary_text (Printf.sprintf "%*s" value_w "Value");
      I.string style_header_border "│";
      I.string style_primary_text (Printf.sprintf "%*s" accum_w "Accumulated Value");
      I.string style_header_border "│";
      I.string style_primary_text (Printf.sprintf "%*s" unreal_w "Unrealized Value");
      I.string style_header_border "┃";
    ] in

    let rows = List.map (fun info ->
      let value_style = if info.total_value_usd > 100.0 then style_profit_text else style_neutral_text in
      let unrealized_display =
        let dollar_str = Printf.sprintf "$%.2f" info.unrealized_value_usd in
        if info.total_value_usd = 0.0 then
          I.string style_neutral_text (Printf.sprintf "%*s" unreal_w dollar_str)
        else
          let perc = ((info.unrealized_value_usd -. info.total_value_usd) /. info.total_value_usd) *. 100.0 in
          let perc_style = if perc >= 0.0 then style_profit_text else style_loss_text in
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
        I.string style_primary_text (Printf.sprintf "%*s" total_w (Printf.sprintf "%.8f %s" info.total_balance info.asset));
        I.string style_header_border "│";
        I.string value_style (Printf.sprintf "%*s" value_w (Printf.sprintf "$%.2f" info.total_value_usd));
        I.string style_header_border "│";
        I.string style_success_text (Printf.sprintf "%*s" accum_w (Printf.sprintf "$%.2f" info.accumulated_value_usd));
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
        let perc_style = if perc >= 0.0 then (style_highlight_text ++ A.st A.bold) else (style_loss_text ++ A.st A.bold) in
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
    let total_row =
      I.hcat [
        I.string style_header_border "┃";
        I.string (style_highlight_text ++ A.st A.bold) (Printf.sprintf "%-*s" asset_w "TOTAL");
        I.string style_header_border "│";
        I.string style_neutral_text (Printf.sprintf "%*s" total_w "");
        I.string style_header_border "│";
        I.string (style_profit_text ++ A.st A.bold) (Printf.sprintf "%*s" value_w (Printf.sprintf "$%.2f" total_portfolio_value));
        I.string style_header_border "│";
        I.string (style_success_text ++ A.st A.bold) (Printf.sprintf "%*s" accum_w (Printf.sprintf "$%.2f" total_accumulated_value));
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

    vcat ([top_border; header; sep] @ intersperse sep rows @ [sep; total_row; bottom_border])

let render state =
  let open I in
  let term_height, term_width = get_term_dimensions () in
  let content_width = term_width - 4 in

  let horiz_border_char_str = "\u{2501}" in
  let create_horizontal_fill width char_str =
    String.concat "" (List.init (max 0 width) (fun _ -> char_str))
  in

  let asset_rows = List.map (fun asset -> row_of_asset asset state.frame)
    (get_all_active_assets () |> List.sort compare_assets)
  in
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
      I.string key_bracket_style "["; I.string key_text_style "L"; I.string key_bracket_style "]";
      I.string text_style "ogs ";
      I.string key_bracket_style "│";
      I.string key_bracket_style " ["; I.string key_text_style "B"; I.string key_bracket_style "]";
      I.string text_style "alances ";
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

  let get_strategy_assets strategy_type =
    let all_assets = get_all_active_assets () in
    List.filter (fun asset ->
      match State.get_global_strategy_assignment asset with
      | Some strat when strat = strategy_type -> true
      | _ -> false
    ) all_assets
  in

  let grid_assets = get_strategy_assets State.Grid in
  let orderbook_assets = get_strategy_assets State.Orderbook in
  let arbitrage_assets = get_strategy_assets State.Arbitrage in

  let format_asset_list assets =
    let get_base_asset s =
      try String.sub s 0 (String.index s '/')
      with Not_found -> s
    in
    let formatted_list = List.map get_base_asset assets in
    String.concat "," formatted_list
  in

  let grid_str = if grid_assets <> [] then
    Printf.sprintf "GRID[%s]" (format_asset_list grid_assets)
  else "" in
  let orderbook_str = if orderbook_assets <> [] then
    Printf.sprintf "MM[%s]" (format_asset_list orderbook_assets)
  else "" in
  let arbitrage_str = if arbitrage_assets <> [] then
    Printf.sprintf "ARB[%s]" (format_asset_list arbitrage_assets)
  else "" in

  let active_parts = List.filter (fun s -> s <> "") [grid_str; orderbook_str; arbitrage_str] in
  let strategy_status = I.string style_highlight_text ("Active Strategies: " ^ String.concat " • " active_parts) in

  let header_components = [left_label; runtime_img; strategy_status; key_bindings_img] in
  let separator = I.string style_header_border " │ " in
  let header_content = I.hcat (intersperse separator header_components) in
  let inner_width = term_width - 2 in
  let content_w = I.width header_content in
  let fill_w = max 0 (inner_width - content_w) in
  let new_header = hcat [
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
    match asset_rows with
    | [] -> I.empty
    | _ -> 
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
                   1 (* void before assets *) + new_asset_rows_height + 1 (* void after assets *) in
let logs_height =
  if state.show_logs then
    max 0 (term_height - other_height)
  else 0
in

let logs_section_image = 
  if not state.show_logs || logs_height <= 0 then void 0 0
  else
    let logs_header_text = I.hcat [
      I.string (style_logs_accent_text ++ A.st A.bold) "System Logs ";
      I.string style_neutral_text "(";
      I.string style_success_text (string_of_int (List.length !Stats.dashboard_logs));
      I.string style_neutral_text " entries)"
    ] in
    let logs_header_img = 
      hcat [
        I.string style_header_border "┏━"; logs_header_text;
        I.string style_header_border (create_horizontal_fill (term_width - (width logs_header_text) - 3) horiz_border_char_str);
        I.string style_header_border "┓"
      ]
    in
    let logs_header_h = height logs_header_img in
    let num_log_lines_to_take = max 0 (logs_height - logs_header_h) in 
    let log_messages = 
      List.map (fun msg -> 
        let msg_img = I.string style_primary_text msg in
        let msg_width = width msg_img in
        if msg_width > content_width then
          let chars_per_line = content_width in
          let rec wrap_text remaining_text acc =
            if String.length remaining_text <= chars_per_line then
              remaining_text :: acc
            else
              let line = String.sub remaining_text 0 chars_per_line in
              let rest = String.sub remaining_text chars_per_line 
                (String.length remaining_text - chars_per_line) in
              wrap_text rest (line :: acc)
          in
          let wrapped_lines = wrap_text msg [] |> List.rev in
          vcat (List.map (fun line -> I.string style_primary_text line) wrapped_lines)
        else
          hcat [spr_power_pellet; I.string style_primary_text " "; msg_img; void (content_width - (width msg_img) - (width spr_power_pellet) - 1) 1]
      ) (take num_log_lines_to_take (List.rev !Stats.dashboard_logs))
    in
    let logs_body_content = vcat log_messages in
    let logs_full_content = vcat [logs_header_img; logs_body_content] in
    let cropped_logs_content = 
      if height logs_full_content > logs_height then 
        vsnap ~align:`Top logs_height logs_full_content 
      else 
        logs_full_content 
    in
    if width cropped_logs_content < content_width then
      hcat [cropped_logs_content; void (content_width - width cropped_logs_content) (height cropped_logs_content)]
    else
      cropped_logs_content
in
vcat [
  new_header;
  void term_width 1;
  balances_section_image;
  (if state.show_balances && state.balances <> [] then void term_width 1 else I.empty);
  asset_rows_section_with_boxing;
  void term_width 1;
  logs_section_image;
  void term_width 1
]

(* ─── loop ────────────────────────────────────────────────── *)
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
    | _ -> Lwt.return_unit
  in
  let rec tick () =
    get_balance_info () >>= fun balances ->
    let new_frame = !state.frame + 1 in
    state := { !state with frame = new_frame; balances };

    (* Initialize transaction history on first tick if balances are available *)
    (if new_frame = 1 && balances <> [] then
      Kraken.Kraken_balances.initialize_transaction_history ()
    else
      Lwt.return_unit
    ) >>= fun () ->

    (* Periodic balance reconciliation (every 60 seconds / 60 frames) *)
    (if new_frame mod 60 = 0 then
      Lwt_list.iter_s (fun balance_info ->
        Dio_types.Transaction_history.reconcile_balance balance_info.asset balance_info.total_balance
      ) balances
    else
      Lwt.return_unit
    ) >>= fun () ->

    Notty_lwt.Term.image term_instance (render !state) >>= fun () ->
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