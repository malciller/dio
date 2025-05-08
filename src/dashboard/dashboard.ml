open Lwt.Infix
open Notty
open Dio_types

module Stats = Stats (* Use Dio.Stats *)
module M = Stats.SMap

(* ─── ASCII sprites ───────────────────────────────────────── *)
(* let pacman_open   = I.string A.(fg yellow ++ st bold) "\u{25C9}" (* ◉ *)
   let pacman_closed = I.string A.(fg yellow ++ st bold) "\u{25CF}" (* ● *) *)
let ghost         = I.string A.(fg magenta) "\u{2588}"          (* █ *)
let cherry        = I.string A.(fg red) "\u{2665}"             (* ♥ *)
let power_pellet  = I.string A.(fg cyan ++ st bold) "\u{2605}" (* ★ *)
let buy_order     = I.string A.(fg green) "⬇"                  (* Buy order *)
let sell_order    = I.string A.(fg red) "⬆"                    (* Sell order *)
let price_now     = I.string A.(fg yellow ++ st blink) "◆"     (* Current price marker *)

let header_lines = [
  "     ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓    ";
  "     ┃  ██████╗ ██╗ ██████╗   ALGORITHMIC TRADING ┃    ";
  "     ┃  ██╔══██╗██║██╔═══██╗  ═══════════════════ ┃    ";
  "     ┃  ██║  ██║██║██║   ██║  DIOPHANT SOLUTIONS  ┃    ";
  "     ┃  ██║  ██║██║██║   ██║  ═══════════════════ ┃    ";
  "     ┃  ██████╔╝██║╚██████╔╝  [L]ogs │ [Q]uit     ┃    ";
  "     ┗━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┛    ";
]

let header =
  header_lines
  |> List.map (I.string A.(fg lightgreen ++ st bold))
  |> I.vcat
  |> I.pad ~l:2 ~t:1

(* ─── helpers ─────────────────────────────────────────────── *)
let fmt_runtime start =
  let secs = int_of_float (Unix.gettimeofday () -. start) in
  Printf.sprintf "⟨ %02dh:%02dm:%02ds ⟩"
    (secs / 3600) (secs mod 3600 / 60) (secs mod 60)

let bar ?(max_len=20) n =
  let len = min max_len n in
  I.hcat (List.init len (fun _ -> power_pellet))

let get_term_dimensions () =
  match Notty_unix.winsize Unix.stdout with
  | Some (h, w) -> (max 24 h, max 80 w) (* Ensure minimum dimensions *)
  | None -> (24, 80) (* Fallback dimensions *)

let price_ladder ~ladder_width current_price buy_orders sell_orders =
  (* buy_orders is expected to be 0 or 1 (the anchor buy) *)
  (* sell_orders is the list of sells to display *)
  let ladder = Array.make ladder_width (I.string A.empty " ") in

  (* Determine min/max for scaling based on the specific orders provided *)
  let min_display_price, max_display_price = 
    let all_relevant_prices = current_price :: (List.map fst buy_orders) @ (List.map fst sell_orders) in
    match all_relevant_prices with
    | [] -> (current_price, current_price) (* Should not happen if current_price is always included *)
    | p::ps -> List.fold_left (fun (min_acc, max_acc) pr -> (min min_acc pr, max max_acc pr)) (p,p) ps
  in

  (* Helper to map price to index within the focused window *)
  let price_to_index price =
    let price_range = max_display_price -. min_display_price in
    if price_range = 0. then
      if ladder_width > 0 then ladder_width / 2 else 0 (* Centered if single point, or 0 if no width *)
    else
      let scale_factor = if ladder_width > 0 then float_of_int (ladder_width - 1) else 0.0 in
      let idx = int_of_float (((price -. min_display_price) /. price_range) *. scale_factor) in
      if ladder_width > 0 then max 0 (min (ladder_width - 1) idx) else 0 (* Clamp index *)
  in

  (* Place orders on ladder *)
  List.iter (fun (price,_) -> 
    let idx = price_to_index price in
    ladder.(idx) <- buy_order
  ) buy_orders;
  
  List.iter (fun (price,_) -> 
    let idx = price_to_index price in
    ladder.(idx) <- sell_order
  ) sell_orders;
  
  (* Place current price marker *)
  let current_idx = price_to_index current_price in
  ladder.(current_idx) <- price_now;
  
  (* Convert to image *)
  I.hcat (Array.to_list ladder)

(* Helper to format price based on symbol *)
let format_price asset price =
  match Kraken.Ws_feed.get_price_precision asset with
  | Some prec -> Printf.sprintf "%.*f" prec price
  | None -> Printf.sprintf "%.2f" price

(* Take first n elements from a list - PLACED BEFORE USAGE *)
let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n-1) xs

(* Row for each asset with price ladder *)
let row_of_asset asset =
  let open I in
  let _, term_width = get_term_dimensions () in
  let current_price_opt = Stats.get_price asset in
  let all_buy_orders_for_symbol, all_sell_orders_for_symbol = Stats.get_orders_for_symbol asset in
  
  (* Logic for the info pane (closest overall buy/sell) - remains the same *)
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
  
  (* Images for the info pane *)
  let asset_label = I.string A.(fg lightcyan ++ st bold) (Printf.sprintf "%-7s" asset) in
  let buy_price_img = I.string A.(fg green) (format_opt_price asset closest_buy_for_info) in
  let curr_price_img = I.string A.(fg yellow) (match current_price_opt with
    | Some p -> format_price asset (Float.of_string (Primitives.Price.to_string p))
    | None -> "-.--")
  in
  let sell_price_img = I.string A.(fg red) (format_opt_price asset closest_sell_for_info) in
  let order_count_str = Printf.sprintf "◎%2d" (List.length all_sell_orders_for_symbol) in (* Count of all sells *)
  let sell_count_img = I.string A.(fg lightmagenta) order_count_str in

  (* Info pane content *)
  let info_pane_content = hcat [
    asset_label;
    I.string A.(fg white) " │ ";
    I.string A.(fg green) "B:"; buy_price_img; (* Shortened label *)
    I.string A.(fg white) " │ ";
    I.string A.(fg yellow) "P:"; curr_price_img; (* Shortened label *)
    I.string A.(fg white) " │ ";
    I.string A.(fg red) "S:"; sell_price_img; (* Shortened label *)
    I.string A.(fg white) " │ ";
    sell_count_img; (* Using the new compact order count image directly *)
  ] in

  (* Separator image *)
  let separator_img = I.string A.(fg white) " │ " in

  (* Calculate available width for the ladder *)
  let total_content_width_inside_borders = term_width - 4 in (* for "┃ " and " ┃" *)
  let calculated_ladder_width =
    total_content_width_inside_borders - I.width info_pane_content - I.width separator_img
  in
  let available_ladder_width = max 1 calculated_ladder_width in

  (* Select orders for the ladder: closest buy, current price, and closest sell *)
  let ladder_buy_orders, ladder_sell_orders = 
    match current_price_opt with
    | Some cp_val ->
        let current_f = Float.of_string (Primitives.Price.to_string cp_val) in
        
        (* 1. Select the closest buy order (<= current_f) *)
        let closest_buy_order_list =
          all_buy_orders_for_symbol
          |> List.filter (fun (p, _) -> p <= current_f)
          |> List.sort (fun (p1, _) (p2, _) -> compare p2 p1) (* Closest first *)
          |> (fun l -> match l with [] -> [] | h :: _ -> [h]) (* Max 1 *)
        in

        (* 2. Select the closest sell order (>= current_f) *)
        let closest_sell_order_list =
          all_sell_orders_for_symbol
          |> List.filter (fun (p, _) -> p >= current_f)
          |> List.sort (fun (p1, _) (p2, _) -> compare p1 p2) (* Closest first *)
          |> (fun l -> match l with [] -> [] | h :: _ -> [h]) (* Max 1 *)
        in
        (closest_buy_order_list, closest_sell_order_list)
    | None -> ([], [])
  in

  (* Ladder image content *)
  let ladder_img_content =
    match current_price_opt with
    | Some current_price_val ->
        price_ladder
          ~ladder_width:available_ladder_width
          (Float.of_string (Primitives.Price.to_string current_price_val))
          ladder_buy_orders (* Use the new selected list *)
          ladder_sell_orders (* Use the new selected list *)
    | None ->
        let no_data_img = I.string A.(fg red) "No price data" in
        (* Snap the "No price data" message to the available width, cropping if necessary *)
        I.hsnap ~align:`Left available_ladder_width no_data_img
  in

  (* Combine info pane, separator, and ladder image *)
  let combined_inline_content = hcat [
    info_pane_content;
    separator_img;
    ladder_img_content;
  ] in
  
  (* Final row construction with borders *)
  (* The combined_inline_content should fill total_content_width_inside_borders *)
  hcat [
    I.string A.(fg white) "┃ ";
    combined_inline_content;
    (* If combined_inline_content is narrower than total_content_width_inside_borders due to rounding or min width, add padding *)
    (* This case should be rare if available_ladder_width is calculated and used correctly. *)
    void (total_content_width_inside_borders - I.width combined_inline_content) 1;
    I.string A.(fg white) " ┃"
  ]

(* Dashboard state *)
type dashboard_state = {
  show_logs: bool;
  frame: int;
}

let initial_state = {
  show_logs = true;
  frame = 0;
}

(* Add this helper function for interspersing elements in a list *)
let rec intersperse sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse sep xs

(* Define the priority order for assets *)
let priority_assets = ["BTC/USD"; "SOL/USD"; "ETH/USD"; "ADA/USD"; "TRX/USD"]

(* Helper to find index in list *)
let find_index pred lst =
  let rec aux i = function
    | [] -> None
    | x :: xs -> if pred x then Some i else aux (i + 1) xs
  in
  aux 0 lst

(* Helper to sort assets based on priority *)
let compare_assets a b =
  let get_priority asset =
    match find_index ((=) asset) priority_assets with
    | Some idx -> idx
    | None -> List.length priority_assets  (* Non-priority assets go last *)
  in
  compare (get_priority a) (get_priority b)

(* Update render function *)
let render state =
  let open I in
  let term_height, term_width = get_term_dimensions () in
  let content_width = term_width - 4 in
  
  (* Runtime display image (no change in its definition) *)
  let runtime_display =
    string A.(fg lightgreen ++ st bold)
      ("Runtime: " ^ fmt_runtime Stats.start_ts)
    |> I.pad ~t:1 ~l:2 (* Original padding; might look better centered or full-width later if desired *)
  in
  
  (* Asset rows section *)
  let all_assets = 
    M.fold (fun asset _ acc -> asset :: acc) !Stats.pending_orders []
    |> List.sort compare_assets
  in
  let asset_rows = List.map row_of_asset all_assets in
  let asset_rows_section = vcat asset_rows in
  
  (* Calculate heights *)
  let header_height = height header in
  let asset_rows_height = height asset_rows_section in
  let runtime_display_height = height runtime_display in
  
  (* Calculate logs_height based on new layout *)
  (* Elements above logs: header, asset_rows_section, runtime_display *)
  (* Padding lines: 1 at screen top, 1 between runtime and logs, 1 at screen bottom = 3 total *)
  let logs_height = 
    if state.show_logs then
      let height_of_content_above_logs = header_height + asset_rows_height + runtime_display_height in
      max 0 (term_height - height_of_content_above_logs - 3) (* 3 for void spacers *)
    else 0
  in
  
  (* Logs section (internal logic remains the same, uses new logs_height) *)
  let logs_section_image = 
    if not state.show_logs || logs_height <= 0 then void 0 0
    else
      let logs_header_img = 
        hcat [
          I.string A.(fg lightcyan ++ st bold) "┏━ LOGS ";
          I.string A.(fg white) (String.make (content_width - 8) '-');
          I.string A.(fg white) "┓"
        ] |> I.pad ~l:2
      in
      let log_messages = 
        (* Ensure we take (logs_height - 1) for messages to account for logs_header_img *)
        let num_log_lines_to_take = max 0 (logs_height - height logs_header_img) in 
        List.map (fun msg -> 
          let msg_img = I.string A.(fg white) msg in
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
            vcat (List.map (fun line -> I.string A.(fg white) line) wrapped_lines)
          else
            (* Pad individual log lines to content_width if shorter *)
            hcat [power_pellet; I.string A.(fg white) " "; msg_img; void (content_width - (width msg_img) - 2) 1] (* -2 for pellet and space *)
        ) (take num_log_lines_to_take (List.rev !Stats.dashboard_logs))
      in
      let logs_body_content = vcat log_messages in
      let logs_full_content = vcat [logs_header_img; logs_body_content] in
      (* Ensure the entire logs section is constrained by logs_height and content_width *)
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
  
  (* Combine sections in the new order with explicit spacing *)
  vcat [
    void content_width 1; (* Top padding *)
    header;                 (* Header section *)
    asset_rows_section;   (* Asset rows section *)
    runtime_display;      (* Runtime display moved here *)
    void content_width 1; (* Padding between runtime and logs *)
    logs_section_image;   (* Logs section *)
    void content_width 1; (* Bottom padding *)
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
        on_quit () (* Call the provided on_quit callback *)
    | _ -> Lwt.return_unit  (* Ignore all other inputs *)
  in
  
  let rec tick () =
    state := { !state with frame = !state.frame + 1 };
    Notty_lwt.Term.image term_instance (render !state) >>= fun () ->
    (* Using Lwt.pause to explicitly yield to other Lwt tasks before sleeping *)
    Lwt.pause () >>= fun () ->
    Lwt_unix.sleep 1.0 >>= tick  (* Reduced from 2.0 to 1.0 for more frequent yields *)
  in
  
  let rec input_loop () =
    Lwt.catch (fun () ->
      Lwt_stream.get (Notty_lwt.Term.events term_instance) >>= function
      | Some event -> handle_event event >>= input_loop
      | None -> 
          (* Stream closed, potentially signal quit as well or log *)
          Lwt_log_core.info ~section:(Lwt_log_core.Section.make "dashboard") "Input event stream closed." >>= fun () ->
          on_quit () (* Consider if closing the stream should also trigger a quit *)
    )
    (fun ex -> 
      (* Log error and potentially stop or attempt to gracefully shutdown dashboard input *)
      Lwt_log_core.error ~section:(Lwt_log_core.Section.make "dashboard") 
        (Printf.sprintf "Error in dashboard input handling: %s. Stopping input loop." (Printexc.to_string ex))
      (* Depending on the error, you might re-call on_quit() or simply let this async thread terminate *)
      >>= fun () -> on_quit () (* Or Lwt.return_unit if quit is already handled by the error source *)
    )
  in
  
  Lwt.async tick;
  Lwt.async input_loop; (* Changed from 'input' to 'input_loop' to avoid conflict if 'input' is a keyword/std lib fn *)
  term_instance
