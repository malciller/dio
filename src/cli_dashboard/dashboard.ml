open Lwt.Infix
open Notty
open Notty.A
open Dio_types
module Stats = Stats (* Use Dio.Stats *)
module M = State.SMap

(* ─── Color Palette & Styles ───────────────────────────────────────── *)
let style_primary_text    = A.fg (A.rgb ~r:(180*5/255) ~g:(180*5/255) ~b:(180*5/255)) (* Approx r:3 g:3 b:3 *)
let style_buy_order_text  = A.fg (A.rgb ~r:(0*5/255)   ~g:(255*5/255) ~b:(100*5/255)) (* Approx r:0 g:5 b:1 *)
let style_sell_order_text = A.fg (A.rgb ~r:(255*5/255) ~g:(50*5/255)  ~b:(50*5/255))  (* Approx r:5 g:0 b:0 *)
let style_current_price_text= A.fg (A.rgb ~r:(0*5/255)   ~g:(255*5/255) ~b:(255*5/255)) (* Approx r:0 g:5 b:5 *)
let style_header_border   = A.fg (A.rgb ~r:(50*5/255)  ~g:(150*5/255) ~b:(255*5/255)) (* Approx r:0 g:2 b:5 *)
let style_logs_accent_text= A.fg (A.rgb ~r:(150*5/255) ~g:(100*5/255) ~b:(255*5/255)) (* Approx r:2 g:1 b:5 *)

(* Combined styles *)
let style_asset_name = style_current_price_text ++ A.st A.bold ++ A.st A.underline
let style_header_title_art = style_header_border ++ A.st A.bold
let style_header_info_text = style_primary_text 
let style_keybinding_bracket = style_header_border
let style_keybinding_text = style_primary_text

(* ─── Updated ASCII/Unicode Sprites ────────────────────────────────────── *)
let spr_power_pellet  = I.string style_logs_accent_text "\u{2726}"  (* ✦ Sparkle *)
let spr_buy_order     = I.string style_buy_order_text "\u{2193}"    (* ↓ Sleek Down Arrow *)
let spr_sell_order    = I.string style_sell_order_text "\u{2191}"   (* ↑ Sleek Up Arrow *)
let spr_price_now     = I.string (style_current_price_text ++ A.st A.bold) "\u{25C7}" (* ◇ Open Diamond, bold, no blink *)


(* ─── helpers ─────────────────────────────────────────────── *)
let fmt_runtime start =
  let secs = int_of_float (Unix.gettimeofday () -. start) in
  Printf.sprintf "%02dh:%02dm:%02ds"
    (secs / 3600) (secs mod 3600 / 60) (secs mod 60)

let get_term_dimensions () =
  match Notty_unix.winsize Unix.stdout with
  | Some (h, w) -> (max 24 h, max 80 w) 
  | None -> (24, 80) 

let price_ladder ~ladder_width current_price buy_orders sell_orders =
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
  if current_idx < ladder_width && current_idx >= 0 then ladder.(current_idx) <- spr_price_now;
  I.hcat (Array.to_list ladder)

let format_price asset price =
  match Kraken.Kraken_incoming_data.get_price_precision asset with
  | Some prec -> Printf.sprintf "%.*f" prec price
  | None -> Printf.sprintf "%.2f" price

let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n-1) xs

let row_of_asset asset =
  let open I in
  let _, term_width = get_term_dimensions () in
  let current_price_opt = Stats.get_price asset in
  let all_buy_orders_for_symbol, all_sell_orders_for_symbol = Stats.get_orders_for_symbol asset in
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
          I.string style_primary_text (Printf.sprintf " (%+.2f%%)" diff)
        ) else I.empty (* Avoid division by zero if current price is 0 *)
    | _ -> I.empty (* Handle cases where current or buy price is missing *)
  in
  let sell_perc_str = 
    match current_price_opt, closest_sell_for_info with
    | Some cp_prim, Some sell_p_float ->
        let current_f = Float.of_string (Primitives.Price.to_string cp_prim) in
        if current_f > 0.0 then (
          let diff = ((sell_p_float -. current_f) /. current_f) *. 100.0 in
          I.string style_primary_text (Printf.sprintf " (%+.2f%%)" diff)
        ) else I.empty
    | _ -> I.empty
  in
  let buy_price_str = format_opt_price asset closest_buy_for_info in
  let sell_price_str = format_opt_price asset closest_sell_for_info in
  let asset_label_img = I.string style_asset_name (Printf.sprintf "%-7s" asset) in
  let buy_price_img = I.hcat [I.string style_buy_order_text buy_price_str; buy_perc_str] in
  let curr_price_img = I.string (style_current_price_text ++ A.st A.bold) (match current_price_opt with 
    | Some p -> format_price asset (Float.of_string (Primitives.Price.to_string p))
    | None -> "-.--")
  in
  let sell_price_img = I.hcat [I.string style_sell_order_text sell_price_str; sell_perc_str] in
  let sell_count_img = I.string style_logs_accent_text (Printf.sprintf "◎%2d" (List.length all_sell_orders_for_symbol)) in
  let info_pane_items = [
    asset_label_img;
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

type dashboard_state = {
  show_logs: bool;
  frame: int;
}

let initial_state = {
  show_logs = true;
  frame = 0;
}

let rec intersperse sep = function
  | [] -> []
  | [x] -> [x]
  | x :: xs -> x :: sep :: intersperse sep xs

let priority_assets = ["BTC/USD"; "ETH/USD"; "SOL/USD"; "ADA/USD"; "TRX/USD"]
let find_index pred lst =
  let rec aux i = function
    | [] -> None
    | x :: xs -> if pred x then Some i else aux (i + 1) xs
  in
  aux 0 lst

let compare_assets a b =
  let get_priority asset =
    match find_index ((=) asset) priority_assets with
    | Some idx -> idx
    | None -> List.length priority_assets  
  in
  compare (get_priority a) (get_priority b)

let render state =
  let open I in
  let term_height, term_width = get_term_dimensions () in
  let content_width = term_width - 4 in

  let horiz_border_char_str = "\u{2501}" in
  let create_horizontal_fill width char_str =
    String.concat "" (List.init (max 0 width) (fun _ -> char_str))
  in

  let header =
    let art_style = style_header_title_art in
    let text_style = style_header_info_text in
    let key_bracket_style = style_keybinding_bracket in
    let key_text_style = style_keybinding_text in

    let line0 = I.string art_style "     ┏━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━┓    " in
    let line1 = I.hcat [
                  I.string art_style         "     ┃  ██████╗ ██╗ ██████╗   ";
                  I.string text_style         "ALGORITHMIC TRADING";
                  I.string art_style         " ┃    ";
                ]
    in
    let line2 = I.string art_style "     ┃  ██╔══██╗██║██╔═══██╗  ═══════════════════ ┃    " in
    let line3 = I.hcat [
                  I.string art_style         "     ┃  ██║  ██║██║██║   ██║  ";
                  I.string text_style         "DIOPHANT SOLUTIONS "; 
                  I.string art_style         " ┃    ";
                ]
    in
    let line4 = I.string art_style "     ┃  ██║  ██║██║██║   ██║  ═══════════════════ ┃    " in
    let line5_keys = I.hcat [
                       I.string key_bracket_style "["; I.string key_text_style "L"; I.string key_bracket_style "]";
                       I.string text_style        "ogs ";
                       I.string key_bracket_style "│";
                       I.string key_bracket_style        " ["; I.string key_text_style "Q"; I.string key_bracket_style "]";
                       I.string text_style        "uit     ";
                     ]
    in
    let line5 = I.hcat [
                  I.string art_style         "     ┃  ██████╔╝██║╚██████╔╝  ";
                  line5_keys;
                  I.string art_style         "┃    ";
                ]
    in

    let runtime_str = fmt_runtime Stats.start_ts in
    let runtime_img = I.string (style_logs_accent_text ++ A.st A.bold) (Printf.sprintf " %s " runtime_str) in
    let runtime_width = I.width runtime_img in
    let line6 =
      (* Required width = 5 (pad) + 1 (┗) + Runtime(W) + 1 (┛) = W + 7 *)
      let required_width_for_line = runtime_width + 7 in
      if term_width >= required_width_for_line then
        (* Total space available for dashes *)
        let total_dash_space = term_width - required_width_for_line in
        let dashes_before_count = max 0 ((total_dash_space / 2) - 30) in
        let dashes_after_count = max 0 ((total_dash_space / 2) + 1 ) in

        I.hcat [
          I.string A.empty "     "; 
          I.string art_style "\u{2517}"; 
          I.string art_style (create_horizontal_fill dashes_before_count horiz_border_char_str); 
          runtime_img;
          I.string art_style (create_horizontal_fill dashes_after_count horiz_border_char_str);
          I.string art_style "\u{251B}" 
        ]
      else 
        I.empty 
    in

    I.vcat [line0; line1; line2; line3; line4; line5; line6]
  in

  let asset_rows = List.map row_of_asset
    (M.fold (fun asset _ acc -> asset :: acc) (!Stats.state).pending_orders [] |> List.sort compare_assets)
  in
  let diogrid_label_text = " DioGrid " in
  let diogrid_label_style = style_logs_accent_text ++ A.st A.bold in 
  let diogrid_label_img = I.string diogrid_label_style diogrid_label_text in
  let diogrid_label_width = I.width diogrid_label_img in
  let top_asset_box_line = 
    match asset_rows with 
    | [] -> I.empty 
    | _ -> 
      
        let min_space_for_labeled_border = diogrid_label_width + 4 in
        if term_width >= min_space_for_labeled_border then
          let fill_width = term_width - 2 - diogrid_label_width - 1 - 1 in 
          I.hcat [
            I.string style_header_border "\u{250F}\u{2501}";
            diogrid_label_img;
            I.string style_header_border horiz_border_char_str;
            I.string style_header_border (create_horizontal_fill fill_width horiz_border_char_str);
            I.string style_header_border "\u{2513}";
          ]
        else (* Fallback to a simple line if not enough space *) 
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
  let header_height = height header in
  let new_asset_rows_height = height asset_rows_section_with_boxing in
  let logs_height =
    if state.show_logs then
      let height_of_content_above_logs = header_height + new_asset_rows_height in
      max 0 (term_height - height_of_content_above_logs - 3)
    else 0
  in
  let logs_section_image = 
    if not state.show_logs || logs_height <= 0 then void 0 0
    else
      let logs_header_text = I.string (style_logs_accent_text ++ A.st A.bold) " Logs " in
      let logs_header_img = 
        hcat [
          I.string style_header_border "┏━"; logs_header_text;
          I.string style_header_border (create_horizontal_fill (content_width - (width logs_header_text) - 4) horiz_border_char_str);
          I.string style_header_border "┓"
        ] |> I.pad ~l:2
      in
      let num_log_lines_to_take = max 0 (logs_height - height logs_header_img) in 
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
    header;
    asset_rows_section_with_boxing;
    void content_width 1;
    logs_section_image;
    void content_width 1;
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
    | _ -> Lwt.return_unit  
  in
  let rec tick () =
    state := { !state with frame = !state.frame + 1 };
    Notty_lwt.Term.image term_instance (render !state) >>= fun () ->
    Lwt.pause () >>= fun () ->
    Lwt_unix.sleep 1.0 >>= tick 
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
