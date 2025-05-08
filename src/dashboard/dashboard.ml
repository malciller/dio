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
  "     ┃  ██████╗ ██╗ ██████╗   ALGORITHMIC TRADE   ┃    ";
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

let price_ladder current_price buy_orders sell_orders =

  (* Sort orders by price *)
  let buys = List.sort (fun (p1,_) (p2,_) -> compare p2 p1) buy_orders in
  let sells = List.sort (fun (p1,_) (p2,_) -> compare p1 p2) sell_orders in
  
  (* Calculate available width for ladder *)
  let _, term_width = get_term_dimensions () in
  let content_width = term_width - 4 in (* Padding for borders *)
  let ladder_width = content_width - 20 in (* Reserve space for price visualization *)
  
  (* Log width calculations for debugging *)
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "pacdash")
    (Printf.sprintf "Price ladder: term_width=%d, content_width=%d, ladder_width=%d"
       term_width content_width ladder_width)
  |> Lwt.ignore_result;
  
  let ladder = Array.make ladder_width (I.string A.empty " ") in
  
  (* Helper to map price to index *)
  let price_to_index price =
    let min_price = min (List.fold_left (fun acc (p,_) -> min acc p) current_price buys)
                        (List.fold_left (fun acc (p,_) -> min acc p) current_price sells) in
    let max_price = max (List.fold_left (fun acc (p,_) -> max acc p) current_price buys)
                        (List.fold_left (fun acc (p,_) -> max acc p) current_price sells) in
    let price_range = max_price -. min_price in
    if price_range = 0. then ladder_width / 2
    else
      let idx = int_of_float ((price -. min_price) /. price_range *. float_of_int (ladder_width - 1)) in
      max 0 (min (ladder_width - 1) idx)
  in
  
  (* Place orders on ladder *)
  List.iter (fun (price,_) -> 
    let idx = price_to_index price in
    ladder.(idx) <- buy_order
  ) buys;
  
  List.iter (fun (price,_) -> 
    let idx = price_to_index price in
    ladder.(idx) <- sell_order
  ) sells;
  
  (* Place current price marker *)
  let current_idx = price_to_index current_price in
  ladder.(current_idx) <- price_now;
  
  (* Convert to image - just return the ladder without the asset name *)
  I.hcat (Array.to_list ladder)

(* Helper to format price based on symbol *)
let format_price asset price =
  match Kraken.Ws_feed.get_price_precision asset with
  | Some prec -> Printf.sprintf "%.*f" prec price
  | None -> Printf.sprintf "%.2f" price

(* Row for each asset with price ladder *)
let row_of_asset asset =
  let open I in
  let current_price_opt = Stats.get_price asset in
  let buy_prices, sell_prices = Stats.get_orders_for_symbol asset in
  
  (* Get closest buy and sell prices *)
  let closest_buy = match current_price_opt with
    | Some current_price ->
        let current = Float.of_string (Primitives.Price.to_string current_price) in
        List.fold_left (fun acc (price, _) ->
          if price <= current then
            match acc with
            | None -> Some price
            | Some p -> Some (max p price)
          else acc
        ) None buy_prices
    | None -> None
  in
  
  let closest_sell = match current_price_opt with
    | Some current_price ->
        let current = Float.of_string (Primitives.Price.to_string current_price) in
        List.fold_left (fun acc (price, _) ->
          if price >= current then
            match acc with
            | None -> Some price
            | Some p -> Some (min p price)
          else acc
        ) None sell_prices
    | None -> None
  in
  
  let format_opt_price asset opt_price =
    match opt_price with
    | Some price -> format_price asset price
    | None -> "-.--"
  in
  
  (* Stats section (now on the left) *)
  let stats = 
    let asset_label = I.string A.(fg lightcyan ++ st bold) (Printf.sprintf "%-8s" asset) in
    let buy_price = I.string A.(fg green) (format_opt_price asset closest_buy) in
    let curr_price = I.string A.(fg yellow) (match current_price_opt with 
      | Some p -> format_price asset (Float.of_string (Primitives.Price.to_string p))
      | None -> "-.--") in
    let sell_price = I.string A.(fg red) (format_opt_price asset closest_sell) in
    let sell_count = I.string A.(fg lightmagenta) (Printf.sprintf "◀ %d ▶" (List.length sell_prices)) in
    hcat [
      asset_label;
      I.string A.(fg white) " │ ";
      I.string A.(fg green) "buy: "; buy_price;
      I.string A.(fg white) " │ ";
      I.string A.(fg yellow) "now: "; curr_price;
      I.string A.(fg white) " │ ";
      I.string A.(fg red) "sell: "; sell_price;
      I.string A.(fg white) " │ ";
      I.string A.(fg lightmagenta) "orders: "; sell_count;
    ]
  in
  
  (* Price ladder visualization (now on the right) *)
  let price_view = match current_price_opt with
    | Some current_price -> 
        price_ladder (Float.of_string (Primitives.Price.to_string current_price)) buy_prices sell_prices
    | None ->
        string A.(fg red) "No price data"
  in
  
  let _, term_width = get_term_dimensions () in
  let content_width = term_width - 4 in
  let row = stats <|> void 2 1 <|> price_view in (* Added spacing between stats and price_view *)
  
  (* Add border elements *)
  hcat [
    I.string A.(fg white) "┃ ";
    row;
    void (content_width - width row - 4) 1;
    I.string A.(fg white) " ┃"
  ]

(* Take first n elements from a list *)
let rec take n = function
  | [] -> []
  | x :: xs -> if n <= 0 then [] else x :: take (n-1) xs

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
  
  (* Runtime display *)
  let runtime_display =
    string A.(fg lightgreen ++ st bold)
      ("Runtime: " ^ fmt_runtime Stats.start_ts)
    |> I.pad ~t:1 ~l:2
  in
  
  (* Trading view - now with ordered assets *)
  let all_assets = 
    M.fold (fun asset _ acc -> asset :: acc) !Stats.pending_orders []
    |> List.sort compare_assets
  in
  let rows = List.map row_of_asset all_assets in
  
  (* Calculate heights with priority to main section *)
  let total_padding = 4 in
  let header_height = height header in
  let runtime_height = height runtime_display in
  let rows_height = List.length rows in
  
  (* Main section must show everything *)
  let main_height = header_height + runtime_height + rows_height + 2 in (* +2 for spacing *)
  
  (* Remaining space for logs *)
  let logs_height = 
    if state.show_logs then
      max 0 (term_height - main_height - total_padding)
    else 0
  in
  
  (* Main section *)
  let main_section =
    let content = vcat (
      header ::
      runtime_display ::
      rows
    ) in
    let width_adjusted = 
      if width content < content_width then
        hcat [content; void (content_width - width content) main_height]
      else
        content
    in
    width_adjusted
  in
  
  (* Logs section *)
  let logs_section = 
    if not state.show_logs || logs_height <= 0 then void 0 0
    else
      let logs_header = 
        hcat [
          I.string A.(fg lightcyan ++ st bold) "┏━ LOGS ";
          I.string A.(fg white) (String.make (content_width - 8) '-');  (* Using simple ASCII *)
          I.string A.(fg white) "┓"
        ] |> I.pad ~l:2
      in
      let log_messages =
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
            hcat [power_pellet; msg_img; void (content_width - msg_width - 1) 1]
        ) (take (logs_height - 1) (List.rev !Stats.dashboard_logs)) (* -1 to account for header *)
      in
      let logs_content = vcat (logs_header :: log_messages) in
      let width_adjusted =
        if width logs_content < content_width then
          hcat [logs_content; void (content_width - width logs_content) logs_height]
        else
          logs_content
      in
      width_adjusted
  in
  
  (* Combine sections with explicit spacing *)
  vcat [
    void content_width 1;
    main_section;
    void content_width 1;
    logs_section;
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