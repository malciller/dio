(* src/exchange/kraken/kraken_orderbook.ml *)

open Lwt.Infix
module Json = Yojson.Safe
module JsonUtil = Yojson.Safe.Util
open Dio_types

let section = Lwt_log_core.Section.make "kraken_orderbook"

(* Price level data structure *)
type price_level = {
  price: float;
  qty: float;
  price_str: string; (* string representation for checksum *)
  qty_str: string;   (* string representation for checksum *)
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Orderbook data structure *)
type orderbook = {
  symbol: string;
  bids: price_level list;
  asks: price_level list;
  checksum: int32;
  timestamp: string option;
}

(* Book channel types for parsing *)
type book_data = {
  asks: price_level list;
  bids: price_level list;
  checksum: int32;
  symbol: string;
  timestamp: string option;
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Custom JSON parsers for book_data to handle optional timestamp *)
let book_data_of_yojson ?(get_precisions = fun _ -> None) json =
  let open Yojson.Safe.Util in
  try
    let symbol = json |> member "symbol" |> to_string in
    let (price_precision, qty_precision) = 
      match get_precisions symbol with
      | Some (price_prec, qty_prec) -> (price_prec, qty_prec)
      | None -> (8, 8) (* Default fallback *)
    in
    
    let parse_price_level json =
      let price_str = 
        match json |> member "price" with
        | `String s -> s
        | `Float f -> Printf.sprintf "%.*f" price_precision f
        | `Int i -> string_of_int i
        | _ -> failwith "Invalid price format"
      in
      let qty_str = 
        match json |> member "qty" with
        | `String s -> s
        | `Float f -> Printf.sprintf "%.*f" qty_precision f
        | `Int i -> string_of_int i
        | _ -> failwith "Invalid qty format"
      in
      let price_float = Float.of_string price_str in
      let qty_float = Float.of_string qty_str in
      { price = price_float; qty = qty_float; price_str; qty_str }
    in
    Ok {
      asks = json |> member "asks" |> to_list |> List.map parse_price_level;
      bids = json |> member "bids" |> to_list |> List.map parse_price_level;
      checksum = json |> member "checksum" |> to_int |> Int32.of_int;
      symbol;
      timestamp = json |> member "timestamp" |> to_string_option;
    }
  with 
  | Yojson.Safe.Util.Type_error (msg, _) -> Error ("book_data: " ^ msg)
  | exn -> Error ("book_data: " ^ Printexc.to_string exn)

let book_data_of_yojson_exn json =
  match book_data_of_yojson json with
  | Ok v -> v
  | Error msg -> failwith msg

type book_response = {
  channel: string;
  type_: string; [@key "type"]
  data: book_data list;
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(* Global orderbook storage *)
let orderbooks : (string, orderbook) Hashtbl.t = Hashtbl.create 16

(* Store previous top-of-book for change detection *)
let previous_top_of_book : (string, (float * float)) Hashtbl.t = Hashtbl.create 16

(* Log top-of-book changes *)
let log_top_of_book_update (symbol: string) (sorted_bids: price_level list) (sorted_asks: price_level list) : unit Lwt.t =
  match sorted_bids, sorted_asks with
  | top_bid :: _, top_ask :: _ ->
    let current_top = (top_bid.price, top_ask.price) in
    let previous_top = Hashtbl.find_opt previous_top_of_book symbol in
    
    (* Check if top-of-book changed *)
    let should_log = match previous_top with
      | None -> true (* First time seeing this symbol *)
      | Some (prev_bid, prev_ask) -> 
        not (Float.equal prev_bid top_bid.price && Float.equal prev_ask top_ask.price)
    in
    
    if should_log then (
      Hashtbl.replace previous_top_of_book symbol current_top;
      Lwt_log_core.info ~section
        (Printf.sprintf "Top-of-book update for %s: bid=%.8f@%.8f ask=%.8f@%.8f spread=%.8f"
           symbol top_bid.price top_bid.qty top_ask.price top_ask.qty (top_ask.price -. top_bid.price))
    ) else
      Lwt.return_unit
  | [], _ ->
    Lwt_log_core.warning ~section (Printf.sprintf "No bids available for %s" symbol)
  | _, [] ->
    Lwt_log_core.warning ~section (Printf.sprintf "No asks available for %s" symbol)

(* Take first n elements from list *)
let take n lst =
  let rec take_aux acc n = function
    | [] -> List.rev acc
    | h :: t when n > 0 -> take_aux (h :: acc) (n - 1) t
    | _ -> List.rev acc
  in
  take_aux [] n lst

(* CRC32 checksum calculation following Kraken specification *)
let calculate_crc32_checksum (symbol: string) (bids: price_level list) (asks: price_level list) : int32 =
  (* Take top 10 bids and asks *)
  let top_bids = take (min 10 (List.length bids)) bids in
  let top_asks = take (min 10 (List.length asks)) asks in
  
  (* Helper function to remove leading zeros *)
  let remove_leading_zeros s =
    let rec remove_zeros i =
      if i >= String.length s then "0"
      else if s.[i] = '0' then remove_zeros (i + 1)
      else String.sub s i (String.length s - i)
    in
    remove_zeros 0
  in
  
  (* Format a price level according to Kraken spec *)
  let format_price_level (level: price_level) : string =
    (* Use original string representations to avoid precision loss *)
    let price_str = level.price_str in
    let qty_str = level.qty_str in
    
    (* Remove decimal point and leading zeros from price *)
    let price_no_decimal = String.concat "" (String.split_on_char '.' price_str) in
    let price_final = remove_leading_zeros price_no_decimal in
    
    (* Remove decimal point and leading zeros from qty *)
    let qty_no_decimal = String.concat "" (String.split_on_char '.' qty_str) in
    let qty_final = remove_leading_zeros qty_no_decimal in
    
    price_final ^ qty_final
  in
  
  (* Generate asks string (sorted low to high) *)
  let asks_string = String.concat "" (List.map format_price_level top_asks) in
  
  (* Generate bids string (sorted high to low) *)
  let bids_string = String.concat "" (List.map format_price_level top_bids) in
  
  (* Concatenate asks + bids *)
  let combined_string = asks_string ^ bids_string in
  
  (* Debug logging for checksum calculation *)
  Lwt_log_core.debug ~section 
    (Printf.sprintf "CRC32 checksum calculation for %s: combined_string length = %d" 
       symbol (String.length combined_string)) |> ignore;
  
  (* Calculate proper CRC32 checksum *)
  let crc32_table = Array.make 256 0l in
  
  (* Initialize CRC32 table *)
  for i = 0 to 255 do
    let rec calc_crc crc bit =
      if bit = 8 then crc
      else if Int32.logand crc 1l = 1l then
        calc_crc (Int32.logxor (Int32.shift_right_logical crc 1) 0xEDB88320l) (bit + 1)
      else
        calc_crc (Int32.shift_right_logical crc 1) (bit + 1)
    in
    crc32_table.(i) <- calc_crc (Int32.of_int i) 0
  done;
  
  (* Calculate CRC32 of string *)
  let crc32_string s =
    let crc = ref 0xFFFFFFFFl in
    String.iter (fun c ->
      let byte = Char.code c in
      let table_idx = Int32.to_int (Int32.logand (Int32.logxor !crc (Int32.of_int byte)) 0xFFl) in
      crc := Int32.logxor (Int32.shift_right_logical !crc 8) crc32_table.(table_idx)
    ) s;
    Int32.logxor !crc 0xFFFFFFFFl
  in
  
  crc32_string combined_string

(* Sort price levels *)
let sort_bids (levels: price_level list) : price_level list =
  (* Sort bids in descending order (highest price first) *)
  List.sort (fun a b -> Float.compare b.price a.price) levels

let sort_asks (levels: price_level list) : price_level list =
  (* Sort asks in ascending order (lowest price first) *)
  List.sort (fun a b -> Float.compare a.price b.price) levels

(* Update orderbook with new levels *)
let update_orderbook_levels (current_levels: price_level list) (updates: price_level list) : price_level list =
  (* Create a map of current levels *)
  let level_map = Hashtbl.create (List.length current_levels) in
  List.iter (fun level ->
    Hashtbl.replace level_map level.price level
  ) current_levels;
  
  (* Apply updates *)
  List.iter (fun update ->
    if update.qty = 0.0 then
      (* Remove level if quantity is 0 *)
      Hashtbl.remove level_map update.price
    else
      (* Update or add level *)
      Hashtbl.replace level_map update.price update
  ) updates;
  
  (* Convert back to list *)
  Hashtbl.fold (fun _ level acc -> level :: acc) level_map []

(* Validate checksum *)
let validate_checksum (book: book_data) : bool =
  let sorted_bids = sort_bids book.bids in
  let sorted_asks = sort_asks book.asks in
  let calculated_checksum = calculate_crc32_checksum book.symbol sorted_bids sorted_asks in
  let result = Int32.equal calculated_checksum book.checksum in
  if not result then
    Lwt_log_core.warning ~section 
      (Printf.sprintf "Checksum mismatch for %s: calculated=0x%lx, expected=0x%lx" 
         book.symbol calculated_checksum book.checksum) |> ignore;
  result

(* Process book snapshot *)
let process_book_snapshot (book_data: book_data) : unit Lwt.t =
  Lwt_log_core.debug ~section 
    (Printf.sprintf "Processing book snapshot for %s" book_data.symbol) >>= fun () ->
  
  (* Validate checksum *)
  if not (validate_checksum book_data) then (
    Lwt_log_core.error ~section 
      (Printf.sprintf "Checksum validation failed for %s snapshot" book_data.symbol) >>= fun () ->
    Lwt.return_unit
  ) else (
    let sorted_bids = sort_bids book_data.bids in
    let sorted_asks = sort_asks book_data.asks in
    
    let orderbook_record : orderbook = {
      symbol = book_data.symbol;
      bids = sorted_bids;
      asks = sorted_asks;
      checksum = book_data.checksum;
      timestamp = book_data.timestamp;
    } in
    
    Hashtbl.replace orderbooks book_data.symbol orderbook_record;
    
    Lwt_log_core.info ~section 
      (Printf.sprintf "Book snapshot processed for %s: %d bids, %d asks" 
         book_data.symbol (List.length sorted_bids) (List.length sorted_asks)) >>= fun () ->
    log_top_of_book_update book_data.symbol sorted_bids sorted_asks
  )

(* Process book update *)
let process_book_update (book_data: book_data) : unit Lwt.t =
  Lwt_log_core.debug ~section 
    (Printf.sprintf "Processing book update for %s" book_data.symbol) >>= fun () ->
  
  match Hashtbl.find_opt orderbooks book_data.symbol with
  | None ->
    Lwt_log_core.warning ~section 
      (Printf.sprintf "Received update for unknown symbol %s" book_data.symbol) >>= fun () ->
    Lwt.return_unit
  | Some current_book ->
    (* Update bids and asks *)
    let updated_bids = update_orderbook_levels current_book.bids book_data.bids in
    let updated_asks = update_orderbook_levels current_book.asks book_data.asks in
    
    (* Sort updated levels *)
    let sorted_bids = sort_bids updated_bids in
    let sorted_asks = sort_asks updated_asks in
    
    (* Create updated book data for validation *)
    let updated_book_data = {
      book_data with
      bids = sorted_bids;
      asks = sorted_asks;
    } in
    
    (* Validate checksum *)
    if not (validate_checksum updated_book_data) then (
      Lwt_log_core.error ~section 
        (Printf.sprintf "Checksum validation failed for %s update" book_data.symbol) >>= fun () ->
      Lwt.return_unit
    ) else (
      let updated_orderbook_record : orderbook = {
        symbol = book_data.symbol;
        bids = sorted_bids;
        asks = sorted_asks;
        checksum = book_data.checksum;
        timestamp = book_data.timestamp;
      } in
      
      Hashtbl.replace orderbooks book_data.symbol updated_orderbook_record;
      
      Lwt_log_core.debug ~section 
        (Printf.sprintf "Book update processed for %s: %d bids, %d asks" 
           book_data.symbol (List.length sorted_bids) (List.length sorted_asks)) >>= fun () ->
      log_top_of_book_update book_data.symbol sorted_bids sorted_asks
    )

(* Handle book message from WebSocket *)
let handle_book_message ?(get_precisions = fun _ -> None) (json: Json.t) : unit Lwt.t =
  (* Parse the book response with custom parsing for data *)
  let open Yojson.Safe.Util in
  try
    let channel = json |> member "channel" |> to_string in
    let msg_type = json |> member "type" |> to_string in
    let data_list = json |> member "data" |> to_list in
    
    if channel = "book" then
      Lwt_list.iter_s (fun data_json ->
        match book_data_of_yojson ~get_precisions data_json with
        | Ok book_data ->
          (match msg_type with
           | "snapshot" -> process_book_snapshot book_data
           | "update" -> process_book_update book_data
           | _ ->
             Lwt_log_core.warning ~section 
               (Printf.sprintf "Unknown book message type: %s" msg_type) >>= fun () ->
             Lwt.return_unit)
        | Error err ->
          Lwt_log_core.error ~section 
            (Printf.sprintf "Failed to parse book data: %s" err) >>= fun () ->
          Lwt.return_unit
      ) data_list
    else
      Lwt_log_core.warning ~section "Received non-book message in book handler" >>= fun () ->
      Lwt.return_unit
  with
  | exn ->
    Lwt_log_core.error ~section 
      (Printf.sprintf "Failed to parse book message: %s" (Printexc.to_string exn)) >>= fun () ->
    Lwt.return_unit

(* Generate Book events from orderbook *)
let generate_book_events (symbol: string) : Core.market_event list =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> []
  | Some book ->
    match book.bids, book.asks with
    | top_bid :: _, top_ask :: _ ->
      let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
      let price_prec = 8 in (* Default precision, should be retrieved from instrument data *)
      let bid_price = Primitives.Price.of_string_exn ~scale:price_prec (Printf.sprintf "%.8f" top_bid.price) in
      let ask_price = Primitives.Price.of_string_exn ~scale:price_prec (Printf.sprintf "%.8f" top_ask.price) in
      [Core.Book { symbol; bid = bid_price; ask = ask_price; ts }]
    | _ -> []

(* Public API functions *)

(* Get current orderbook *)
let get_orderbook (symbol: string) : orderbook option =
  Hashtbl.find_opt orderbooks symbol

(* Get best bid and ask *)
let get_best_bid_ask (symbol: string) : (float * float) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book ->
    match book.bids, book.asks with
    | top_bid :: _, top_ask :: _ -> Some (top_bid.price, top_ask.price)
    | _ -> None

(* Get detailed top-of-book information *)
let get_top_of_book (symbol: string) : (price_level * price_level) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book ->
    match book.bids, book.asks with
    | top_bid :: _, top_ask :: _ -> Some (top_bid, top_ask)
    | _ -> None

(* Get top N levels *)
let get_top_levels (symbol: string) (n: int) : (price_level list * price_level list) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book ->
    let top_bids = take (min n (List.length book.bids)) book.bids in
    let top_asks = take (min n (List.length book.asks)) book.asks in
    Some (top_bids, top_asks)

(* Clear orderbook data *)
let clear_orderbook (symbol: string) : unit =
  Hashtbl.remove orderbooks symbol;
  Hashtbl.remove previous_top_of_book symbol

(* Clear all orderbooks *)
let clear_all_orderbooks () : unit =
  Hashtbl.clear orderbooks;
  Hashtbl.clear previous_top_of_book

(* Get all tracked symbols *)
let get_tracked_symbols () : string list =
  Hashtbl.fold (fun symbol _ acc -> symbol :: acc) orderbooks []

(* Check if symbol has orderbook *)
let has_orderbook (symbol: string) : bool =
  Hashtbl.mem orderbooks symbol

(* Get orderbook statistics *)
let get_orderbook_stats (symbol: string) : (int * int) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book -> Some (List.length book.bids, List.length book.asks)

(* Debug: Print orderbook state *)
let debug_print_orderbook (symbol: string) : unit Lwt.t =
  match Hashtbl.find_opt orderbooks symbol with
  | None ->
    Lwt_log_core.debug ~section 
      (Printf.sprintf "No orderbook data for %s" symbol)
  | Some book ->
    let top_bids = take (min 5 (List.length book.bids)) book.bids in
    let top_asks = take (min 5 (List.length book.asks)) book.asks in
    let bid_str = String.concat ", " (List.map (fun l -> Printf.sprintf "%.8f@%.8f" l.price l.qty) top_bids) in
    let ask_str = String.concat ", " (List.map (fun l -> Printf.sprintf "%.8f@%.8f" l.price l.qty) top_asks) in
    Lwt_log_core.debug ~section 
      (Printf.sprintf "Orderbook for %s - Bids: [%s] Asks: [%s] Checksum: %ld" 
         symbol bid_str ask_str book.checksum)
