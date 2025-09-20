(**
 * Kraken Orderbook Management
 *
 * Handles real-time orderbook data from Kraken WebSocket API, including:
 * - Parsing and validating orderbook snapshots and updates
 * - Maintaining sorted, checksum-validated orderbook state
 * - Thread-safe operations with per-symbol mutexes
 * - Top-of-book change detection and logging
 *)

open Lwt.Infix
module Json = Yojson.Safe
module JsonUtil = Yojson.Safe.Util
open Dio_types
open Lwt_log_core

let section = Section.make "kraken_orderbook"
let subscription_depth = 25

(** Convert list of results to result of list, failing on first error *)
let sequence_results (lst : ('a, 'e) result list) : ('a list, 'e) result =
  let folder acc res =
    match acc, res with
    | Result.Ok acc_lst, Result.Ok x -> Result.Ok (x :: acc_lst)
    | Result.Error e, _ -> Result.Error e
    | _, Result.Error e -> Result.Error e
  in
  List.fold_left folder (Result.Ok []) lst |> Result.map List.rev

(** Individual price level with both float and string representations *)
type price_level = {
  price: float;
  qty: float;
  price_str: string;
  qty_str: string;
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Complete orderbook state for a symbol *)
type orderbook = {
  symbol: string;
  bids: price_level list;  (** Sorted descending by price *)
  asks: price_level list;  (** Sorted ascending by price *)
  checksum: int32;         (** Kraken CRC32 checksum *)
  timestamp: string option;
}

(** Raw orderbook data from WebSocket messages *)
type book_data = {
  asks: price_level list;
  bids: price_level list;
  checksum: int32;
  symbol: string;
  timestamp: string option;
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Parse book_data from JSON with precision handling for price/quantity formatting *)
let book_data_of_yojson ?(get_precisions = fun _ -> None) json : (book_data, string) result =
  let open Yojson.Safe.Util in
  try
    let symbol = json |> member "symbol" |> to_string in
    let (price_precision, qty_precision) = 
      match get_precisions symbol with
      | Some (price_prec, qty_prec) -> (price_prec, qty_prec)
      | None -> (8, 8) 
    in
    
    let parse_price_level json : (price_level, string) result = 
      let price_str_res = 
        match json |> member "price" with
        | `String s -> Result.Ok s
        | `Float f -> Result.Ok (Printf.sprintf "%.*f" price_precision f)
        | `Int i -> Result.Ok (string_of_int i)
        | _ -> Result.Error "Invalid price format"
      in
      let qty_str_res = 
        match json |> member "qty" with
        | `String s -> Result.Ok s
        | `Float f -> Result.Ok (Printf.sprintf "%.*f" qty_precision f)
        | `Int i -> Result.Ok (string_of_int i)
        | _ -> Result.Error "Invalid qty format"
      in
      
      match price_str_res with
      | Result.Ok price_str ->
        (match qty_str_res with
        | Result.Ok qty_str ->
          let price_float = Float.of_string price_str in
          let qty_float = Float.of_string qty_str in
          Result.Ok { price = price_float; qty = qty_float; price_str; qty_str }
        | Result.Error e -> Result.Error e)
      | Result.Error e -> Result.Error e
    in
    
    let asks_results = json |> member "asks" |> to_list |> List.map parse_price_level in
    let bids_results = json |> member "bids" |> to_list |> List.map parse_price_level in
    
    match sequence_results asks_results with
    | Result.Ok asks ->
      (match sequence_results bids_results with
      | Result.Ok bids ->
        Result.Ok {
          asks;
          bids;
          checksum = json |> member "checksum" |> Json.to_string |> Int64.of_string |> Int64.to_int32;
          symbol;
          timestamp = json |> member "timestamp" |> to_string_option;
        }
      | Result.Error e -> Result.Error e)
    | Result.Error e -> Result.Error e
  with 
  | Yojson.Safe.Util.Type_error (msg, _) -> Result.Error ("book_data: " ^ msg)
  | exn -> Result.Error ("book_data: " ^ Printexc.to_string exn)

(** Parse book_data from JSON, raising exception on failure *)
let book_data_of_yojson_exn ?(get_precisions = fun _ -> None) json : book_data Lwt.t =
  match book_data_of_yojson ~get_precisions json with
  | Result.Ok v -> Lwt.return v
  | Result.Error msg ->
    error_f ~section "Failed to parse book data: %s" msg >>= fun () ->
    Lwt.fail (Failure msg)

(** WebSocket book message structure *)
type book_response = {
  channel: string;
  type_: string; [@key "type"]
  data: book_data list;
} [@@deriving yojson { strict = false }] [@@yojson.allow_extra_fields]

(** Global storage for orderbook state by symbol *)
let orderbooks : (string, orderbook) Hashtbl.t = Hashtbl.create 16

(** Per-symbol mutexes to prevent race conditions during updates *)
let symbol_locks : (string, Lwt_mutex.t) Hashtbl.t = Hashtbl.create 16

(** Get or create mutex for symbol-specific locking *)
let get_lock symbol =
  match Hashtbl.find_opt symbol_locks symbol with
  | Some lock -> lock
  | None ->
      let lock = Lwt_mutex.create () in
      Hashtbl.add symbol_locks symbol lock;
      lock

(** Cache of previous top-of-book prices for change detection *)
let previous_top_of_book : (string, (float * float)) Hashtbl.t = Hashtbl.create 16

(** Log top-of-book price changes with spread information *)
let log_top_of_book_update (symbol: string) (sorted_bids: price_level list) (sorted_asks: price_level list) : unit Lwt.t =
  match sorted_bids, sorted_asks with
  | top_bid :: _, top_ask :: _ ->
    let current_top = (top_bid.price, top_ask.price) in
    let previous_top = Hashtbl.find_opt previous_top_of_book symbol in
    
    (* Check if top-of-book changed *)
    let should_log = match previous_top with
      | None -> true 
      | Some (prev_bid, prev_ask) -> 
        not (Float.equal prev_bid top_bid.price && Float.equal prev_ask top_ask.price)
    in
    
    if should_log then (
      Hashtbl.replace previous_top_of_book symbol current_top;
      info_f ~section
        "Top-of-book update for %s: bid=%.8f@%.8f ask=%.8f@%.8f spread=%.8f"
           symbol top_bid.price top_bid.qty top_ask.price top_ask.qty (top_ask.price -. top_bid.price)
    ) else
      Lwt.return_unit
  | [], _ ->
    warning_f ~section "No bids available for %s" symbol
  | _, [] ->
    warning_f ~section "No asks available for %s" symbol

(** Take first n elements from list, or all elements if fewer than n *)
let take n lst =
  let rec take_aux acc n = function
    | [] -> List.rev acc
    | h :: t when n > 0 -> take_aux (h :: acc) (n - 1) t
    | _ -> List.rev acc
  in
  take_aux [] n lst

(** Calculate CRC32 checksum per Kraken specification using top 10 levels *)
let calculate_crc32_checksum (_symbol: string) (bids: price_level list) (asks: price_level list) : int32 =
  (* Take top 10 bids and asks *)
  let top_bids = take (min 10 (List.length bids)) bids in
  let top_asks = take (min 10 (List.length asks)) asks in
  
  (* remove leading zeros *)
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

  (* Calculate proper CRC32 checksum *)
  let crc32_table : int32 array = Array.make 256 0l in
  
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

(** Sort bids in descending order (highest price first) *)
let sort_bids (levels: price_level list) : price_level list =
  List.sort (fun a b -> Float.compare b.price a.price) levels

(** Sort asks in ascending order (lowest price first) *)
let sort_asks (levels: price_level list) : price_level list =
  List.sort (fun a b -> Float.compare a.price b.price) levels

(** Apply price level updates to existing orderbook, handling additions/modifications/removals *)
let update_orderbook_levels (current_levels: price_level list) (updates: price_level list) : price_level list =
  (* Create a map of current levels using price_str as key to avoid float precision issues *)
  let level_map = Hashtbl.create (List.length current_levels) in
  List.iter (fun level ->
    Hashtbl.replace level_map level.price_str level
  ) current_levels;

  (* Apply updates *)
  List.iter (fun update ->
    if update.qty = 0.0 then
      (* Remove level if quantity is 0 *)
      Hashtbl.remove level_map update.price_str
    else
      (* Update or add level *)
      Hashtbl.replace level_map update.price_str update
  ) updates;

  (* Convert back to list *)
  Hashtbl.fold (fun _ level acc -> level :: acc) level_map []

(** Validate orderbook checksum against calculated CRC32 *)
let validate_checksum (book: book_data) : bool =
  let sorted_bids = sort_bids book.bids in
  let sorted_asks = sort_asks book.asks in
  let calculated_checksum = calculate_crc32_checksum book.symbol sorted_bids sorted_asks in
  let result = Int32.equal calculated_checksum book.checksum in
  result

(** Process complete orderbook snapshot, replacing existing state *)
let process_book_snapshot (book_data: book_data) : unit Lwt.t =
  let lock = get_lock book_data.symbol in
  Lwt_mutex.with_lock lock (fun () ->
    debug_f ~section
      "Processing book snapshot for %s" book_data.symbol >>= fun () ->
    
    (* Validate checksum *)
    if not (validate_checksum book_data) then (
      error_f ~section
        "Checksum validation failed for %s snapshot" book_data.symbol >>= fun () ->
      Lwt.return_unit
    ) else (
      let sorted_bids = sort_bids book_data.bids in
      let sorted_asks = sort_asks book_data.asks in
      
      let truncated_bids = take subscription_depth sorted_bids in
      let truncated_asks = take subscription_depth sorted_asks in

      let orderbook_record : orderbook = {
        symbol = book_data.symbol;
        bids = truncated_bids;
        asks = truncated_asks;
        checksum = book_data.checksum;
        timestamp = book_data.timestamp;
      } in
      
      Hashtbl.replace orderbooks book_data.symbol orderbook_record;
      
      info_f ~section
        "Book snapshot processed for %s: %d bids, %d asks"
           book_data.symbol (List.length truncated_bids) (List.length truncated_asks) >>= fun () ->
      log_top_of_book_update book_data.symbol truncated_bids truncated_asks
    )
  )

(** Process batch of orderbook updates for a symbol *)
let process_aggregated_updates (symbol : string) (updates : book_data list) : unit Lwt.t =
  let lock = get_lock symbol in
  Lwt_mutex.with_lock lock (fun () ->
    match updates with
    | [] -> Lwt.return_unit
    | _ ->
      let final_update = List.hd (List.rev updates) in
      let final_checksum = final_update.checksum in
      let final_timestamp = final_update.timestamp in

      let all_bids_updates = List.concat_map (fun u -> u.bids) updates in
      let all_asks_updates = List.concat_map (fun u -> u.asks) updates in

      match Hashtbl.find_opt orderbooks symbol with
      | None ->
          warning_f ~section "Received aggregated update for unknown symbol %s" symbol
      | Some current_book ->
          let updated_bids = update_orderbook_levels current_book.bids all_bids_updates in
          let updated_asks = update_orderbook_levels current_book.asks all_asks_updates in

          let sorted_bids = sort_bids updated_bids in
          let sorted_asks = sort_asks updated_asks in

          let truncated_bids = take subscription_depth sorted_bids in
          let truncated_asks = take subscription_depth sorted_asks in

          let validation_book_data = {
            symbol;
            bids = truncated_bids;
            asks = truncated_asks;
            checksum = final_checksum;
            timestamp = final_timestamp;
          } in

          if not (validate_checksum validation_book_data) then (
            error_f ~section "Checksum validation failed for %s aggregated update" symbol
          ) else (
            let updated_orderbook_record : orderbook = {
              symbol;
              bids = truncated_bids;
              asks = truncated_asks;
              checksum = final_checksum;
              timestamp = final_timestamp;
            } in
            Hashtbl.replace orderbooks symbol updated_orderbook_record;
            debug_f ~section "Aggregated book update processed for %s: %d bids, %d asks"
              symbol (List.length truncated_bids) (List.length truncated_asks) >>= fun () ->
            log_top_of_book_update symbol truncated_bids truncated_asks
          )
    )

(** Process incoming WebSocket book message (snapshot or update) *)
let handle_book_message ?(get_precisions = fun _ -> None) (json: Json.t) : unit Lwt.t =
  (* Parse the book response with custom parsing for data *)
  let open Yojson.Safe.Util in
  Lwt.catch (fun () ->
    let channel = json |> member "channel" |> to_string in
    let msg_type = json |> member "type" |> to_string in
    let data_list = json |> member "data" |> to_list in
    
    if channel = "book" then
      match msg_type with
      | "snapshot" ->
        Lwt_list.iter_s (fun data_json ->
          match book_data_of_yojson ~get_precisions data_json with
          | Result.Ok book_data -> process_book_snapshot book_data
          | Result.Error err -> error_f ~section "Failed to parse book snapshot: %s" err
        ) data_list
      | "update" ->
        let parse_results = List.map (book_data_of_yojson ~get_precisions) data_list in
        (match sequence_results parse_results with
         | Result.Error err ->
             error_f ~section "Failed to parse one or more book data updates: %s" err
         | Result.Ok all_updates ->
             let updates_by_symbol =
               List.fold_left
                 (fun acc (update : book_data) ->
                   let updates = try Hashtbl.find acc update.symbol with Not_found -> [] in
                   Hashtbl.replace acc update.symbol (update :: updates);
                   acc)
                 (Hashtbl.create 4) all_updates
             in
             Hashtbl.fold
               (fun symbol updates_for_symbol acc_lwt ->
                 acc_lwt >>= fun () ->
                 process_aggregated_updates symbol (List.rev updates_for_symbol))
               updates_by_symbol (Lwt.return_unit)
        )
      | _ ->
        warning_f ~section "Unknown book message type: %s" msg_type
    else
      warning_f ~section "Received non-book message in book handler"
  ) (fun exn ->
    error_f ~section 
      "Failed to parse book message: %s" (Printexc.to_string exn) >>= fun () ->
    Lwt.return_unit
  )

(** Generate market events from current orderbook state *)
let generate_book_events (symbol: string) : Core.market_event list =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> []
  | Some book ->
    match book.bids, book.asks with
    | top_bid :: _, top_ask :: _ ->
      let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
      let price_prec = 8 in 
      let bid_price = Primitives.Price.of_string_exn ~scale:price_prec (Printf.sprintf "%.8f" top_bid.price) in
      let ask_price = Primitives.Price.of_string_exn ~scale:price_prec (Printf.sprintf "%.8f" top_ask.price) in
      [Core.Book { symbol; bid = bid_price; ask = ask_price; ts }]
    | _ -> []

(** Get complete orderbook for symbol *)
let get_orderbook (symbol: string) : orderbook option =
  Hashtbl.find_opt orderbooks symbol

(** Get best bid/ask prices as floats *)
let get_best_bid_ask (symbol: string) : (float * float) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book ->
    match book.bids, book.asks with
    | top_bid :: _, top_ask :: _ -> Some (top_bid.price, top_ask.price)
    | _ -> None

(** Get detailed top bid/ask price levels *)
let get_top_of_book (symbol: string) : (price_level * price_level) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book ->
    match book.bids, book.asks with
    | top_bid :: _, top_ask :: _ -> Some (top_bid, top_ask)
    | _ -> None

(** Get top N bid/ask levels *)
let get_top_levels (symbol: string) (n: int) : (price_level list * price_level list) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book ->
    let top_bids = take (min n (List.length book.bids)) book.bids in
    let top_asks = take (min n (List.length book.asks)) book.asks in
    Some (top_bids, top_asks)

(** Remove orderbook data for symbol *)
let clear_orderbook (symbol: string) : unit =
  Hashtbl.remove orderbooks symbol;
  Hashtbl.remove previous_top_of_book symbol

(** Remove all orderbook data *)
let clear_all_orderbooks () : unit =
  Hashtbl.clear orderbooks;
  Hashtbl.clear previous_top_of_book

(** Get list of all tracked symbols *)
let get_tracked_symbols () : string list =
  Hashtbl.fold (fun symbol _ acc -> symbol :: acc) orderbooks []

(** Check if orderbook exists for symbol *)
let has_orderbook (symbol: string) : bool =
  Hashtbl.mem orderbooks symbol

(** Get orderbook level counts (bids, asks) *)
let get_orderbook_stats (symbol: string) : (int * int) option =
  match Hashtbl.find_opt orderbooks symbol with
  | None -> None
  | Some book -> Some (List.length book.bids, List.length book.asks)

(** Debug print orderbook state for symbol *)
let debug_print_orderbook (symbol: string) : unit Lwt.t =
  match Hashtbl.find_opt orderbooks symbol with
  | None ->
    debug_f ~section 
      "No orderbook data for %s" symbol
  | Some book ->
    let top_bids = take (min 5 (List.length book.bids)) book.bids in
    let top_asks = take (min 5 (List.length book.asks)) book.asks in
    let bid_str = String.concat ", " (List.map (fun l -> Printf.sprintf "%.8f@%.8f" l.price l.qty) top_bids) in
    let ask_str = String.concat ", " (List.map (fun l -> Printf.sprintf "%.8f@%.8f" l.price l.qty) top_asks) in
    debug_f ~section 
      "Orderbook for %s - Bids: [%s] Asks: [%s] Checksum: %ld" 
         symbol bid_str ask_str book.checksum
