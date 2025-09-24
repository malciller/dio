(**
 * Kraken Fee Cache
 *
 * Provides cached access to maker/taker fee rates for Kraken trading pairs by
 * querying the TradeVolume REST endpoint. Results are cached for a configurable
 * TTL to minimise authenticated REST calls while keeping fee data reasonably
 * fresh.
 *)

open Lwt.Infix
open Lwt_log_core
open Cohttp_lwt_unix
open Dio_types

module Json = Yojson.Safe
module JsonUtil = Yojson.Safe.Util

let section = Section.make "kraken_fee_cache"

type fee_info = {
  maker_fee: float option;
  taker_fee: float option;
  last_updated: float;
}

let fee_cache : (string, fee_info) Hashtbl.t = Hashtbl.create 128

let cache_ttl_seconds = infinity  (** Fee cache entries persist until explicitly refreshed *)
let backoff_seconds = 60.0      (** Wait before retry when rate-limited *)

type pair_metadata = {
  canonical_pair: string;
  friendly_aliases: string list;
  fallback_maker: float option;
  fallback_taker: float option; 
}

let pair_metadata_by_friendly : (string, pair_metadata) Hashtbl.t = Hashtbl.create 256
let pair_metadata_by_canonical : (string, pair_metadata) Hashtbl.t = Hashtbl.create 256

let metadata_mutex = Lwt_mutex.create ()
let metadata_last_fetch = ref 0.0
let metadata_ttl_seconds = 3600.0

let string_of_json_option f = function
  | Some v -> f v
  | None -> ""

let rec parse_fee_entry json =
  match json with
  | `Assoc assoc ->
      List.find_map (fun key ->
        match List.assoc_opt key assoc with
        | Some (`Float f) -> Some (f /. 100.0)
        | Some (`Int i) -> Some (float_of_int i /. 100.0)
        | Some (`Intlit s | `String s) ->
            (try Some ((float_of_string (String.trim s)) /. 100.0) with _ -> None)
        | _ -> None
      ) ["fee"; "fee_volume"; "volume_fee"; "volume"]
  | `List (_ :: value_json :: _) -> parse_fee_entry value_json
  | `Float f -> Some (f /. 100.0)
  | `Int i -> Some (float_of_int i /. 100.0)
  | `Intlit s | `String s ->
      (try Some ((float_of_string (String.trim s)) /. 100.0) with _ -> None)
  | _ -> None

let fetch_mutex = Lwt_mutex.create ()
let backoff_table : (string, float) Hashtbl.t = Hashtbl.create 128

let normalize_pair_symbol symbol = String.uppercase_ascii symbol

let contains_substring haystack needle =
  let len_h = String.length haystack in
  let len_n = String.length needle in
  let rec aux i =
    if len_n = 0 then true
    else if i + len_n > len_h then false
    else if String.sub haystack i len_n = needle then true
    else aux (i + 1)
  in
  aux 0

let parse_fee_rate entry =
  let open JsonUtil in
  parse_fee_entry (member "fee" entry)

let find_entry key assoc_list =
  List.find_opt (fun (k, _) -> String.equal k key) assoc_list

let build_fee_info pair fees_assoc fees_maker_assoc =
  let taker_rate =
    match find_entry pair fees_assoc with
    | Some (_, entry) -> parse_fee_rate entry
    | None -> None
  in
  let maker_rate =
    match find_entry pair fees_maker_assoc with
    | Some (_, entry) -> parse_fee_rate entry
    | None -> None
  in
  match maker_rate, taker_rate with
  | None, None -> None
  | _ ->
      Some { maker_fee = maker_rate; taker_fee = taker_rate; last_updated = Unix.gettimeofday () }

let format_fee = function
  | None -> "missing"
  | Some fee -> Printf.sprintf "%.6f" fee

let extract_assoc_member name json =
  match JsonUtil.member name json with
  | `Assoc assoc -> assoc
  | _ -> []

let strip_asset_prefix code =
  let rec aux s =
    if String.length s > 3 && (s.[0] = 'X' || s.[0] = 'Z') then
      aux (String.sub s 1 (String.length s - 1))
    else
      s
  in
  aux (String.uppercase_ascii code)

let map_special_asset = function
  | "XBT" | "XXBT" -> "BTC"
  | "XDG" | "XXDG" -> "DOGE"
  | "XETH" | "XXETH" -> "ETH"
  | "ZUSD" -> "USD"
  | other -> other

let asset_code_variants code =
  let stripped = strip_asset_prefix code in
  let mapped = map_special_asset stripped in
  List.filter (fun s -> String.length s > 0)
    [String.uppercase_ascii code; stripped; mapped]
  |> List.sort_uniq String.compare

let normalize_alias alias = alias |> String.trim |> String.uppercase_ascii

let add_alias alias acc =
  if alias = "" then acc
  else
    let normalized = normalize_alias alias in
    if List.mem normalized acc then acc else normalized :: acc

let add_alias_option alias_opt acc =
  match alias_opt with
  | None -> acc
  | Some alias -> add_alias alias acc

let combine_base_quote_aliases base_variants quote_variants =
      List.fold_left (fun acc base ->
      List.fold_left (fun acc quote ->
          let acc = add_alias (base ^ "/" ^ quote) acc in
          let acc = add_alias (base ^ quote) acc in
          let acc = add_alias (base ^ "-" ^ quote) acc in
          acc
        ) acc quote_variants
    ) [] base_variants

let extract_highest_fee tiers_json =
  let rec parse_tiers acc = function
    | [] -> acc
    | value :: rest ->
        let acc =
          match parse_fee_entry value with
          | Some fee -> fee :: acc
          | None -> acc
        in
        parse_tiers acc rest
  in
  match tiers_json with
  | `List tiers ->
      begin match parse_tiers [] tiers with
      | [] -> None
      | lst -> Some (List.fold_left max 0.0 lst)
      end
  | _ -> None

let canonical_pair_of symbol =
  let upper = normalize_pair_symbol symbol in
  match Hashtbl.find_opt pair_metadata_by_friendly upper with
  | Some meta -> Some meta.canonical_pair
  | None -> Some upper

let friendly_aliases_of_canonical canonical =
  match Hashtbl.find_opt pair_metadata_by_canonical canonical with
  | Some meta -> meta.friendly_aliases
  | None -> [canonical]

let fallback_fee_of symbol ~is_maker =
  let upper = normalize_pair_symbol symbol in
  match Hashtbl.find_opt pair_metadata_by_friendly upper with
  | Some meta -> if is_maker then meta.fallback_maker else meta.fallback_taker
  | None ->
      match Hashtbl.find_opt pair_metadata_by_canonical upper with
      | Some meta -> if is_maker then meta.fallback_maker else meta.fallback_taker
      | None -> None

let ensure_metadata_fresh () =
  let now = Unix.gettimeofday () in
  if now -. !metadata_last_fetch < metadata_ttl_seconds then Lwt.return_unit else
  Lwt_mutex.with_lock metadata_mutex (fun () ->
    let now = Unix.gettimeofday () in
    if now -. !metadata_last_fetch < metadata_ttl_seconds then Lwt.return_unit else
    let url = Uri.of_string "https://api.kraken.com/0/public/AssetPairs" in
    Client.get url >>= fun (resp, body) ->
    let status = Cohttp.Response.status resp in
    Cohttp_lwt.Body.to_string body >>= fun body_str ->
    if not (Cohttp.Code.(is_success (code_of_status status))) then
      warning_f ~section "AssetPairs request failed (%s): %s"
        (Cohttp.Code.string_of_status status) body_str >>= fun () ->
      Lwt.return_unit
    else
      try
        let json = Json.from_string body_str in
        let errors = JsonUtil.(member "error" json |> to_list |> List.filter_map to_string_option) in
        if errors <> [] then
          warning_f ~section "AssetPairs returned errors: %s" (String.concat "; " errors) >>= fun () ->
          Lwt.return_unit
        else
          let result = JsonUtil.member "result" json in
          let pairs = JsonUtil.to_assoc result in
          Hashtbl.clear pair_metadata_by_canonical;
          Hashtbl.clear pair_metadata_by_friendly;
          List.iter (fun (canonical, pair_json) ->
            let canonical_upper = String.uppercase_ascii canonical in
            let open JsonUtil in
            let altname = member "altname" pair_json |> to_string_option in
            let wsname = member "wsname" pair_json |> to_string_option in
            let base = member "base" pair_json |> to_string_option in
            let quote = member "quote" pair_json |> to_string_option in
            let maker_tiers = member "fees_maker" pair_json in
            let taker_tiers = member "fees" pair_json in
            let fallback_maker = extract_highest_fee maker_tiers in
            let fallback_taker = extract_highest_fee taker_tiers in
            let base_variants = match base with Some b -> asset_code_variants b | None -> [] in
            let quote_variants = match quote with Some q -> asset_code_variants q | None -> [] in
            let aliases =
              []
              |> add_alias canonical_upper
              |> add_alias_option altname
              |> add_alias_option wsname
            in
            let composite_aliases = combine_base_quote_aliases base_variants quote_variants in
            let all_aliases = List.fold_left (fun acc alias -> add_alias alias acc) aliases composite_aliases in
            let metadata = {
              canonical_pair = canonical_upper;
              friendly_aliases = all_aliases;
              fallback_maker;
              fallback_taker;
            } in
            Hashtbl.replace pair_metadata_by_canonical canonical_upper metadata;
            List.iter (fun alias -> Hashtbl.replace pair_metadata_by_friendly alias metadata) all_aliases
          ) pairs;
          metadata_last_fetch := now;
          info_f ~section "Loaded %d Kraken asset pair metadata entries" (Hashtbl.length pair_metadata_by_canonical)
      with ex ->
      error_f ~section "Failed to parse AssetPairs response: %s" (Printexc.to_string ex)
  )

let send_trade_volume_request (cfg : Config.engine_config) pairs =
  Kraken_common_types.nonce () >>= fun nonce ->
  let base_params = [ ("fee-info", ["true"]); ("nonce", [nonce]) ] in
  let params =
    match pairs with
    | [] -> base_params
    | _ -> ("pair", [String.concat "," pairs]) :: base_params
  in
  let sorted_params = List.sort (fun (k1, _) (k2, _) -> String.compare k1 k2) params in
  let encoded_post_data = Uri.encoded_of_query sorted_params in
  let api_path = "/0/private/TradeVolume" in
  let signature = Kraken_common_types.sign
      ~secret:cfg.kraken_api_secret
      ~path:api_path
      ~body:encoded_post_data
      ~nonce
  in
  let url = Uri.of_string (Printf.sprintf "https://%s%s" "api.kraken.com" api_path) in
  let headers = Cohttp.Header.of_list [
      ("API-Key", cfg.kraken_api_key);
      ("API-Sign", signature);
      ("Content-Type", "application/x-www-form-urlencoded");
    ]
  in
  let body = Cohttp_lwt.Body.of_string encoded_post_data in
  Client.post ~headers ~body url >>= fun (resp, body_lwt) ->
  let status = Cohttp.Response.status resp in
  Cohttp_lwt.Body.to_string body_lwt >>= fun body_str ->
  if not (Cohttp.Code.(is_success (code_of_status status))) then
    warning_f ~section "TradeVolume request failed (%s): %s"
      (Cohttp.Code.string_of_status status) body_str >>= fun () ->
    Lwt.return_none
  else
    try
      let json = Json.from_string body_str in
      let errors = JsonUtil.(member "error" json |> to_list |> List.filter_map to_string_option) in
      if errors <> [] then (
        let is_rate_limit = List.exists (fun err -> contains_substring err "Rate limit") errors in
        let log_fn = if is_rate_limit then debug_f else warning_f in
        log_fn ~section "TradeVolume returned errors: %s"
          (String.concat "; " errors) >>= fun () ->
        Lwt.return_none
      ) else
        let result_json = JsonUtil.member "result" json in
        let fees_assoc = extract_assoc_member "fees" result_json in
        let fees_maker_assoc = extract_assoc_member "fees_maker" result_json in
        let pairs_to_process =
          match pairs with
          | [] -> List.fold_left (fun acc (pair, _) -> pair :: acc) [] fees_assoc |> List.rev
          | lst -> lst
        in
        let updates =
          pairs_to_process
          |> List.filter_map (fun pair ->
                match build_fee_info pair fees_assoc fees_maker_assoc with
                | Some info -> Some (pair, info)
                | None -> None)
        in
        Lwt.return_some updates
    with ex ->
      error_f ~section "Failed to parse TradeVolume response: %s" (Printexc.to_string ex) >>= fun () ->
      Lwt.return_none

let ensure_pairs (cfg : Config.engine_config) pairs =
  let normalized_pairs =
    pairs
    |> List.map normalize_pair_symbol
    |> List.filter (fun pair -> String.length pair > 0)
    |> List.sort_uniq String.compare
  in
  if normalized_pairs = [] then Lwt.return_unit else
  ensure_metadata_fresh () >>= fun () ->
  Lwt_mutex.with_lock fetch_mutex (fun () ->
    let now = Unix.gettimeofday () in
    let pairs_to_fetch =
      normalized_pairs
      |> List.filter_map (fun pair ->
            match canonical_pair_of pair with
            | None -> None
            | Some canonical -> Some (pair, canonical))
      |> List.filter (fun (_, canonical) ->
            match Hashtbl.find_opt fee_cache canonical with
            | None -> true
            | Some info -> now -. info.last_updated > cache_ttl_seconds)
      |> List.filter (fun (_, canonical) ->
            match Hashtbl.find_opt backoff_table canonical with
            | None -> true
            | Some next_allowed -> now >= next_allowed)
    in
    match pairs_to_fetch with
    | [] -> Lwt.return_unit
    | _ ->
        let canonical_pairs = List.map snd pairs_to_fetch in
        info_f ~section "Fetching fee data for %d pair(s): %s"
          (List.length canonical_pairs)
          (String.concat ", " (List.map snd pairs_to_fetch)) >>= fun () ->
        send_trade_volume_request cfg canonical_pairs >>= function
        | None ->
            let next_retry = now +. backoff_seconds in
            List.iter (fun (_, canonical) ->
              Hashtbl.replace backoff_table canonical next_retry;
              warning_f ~section "Backing off %s fee fetch until %.0f" canonical next_retry |> ignore
            ) pairs_to_fetch;
            Lwt.return_unit
        | Some updates ->
            List.iter (fun (canonical, info) -> Hashtbl.replace fee_cache canonical info) updates;
            List.iter (fun (_, canonical) -> Hashtbl.remove backoff_table canonical) pairs_to_fetch;
            Lwt_list.iter_s (fun (canonical, info) ->
              let friendly_aliases = friendly_aliases_of_canonical canonical in
              List.iter (fun alias -> Hashtbl.replace fee_cache alias info) friendly_aliases;
              info_f ~section "Fetched fees for %s: maker=%s taker=%s"
                canonical
                (format_fee info.maker_fee)
                (format_fee info.taker_fee)
            ) updates >>= fun () ->
            Lwt.return_unit
  )

let get_fee_info pair =
  let pair = normalize_pair_symbol pair in
  Hashtbl.find_opt fee_cache pair

let get_fee_rate pair ~is_maker =
  match get_fee_info pair with
  | None -> None
  | Some info ->
      if is_maker then info.maker_fee else info.taker_fee

let last_updated pair =
  get_fee_info pair |> Option.map (fun info -> info.last_updated)

let invalidate pair =
  let pair = normalize_pair_symbol pair in
  Hashtbl.remove fee_cache pair

let all_cached_pairs () =
  Hashtbl.fold (fun pair info acc -> (pair, info) :: acc) fee_cache []

