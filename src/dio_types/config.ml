(* src/dio_types/config.ml *)
open Primitives


let section = Lwt_log_core.Section.make "dio_types.config" 

(* Helper to format error messages consistently - wraps Printf.sprintf for uniform error formatting *)
let format_error_message fmt = Printf.sprintf fmt

(** Trading strategy types available for assets *)
type strategy_type =
  | Grid      (** Grid trading: buys/sells at fixed price intervals with profit-taking multipliers *)
  | Orderbook (** Orderbook-based: maintains positions based on orderbook depth and min USD balance *)

let strategy_type_to_yojson = function
  | Grid -> `String "Grid"
  | Orderbook -> `String "Orderbook"

let strategy_type_of_yojson = function
  | `String "Grid" -> Ok Grid
  | `String "Orderbook" -> Ok Orderbook
  | _ -> Error "Invalid strategy type"

let strategy_type_of_yojson_exn json =
  match strategy_type_of_yojson json with
  | Ok v -> v
  | Error msg -> 
    Lwt_main.run (Lwt_log_core.error_f ~section "Failed to parse strategy_type: %s" msg); 
    failwith msg 

(** Configuration for a single tradable asset *)
type asset_cfg = {
  symbol        : symbol;           (** Trading pair symbol (e.g., "BTC/USD") *)
  qty           : Qty.t;            (** Base quantity to trade per order *)
  grid_interval : Fixed.t option [@yojson.optional] [@yojson.default None];   (** Grid: price interval between buy/sell levels. Required for Grid strategy *)
  sell_mult     : Fixed.t option [@yojson.optional] [@yojson.default None];   (** Grid: profit-taking multiplier (< 1.0). Required for Grid strategy *)
  min_usd_balance : Fixed.t option [@yojson.optional] [@yojson.default None]; (** Orderbook: minimum USD balance to maintain. Required for Orderbook strategy *)
  strategy      : strategy_type;    (** Trading strategy to use for this asset *)
} [@@deriving yojson { exn = true }]   

(** Runtime configuration loaded from JSON config file *)
type runtime_cfg = {
  assets      : asset_cfg list;   (** List of assets to trade with their configurations *)
  queues_cap  : int;              (** Maximum capacity for internal message queues *)
  profit_threshold_pct : float;   (** Minimum profit percentage threshold (default 0.10%) *)
} [@@deriving yojson { exn = true }]

(** Engine configuration for connecting to Kraken exchange *)
type engine_config = {
  ws_host: string;              (** WebSocket hostname (e.g., "ws.kraken.com") *)
  ws_port: int;                 (** WebSocket port number *)
  ws_path: string;              (** WebSocket path endpoint *)
  symbols: string list;         (** List of trading pair symbols to subscribe to *)
  auth_token: string option;    (** Optional authentication token for private feeds *)
  kraken_api_key : string;      (** Kraken API key for authenticated requests *)
  kraken_api_secret : string;   (** Kraken API secret for request signing *)
}

(** --- Validation Functions --- *)

(** Validates a single asset configuration based on its strategy requirements.
    Returns Ok () if valid, Error msg with detailed validation failures. *)
let validate_asset_cfg (asset : asset_cfg) : (unit, string) result =
  let open Primitives in
  let errors = ref [] in
  
  if not (Qty.is_positive asset.qty) then
    errors := (format_error_message "Asset '%s': qty must be positive." asset.symbol) :: !errors;
  
  (* Strategy-specific validation: Grid requires grid_interval and sell_mult, Orderbook requires min_usd_balance *)
  (match asset.strategy with
  | Grid ->
      (match asset.grid_interval with
      | Some grid_interval ->
          if not (Fixed.is_positive grid_interval) then
            errors := (format_error_message "Asset '%s' (Grid): grid_interval must be positive." asset.symbol) :: !errors
      | None ->
          errors := (format_error_message "Asset '%s' (Grid): grid_interval is required." asset.symbol) :: !errors);
      
      (match asset.sell_mult with
      | Some sell_mult ->
          if not (Fixed.is_positive sell_mult) then
            errors := (format_error_message "Asset '%s' (Grid): sell_mult must be positive." asset.symbol) :: !errors
          else if Fixed.(<=) (Fixed.one sell_mult.scale) sell_mult then
            errors := (format_error_message "Asset '%s' (Grid): sell_mult should typically be less than 1.0 for profit-taking." asset.symbol) :: !errors
      | None ->
          errors := (format_error_message "Asset '%s' (Grid): sell_mult is required." asset.symbol) :: !errors);

      if asset.min_usd_balance <> None then
        errors := (format_error_message "Asset '%s' (Grid): min_usd_balance is not applicable." asset.symbol) :: !errors
  | Orderbook ->
      (match asset.min_usd_balance with
      | Some min_usd_balance ->
          if not (Fixed.is_non_negative min_usd_balance) then
            errors := (format_error_message "Asset '%s' (Orderbook): min_usd_balance must be non-negative." asset.symbol) :: !errors
      | None ->
          errors := (format_error_message "Asset '%s' (Orderbook): min_usd_balance is required." asset.symbol) :: !errors);

      if asset.grid_interval <> None then
        errors := (format_error_message "Asset '%s' (Orderbook): grid_interval is not applicable." asset.symbol) :: !errors;
      
      if asset.sell_mult <> None then
        errors := (format_error_message "Asset '%s' (Orderbook): sell_mult is not applicable." asset.symbol) :: !errors
  );

  if !errors = [] then
    Ok ()
  else
    Error (String.concat "\n" (List.rev !errors))

(** Validates the complete runtime configuration including all assets.
    Checks for duplicate symbols, validates each asset, and ensures required fields. *)
let validate_runtime_cfg (cfg : runtime_cfg) : (unit, string) result =
  let errors = ref [] in

  if cfg.assets = [] then
    errors := "Config must contain at least one asset." :: !errors;

  if cfg.queues_cap <= 0 then
    errors := "queues_cap must be positive." :: !errors;

  (* Check for duplicate symbols - extract all symbols, deduplicate, and compare lengths *)
  let symbols = List.map (fun a -> a.symbol) cfg.assets in
  let unique_symbols = List.sort_uniq String.compare symbols in
  if List.length symbols <> List.length unique_symbols then
    errors := "Duplicate symbols found in assets configuration." :: !errors;

  (* Validate each asset individually and collect any validation errors *)
  let asset_errors = List.filter_map (fun asset ->
    match validate_asset_cfg asset with
    | Ok () -> None
    | Error msg -> Some msg
  ) cfg.assets in

  let all_errors = !errors @ asset_errors in

  if all_errors = [] then
    Ok ()
  else
    Error (String.concat "\n" (List.rev all_errors))

