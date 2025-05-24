open Lwt.Infix

open Dio_types.Event
open Dio_types.Primitives



let section = Lwt_log_core.Section.make "database.price_logger"

(* Define the opaque db type to match the interface *)
type db = Caqti_lwt.connection

(* Helper to get table name from symbol. Replaces '/' with '_' and ensures lowercase. *)
let table_name_of_symbol symbol =
  "prices_" ^ String.(lowercase_ascii (map (function '/' -> '_' | c -> c) symbol))

let create_table_if_not_exists conn symbol =
  let table_name = table_name_of_symbol symbol in
  let query_string = Printf.sprintf
    "CREATE TABLE IF NOT EXISTS %s (
      timestamp BIGINT PRIMARY KEY,
      source TEXT NOT NULL,
      bid TEXT NOT NULL,
      ask TEXT NOT NULL,
      mid_price TEXT NOT NULL,
      ask_qty TEXT NOT NULL,
      bid_qty TEXT NOT NULL,
      change TEXT NOT NULL,
      change_pct TEXT NOT NULL,
      high TEXT NOT NULL,
      last_price TEXT NOT NULL,
      low TEXT NOT NULL,
      volume TEXT NOT NULL,
      vwap TEXT NOT NULL
    )" table_name
  in
  let module C = (val conn : Caqti_lwt.CONNECTION) in
  let request =
    Caqti_request.create
      Caqti_type.unit
      Caqti_type.unit
      Caqti_mult.zero
      (fun _ -> Caqti_query.of_string_exn query_string)
  in
  C.exec request ()
  |> Lwt_result.map_error (fun err ->
      let err_msg = Printf.sprintf "Failed to create table %s: %s" table_name (Caqti_error.show err) in
      Lwt_log_core.error ~section err_msg |> Lwt.ignore_result;
      err_msg
    )

let init db_uri_string =
  Lwt_log_core.info ~section (Printf.sprintf "Initializing PostgreSQL database connection for %s" db_uri_string) >>= fun () ->
  let db_uri = Uri.of_string db_uri_string in
  Caqti_lwt_unix.connect db_uri
  |> Lwt_result.map_error (fun err ->
      let err_msg = Printf.sprintf "Failed to connect to database: %s" (Caqti_error.show err) in
      Lwt_log_core.error ~section err_msg |> Lwt.ignore_result;
      err_msg
    )

let log_tick conn (tick_event : tick) =
  create_table_if_not_exists conn tick_event.symbol >>= function
  | Error err_msg -> Lwt.return_error err_msg
  | Ok () ->
    let table_name = table_name_of_symbol tick_event.symbol in
    let query_string =
      Printf.sprintf
        "INSERT INTO %s (timestamp, source, bid, ask, mid_price, ask_qty, bid_qty, change, change_pct, high, last_price, low, volume, vwap) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
        table_name
    in
    let tick_data =
      (tick_event.ts,
       (tick_event.src,
        (Price.to_string tick_event.bid,
         (Price.to_string tick_event.ask,
          (Price.to_string tick_event.current_price,
           (Float.to_string tick_event.ask_qty,
            (Float.to_string tick_event.bid_qty,
             (Float.to_string tick_event.change,
              (Float.to_string tick_event.change_pct,
               (Float.to_string tick_event.high,
                (Float.to_string tick_event.last_price,
                 (Float.to_string tick_event.low,
                  (Float.to_string tick_event.volume,
                   Float.to_string tick_event.vwap)))))))))))))
    in
    let module C = (val conn : Caqti_lwt.CONNECTION) in
    let param_type =
      Caqti_type.(
        let ( & ) = t2 in
        int64 & string & string & string & string & string & string & string & string & string & string & string & string & string
      )
    in
    let request =
      Caqti_request.create
        param_type
        Caqti_type.unit
        Caqti_mult.zero
        (fun _ -> Caqti_query.of_string_exn query_string)
    in
    C.exec request tick_data
    |> Lwt_result.map_error (fun err ->
        let err_msg = Printf.sprintf "Failed to insert tick for %s into %s: %s" tick_event.symbol table_name (Caqti_error.show err) in
        Lwt_log_core.error ~section err_msg |> Lwt.ignore_result;
        err_msg
      )
    >>= function
    | Ok () ->
        Lwt_log_core.debug ~section
          (Printf.sprintf "Logged tick for %s to %s" tick_event.symbol table_name)
        >>= fun () -> Lwt.return_ok ()
    | Error err_msg -> Lwt.return_error err_msg

let close conn =
  Lwt_log_core.info ~section "Closing database connection" >>= fun () ->
  let module C = (val conn : Caqti_lwt.CONNECTION) in
  C.disconnect () >>= fun () -> Lwt.return_unit