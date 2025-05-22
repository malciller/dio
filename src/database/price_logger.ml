(* src/database/price_logger.ml *)
open Lwt.Infix
open Sqlite3 (* Replaces Caqti opens *)
open Dio_types.Event
open Dio_types.Primitives

let section = Lwt_log_core.Section.make "database.price_logger"

(* The database handle is now Sqlite3.db *)
type db = Sqlite3.db

(* Global mutex for serializing database write operations *)
let db_mutex = Lwt_mutex.create ()

(* Helper to get table name from symbol. Replaces '/' with '_' and ensures lowercase. *)
let table_name_of_symbol symbol =
  "prices_" ^ String.(lowercase_ascii (map (function '/' -> '_' | c -> c) symbol))

let create_table_if_not_exists db symbol =
  Lwt_mutex.with_lock db_mutex (fun () ->
    let table_name = table_name_of_symbol symbol in
    let query_string = Printf.sprintf
      "CREATE TABLE IF NOT EXISTS %s (
        timestamp INTEGER PRIMARY KEY,
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
    Lwt_preemptive.detach (fun () ->
      match Sqlite3.exec db query_string with
      | Rc.OK -> Ok ()
      | rc -> Error (Rc.to_string rc)
    ) () >>= function
    | Ok () -> Lwt.return_ok ()
    | Error err_str ->
      let err_msg = Printf.sprintf "Failed to create table %s: %s" table_name err_str in
      Lwt_log_core.error ~section err_msg >>= fun () ->
      Lwt.return_error err_msg
  )

let init db_path =
  Lwt_log_core.info ~section (Printf.sprintf "Initializing database at %s" db_path) >>= fun () ->
  Lwt_preemptive.detach (fun () ->
    try Ok (Sqlite3.db_open db_path)
    with Sqlite3.Error msg -> Error msg (* Catch potential error during db_open *)
  ) () >>= function
  | Ok db_handle -> Lwt.return_ok db_handle
  | Error msg ->
    let err_msg = Printf.sprintf "Failed to connect to database: %s" msg in
    Lwt_log_core.error ~section err_msg >>= fun () ->
    Lwt.return_error err_msg

let log_tick db (tick_event : tick) =
  (* create_table_if_not_exists is already mutex-protected *)
  create_table_if_not_exists db tick_event.symbol >>= function
  | Error err -> Lwt.return_error err
  | Ok () ->
    let table_name = table_name_of_symbol tick_event.symbol in
    Lwt_mutex.with_lock db_mutex (fun () ->
      let query_string =
        Printf.sprintf
          "INSERT INTO %s (timestamp, source, bid, ask, mid_price, ask_qty, bid_qty, change, change_pct, high, last_price, low, volume, vwap) VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?, ?)"
          table_name
      in
      Lwt_preemptive.detach
        (fun () ->
          let stmt_opt = ref None in
          try
            let db_stmt = Sqlite3.prepare db query_string in
            stmt_opt := Some db_stmt;
            ignore (Sqlite3.bind_int64 db_stmt 1 tick_event.ts);
            ignore (Sqlite3.bind_text db_stmt 2 tick_event.src);
            ignore (Sqlite3.bind_text db_stmt 3 (Price.to_string tick_event.bid));
            ignore (Sqlite3.bind_text db_stmt 4 (Price.to_string tick_event.ask));
            ignore (Sqlite3.bind_text db_stmt 5 (Price.to_string tick_event.current_price));
            ignore (Sqlite3.bind_text db_stmt 6 (Float.to_string tick_event.ask_qty));
            ignore (Sqlite3.bind_text db_stmt 7 (Float.to_string tick_event.bid_qty));
            ignore (Sqlite3.bind_text db_stmt 8 (Float.to_string tick_event.change));
            ignore (Sqlite3.bind_text db_stmt 9 (Float.to_string tick_event.change_pct));
            ignore (Sqlite3.bind_text db_stmt 10 (Float.to_string tick_event.high));
            ignore (Sqlite3.bind_text db_stmt 11 (Float.to_string tick_event.last_price));
            ignore (Sqlite3.bind_text db_stmt 12 (Float.to_string tick_event.low));
            ignore (Sqlite3.bind_text db_stmt 13 (Float.to_string tick_event.volume));
            ignore (Sqlite3.bind_text db_stmt 14 (Float.to_string tick_event.vwap));
            let result = match Sqlite3.step db_stmt with
              | Rc.DONE -> Ok ()
              | rc -> Error (Printf.sprintf "Step failed: %s, DB error: %s" (Rc.to_string rc) (Sqlite3.errmsg db))
            in
            Option.iter (fun s -> ignore (Sqlite3.finalize s)) !stmt_opt; (* Finalize in try *)
            result
          with
          | Sqlite3.Error msg ->
              Option.iter (fun s -> ignore (Sqlite3.finalize s)) !stmt_opt; (* Finalize in exn handler *)
              Error (Printf.sprintf "SQLite3 error during prepare/bind/step: %s" msg)
          | exn ->
              Option.iter (fun s -> ignore (Sqlite3.finalize s)) !stmt_opt; (* Finalize in exn handler *)
              Error (Printf.sprintf "Unexpected exception during tick logging: %s" (Printexc.to_string exn))
        )
        () (* Lwt_preemptive.detach call *)
      ) >>= fun result_from_detach -> (* result from Lwt_preemptive.detach wrapped by mutex *)
        match result_from_detach with
        | Ok () ->
            Lwt_log_core.debug ~section
              (Printf.sprintf "Logged tick for %s to %s" tick_event.symbol table_name)
            >>= fun () -> Lwt.return_ok ()
        | Error err_str ->
            let err_msg =
              Printf.sprintf "Failed to insert tick for %s into %s: %s" tick_event.symbol table_name err_str
            in
            Lwt_log_core.error ~section err_msg >>= fun () -> Lwt.return_error err_msg

let close db =
  Lwt_mutex.with_lock db_mutex (fun () ->
    Lwt_log_core.info ~section "Closing database connection" >>= fun () ->
    Lwt_preemptive.detach (fun () ->
      ignore (Sqlite3.db_close db)
    ) ()
  )
