(* src/database/price_logger.mli *)

(** Module for logging price ticks to a SQLite database. *)

open Dio_types.Event

(** Opaque type for the database connection. *)
type db

(** [init db_path] initializes the database connection.
    Creates the database file and necessary tables if they don't exist.
    @param db_path The path to the SQLite database file.
    @return A Lwt promise that resolves to the database connection or an error. *)
val init : string -> (db, string) result Lwt.t

(** [log_tick db tick_event] logs a price tick to the appropriate table.
    The table name is derived from the tick's symbol.
    @param db The database connection.
    @param tick_event The tick event to log.
    @return A Lwt promise that resolves to unit or an error. *)
val log_tick : db -> tick -> (unit, string) result Lwt.t

(** [close db] closes the database connection.
    @param db The database connection.
    @return A Lwt promise that resolves to unit. *)
val close : db -> unit Lwt.t 