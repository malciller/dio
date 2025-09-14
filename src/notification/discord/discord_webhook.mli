val send_message : string -> unit Lwt.t
(** [send_message message] pushes a message to the Discord notification queue. *)

val get_message_queue_for_test : unit -> string Dio_types.Ringbuffer.t
(** [get_message_queue_for_test ()] returns the internal message queue.
    NOTE: This is intended for testing purposes only. *)

val start : unit -> unit Lwt.t
(** [start ()] starts the Discord webhook worker. *)