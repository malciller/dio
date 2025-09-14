open Dio_types


module Notification : sig
  type fill_notification_payload = {
    side: Core.side;
    asset_name: string;
    qty_str: string;
    value_str: string;
    order_id: string;
    symbol: string;
  }
end

type fill_notification_payload = Notification.fill_notification_payload

val send_message : fill_notification_payload -> unit Lwt.t
(** [send_message payload] pushes a fill notification payload to the Discord notification queue. *)

val get_message_queue_for_test : unit -> fill_notification_payload Ringbuffer.t
(** [get_message_queue_for_test ()] returns the internal message queue.
    NOTE: This is intended for testing purposes only. *)

val start : unit -> unit Lwt.t
(** [start ()] starts the Discord webhook worker. *)    