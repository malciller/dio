open Dio_types




type fill_notification_payload = {
  side: Core.side;
  asset_name: string;
  qty_str: string;
  value_str: string;
  order_id: string;
  symbol: string;
}

type balance_notification_payload = {
  balances: (string * float) list;
}

type notification_payload =
  | Fill of fill_notification_payload
  | Balance of balance_notification_payload

val send_message : notification_payload -> unit Lwt.t
(** [send_message payload] pushes a fill notification payload to the Discord notification queue. *)

val get_message_queue_for_test : unit -> notification_payload Ringbuffer.t
(** [get_message_queue_for_test ()] returns the internal message queue.
    NOTE: This is intended for testing purposes only. *)

val start : (Config.engine_config -> (string, float) Hashtbl.t Lwt.t) -> Config.engine_config -> unit Lwt.t
(** [start balance_fetcher cfg] starts the Discord webhook worker and balance scheduler with the given balance fetcher and configuration. *)    