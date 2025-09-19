(* src/exchange/kraken/kraken_balances.ml *)

open Lwt.Infix
open Lwt_log_core
open Dio_types

let section = Section.make "kraken.balances"

(* In-memory store for balances *)
let balances : (string, float) Hashtbl.t = Hashtbl.create(16)

(* Condition to signal when the initial balance snapshot is received *)
let balances_initialized = Lwt_condition.create ()

(* Track whether balances have been initialized *)
let balances_ready = ref false

let handle_balance_snapshot data =
  let open Yojson.Safe.Util in
  Hashtbl.clear balances;
  data |> to_list |> Lwt_list.iter_s (fun item ->
    let asset = item |> member "asset" |> to_string in
    let balance_val = item |> member "balance" |> to_float in
    Hashtbl.add balances asset balance_val;
    debug_f ~section "Balance snapshot: %s -> %f" asset balance_val
  ) >>= fun () ->
  balances_ready := true;
  Lwt_condition.broadcast balances_initialized ();
  info_f ~section "Processed balance snapshot. %d assets loaded." (Hashtbl.length balances)

let handle_balance_update data =
  let open Yojson.Safe.Util in
  data |> to_list |> Lwt_list.iter_s (fun item ->
    let asset = item |> member "asset" |> to_string in
    let new_balance = item |> member "balance" |> to_float in
    Hashtbl.replace balances asset new_balance;
    debug_f ~section "Balance update: %s -> %f" asset new_balance
  )

let handle_balances_message msg =
  let json = Yojson.Safe.from_string msg in
  let open Yojson.Safe.Util in
  match member "channel" json with
  | `String "balances" ->
      (match member "type" json with
      | `String "snapshot" -> handle_balance_snapshot (member "data" json)
      | `String "update" -> handle_balance_update (member "data" json)
      | _ -> warning_f ~section "Unhandled message type on balances channel: %s" msg
      )
  | _ ->
      debug_f ~section "Received non-balances message: %s" msg

let subscribe_to_balances conn token =
  let sub_msg =
    `Assoc [
      ("method", `String "subscribe");
      ("params", `Assoc [
        ("channel", `String "balances");
        ("token", `String token)
      ])
    ] |> Yojson.Safe.to_string
  in
  let frame = Websocket.Frame.create ~content:sub_msg () in
  Websocket_lwt_unix.write conn frame

let rec listen_for_balances conn =
  Websocket_lwt_unix.read conn >>= fun frame ->
  let content = frame.content in
  handle_balances_message content >>= fun () ->
  listen_for_balances conn

let initialize_ws_balances_feed (cfg : Config.engine_config) (token : string) =
  Lwt.async (fun () ->
    let rec connect_and_listen () =
      Lwt.catch
        (fun () ->
          info_f ~section "Connecting to Kraken balances WebSocket..." >>= fun () ->
          let port = cfg.ws_port in
          let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
          let connect_host = "ws-auth.kraken.com" in
          let path = "/v2" in
          let uri = Uri.of_string (Printf.sprintf "wss://%s:%d%s" connect_host port path) in

          Lwt_unix.getaddrinfo connect_host (string_of_int port) [Unix.(AI_FAMILY PF_INET)] >>= fun addrs ->
          match addrs with
          | { Unix.ai_addr = Unix.ADDR_INET (ip_addr_from_dns, _); _ } :: _ ->
              let ip_to_use = Ipaddr.of_string_exn (Unix.string_of_inet_addr ip_addr_from_dns) in
              let tls_config = `Hostname connect_host, `IP ip_to_use, `Port port in
              let endpoint = `TLS tls_config in
              Websocket_lwt_unix.connect ~ctx endpoint uri >>= fun conn ->
              info_f ~section "Connected to Kraken balances WebSocket." >>= fun () ->
              subscribe_to_balances conn token >>= fun () ->
              listen_for_balances conn
          | _ -> Lwt.fail_with (Printf.sprintf "Failed to resolve host: %s" connect_host)
        )
        (fun exn ->
          error_f ~section "Balances WebSocket error: %s. Reconnecting in 10s..." (Printexc.to_string exn) >>= fun () ->
          Lwt_unix.sleep 10.0 >>= fun () ->
          connect_and_listen ()
        )
    in
    connect_and_listen ()
  )

let is_balances_initialized () : bool =
  !balances_ready

let get_account_balance () : (string, float) Hashtbl.t Lwt.t =
  if is_balances_initialized () then (
    (* WebSocket is already initialized, return balances immediately *)
    Lwt.return (Hashtbl.copy balances)
  ) else (
    let timeout = 30.0 in (* 30 second timeout *)
    Lwt.pick [
      (Lwt_condition.wait balances_initialized >>= fun () ->
       info_f ~section "Balance WebSocket initialized, received initial snapshot" >>= fun () ->
       Lwt.return (Hashtbl.copy balances));
      (Lwt_unix.sleep timeout >>= fun () ->
       warning_f ~section "Balance WebSocket initialization timeout after %.1f seconds, returning empty balances" timeout >>= fun () ->
       Lwt.return (Hashtbl.create 16))
    ]
  )
