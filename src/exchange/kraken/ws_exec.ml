(* src/exchange/kraken/ws_exec.ml *)
open Lwt.Infix
open Websocket
open Types

(* Global connection state *)
let connection_state = ref None

(* Helper functions *)
let get_next_req_id state =
  let id = state.Common.next_req_id in
  state.Common.next_req_id <- id + 1;
  id

let float_of_price price =
  float_of_string (Primitives.Price.to_string price)

let float_of_qty qty =
  float_of_string (Primitives.Qty.to_string qty)

(* WebSocket connection setup *)
let connect (cfg : Config.engine_config) =
  let ctx = Lazy.force Conduit_lwt_unix.default_ctx in
  let host = "ws-auth.kraken.com" in
  let port = cfg.ws_port in
  let uri = Uri.of_string (Printf.sprintf "wss://%s:%d/v2" host port) in
  let tls_config = `Hostname host, `IP (Ipaddr.of_string_exn "104.16.248.94"), `Port port in
  let endpoint = `TLS tls_config in
  Websocket_lwt_unix.connect ~ctx endpoint uri

(* Message sending *)
let send_message state msg =
  let content = Yojson.Safe.to_string msg in
  Websocket_lwt_unix.write state.Common.conn (Frame.create ~content ())

(* Order placement - MODIFIED *)
let send_order_command state token (cmd : Core.order_cmd) : unit Lwt.t =
  let section = Lwt_log_core.Section.make "kraken_ws_exec" in
  match cmd with
  | Add { symbol; side; price; qty; client_id; _ } ->
      let req_id = get_next_req_id state in
      Hashtbl.add state.Common.req_to_client req_id client_id;

      (* --- PRICE FORMATTING using Instrument Precision --- *)
      let price_string =
        let raw_price_float = float_of_price price in
        match Ws_feed.get_price_precision symbol with (* Use getter from feed *)
        | Some precision ->
            Lwt_log_core.debug ~section
              (Printf.sprintf "Formatting price for %s with precision %d" symbol precision) |> Lwt.ignore_result;
            (* Use the shared formatting function from Primitives *) 
            Primitives.format_float_precision raw_price_float precision
        | None ->
            Lwt_log_core.warning ~section
              (Printf.sprintf "No precision found for symbol %s, sending raw price as string." symbol) |> Lwt.ignore_result;
            string_of_float raw_price_float (* Fallback to basic string conversion *)
      in
      (* --- END PRICE FORMATTING --- *)

      (* Define a unique placeholder for string replacement *) 
      let price_placeholder = "@@PRICE_PLACEHOLDER@@" in

      (* Create request with placeholder string *) 
      let request_with_placeholder = `Assoc [
        "method", `String "add_order";
        "params", `Assoc [
          "symbol", `String symbol;
          "side", `String (match side with Buy -> "buy" | Sell -> "sell");
          "order_type", `String "limit";
          "limit_price", `String price_placeholder; (* Use placeholder string *) 
          "order_qty", `Float (float_of_qty qty);
          "time_in_force", `String "gtc";
          "post_only", `Bool true;
          "token", `String token;
        ];
        "req_id", `Int req_id;
      ] in

      (* Convert to JSON string *) 
      let json_string_with_placeholder = Yojson.Safe.to_string request_with_placeholder in
      
      (* Replace the quoted placeholder with the actual numeric price string *) 
      let final_json_string = Str.replace_first 
        (Str.regexp ("\"" ^ price_placeholder ^ "\"")) 
        price_string 
        json_string_with_placeholder 
      in

      (* Log the request *) 
      Lwt_log_core.debug ~section
        (Printf.sprintf "Sending order request: %s" final_json_string) >>= fun () ->

      (* Create a promise for the response *)
      let (_response_promise, response_resolver) = Lwt.wait () in
      Hashtbl.add state.response_promises req_id response_resolver;

      (* Send the request (as a raw string frame) *) 
      Websocket_lwt_unix.write state.conn (Frame.create ~content:final_json_string ())

  | Amend { order_id; symbol; new_price; new_qty; _ } -> (* Add symbol *) 
      let req_id = get_next_req_id state in

      (* Use the provided Kraken order_id directly *) 
      let kraken_order_id = order_id in (* Assign for clarity/consistency *) 

      (* --- PRICE FORMATTING using Instrument Precision --- *) 
      (* Use symbol from the command *) 
      let price_string =
        let raw_price_float = float_of_price new_price in
        match Ws_feed.get_price_precision symbol with (* Use symbol from cmd *) 
        | Some precision ->
            Lwt_log_core.debug ~section
              (Printf.sprintf "Formatting amend price for %s with precision %d" symbol precision) |> Lwt.ignore_result;
            (* Use the shared formatting function from Primitives *) 
            Primitives.format_float_precision raw_price_float precision
        | None ->
            Lwt_log_core.warning ~section
              (Printf.sprintf "No precision found for symbol %s in amend, sending raw price as string." symbol) |> Lwt.ignore_result;
            string_of_float raw_price_float (* Fallback to basic string conversion *)
      in
      (* --- END PRICE FORMATTING --- *) 

      (* Define a unique placeholder *) 
      let price_placeholder = "@@PRICE_PLACEHOLDER@@" in

      (* Create request with placeholder string *) 
      let params_placeholder = `Assoc [
        "order_id", `String kraken_order_id;
        "order_qty", `Float (float_of_qty new_qty);
        "limit_price", `String price_placeholder; (* Use placeholder string *) 
        "post_only", `Bool true;
        "token", `String token; (* Move token inside params *) 
      ] in
      let request_with_placeholder = `Assoc [
        "method", `String "amend_order";
        "params", params_placeholder;
        "req_id", `Int req_id;
      ] in

      (* Convert to JSON string *) 
      let json_string_with_placeholder = Yojson.Safe.to_string request_with_placeholder in
      
      (* Replace the quoted placeholder with the actual numeric price string *) 
      let final_json_string = Str.replace_first 
        (Str.regexp ("\"" ^ price_placeholder ^ "\"")) 
        price_string 
        json_string_with_placeholder 
      in

      Lwt_log_core.debug ~section
        (Printf.sprintf "Sending amend request: %s" final_json_string) >>= fun () ->

      (* Create a promise for the response *) 
      let (_response_promise, response_resolver) = Lwt.wait () in
      Hashtbl.add state.response_promises req_id response_resolver;

      (* Send the request (as a raw string frame) *) 
      Websocket_lwt_unix.write state.conn (Frame.create ~content:final_json_string ()) >>= fun () ->
      
      (* No need to return bool anymore *) 
      Lwt.return_unit (* Always return unit Lwt.t from the function arm *) 

  | Cancel { dst; order_id } -> (* Explicitly match fields *)
      (* TODO: Implement Cancel for exchange dst using order_id *)
      Lwt_log_core.warning ~section (Printf.sprintf "Cancel order %s not implemented yet for exchange %s" order_id dst) >>= fun () ->
      Lwt.return_unit


(* Message handling - MODIFIED PARSING *)
let handle_message state msg ~on_event =
  let section = Lwt_log_core.Section.make "kraken_ws_exec" in
  match msg.Frame.opcode with
  | Frame.Opcode.Text ->
      Lwt_log_core.debug ~section 
        (Printf.sprintf "Raw response: %s" msg.content) >>= fun () ->
      let json = Yojson.Safe.from_string msg.content in
      
      (* Check for status/heartbeat first *) 
      let message_type = Yojson.Safe.Util.(member "type" json |> to_string_option) in
      let channel = Yojson.Safe.Util.(member "channel" json |> to_string_option) in
      
      (match message_type, channel with
       | Some "update", Some "status" -> 
           Lwt_log_core.debug ~section "Received status update" >>= fun () -> Lwt.return_unit
       | Some "heartbeat", _ -> 
           Lwt_log_core.debug ~section "Received heartbeat" >>= fun () -> Lwt.return_unit
       | _ ->
           (* Attempt manual parsing for order response fields *) 
           Lwt_log_core.debug ~section "Attempting manual parse for order response" >>= fun () ->
           let req_id_opt = Yojson.Safe.Util.(member "req_id" json |> to_int_option) in
           let success_opt = Yojson.Safe.Util.(member "success" json |> to_bool_option) in
           let error_opt = Yojson.Safe.Util.(member "error" json |> to_string_option) in
           (* Note: 'result' field is often absent in errors, so we don't extract it here yet *) 

           match req_id_opt with
           | Some req_id ->
               Lwt_log_core.debug ~section (Printf.sprintf "Manually parsed req_id: %d" req_id) >>= fun () ->
               if Hashtbl.mem state.Common.response_promises req_id then (
                 Lwt_log_core.debug ~section (Printf.sprintf "Found matching promise for req_id: %d" req_id) >>= fun () ->
                 (* Construct the response record manually *) 
                 let response : Core.order_response = {
                   success = Option.value success_opt ~default:false; (* Assume false if 'success' field missing *) 
                   error = error_opt;
                   result = Yojson.Safe.Util.(member "result" json |> to_option (fun x -> x)); (* Get result if present *) 
                 } in

                 let resolver = Hashtbl.find state.Common.response_promises req_id in
                 Hashtbl.remove state.Common.response_promises req_id;
                 Lwt.wakeup resolver response;
                 Lwt_log_core.debug ~section "Woke up promise" >>= fun () ->

                 (* Process the response for events (AFTER WAKEUP) *) 
                 if response.success then
                   match response.result with
                   | Some result_json ->
                       let order_id = match Yojson.Safe.Util.(member "order_id" result_json |> to_string_option) with
                         | Some id -> id
                         | None -> "unknown_order_id"
                       in
                       let client_id = Hashtbl.find_opt state.Common.req_to_client req_id in
                       Hashtbl.remove state.Common.req_to_client req_id;
                       (match client_id with
                        | Some cid ->
                            (* REMOVED: client_id_to_order_id mapping storage *) 
                            Lwt_log_core.debug ~section (Printf.sprintf "Received Add Ack for order %s (client_id %s)" order_id cid) >>= fun () -> 

                            let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
                            let ack = Core.Ack { order_id; client_id = cid; state = Core.Open; ts } in
                            on_event ack
                        | None -> Lwt_log_core.warning ~section (Printf.sprintf "No client_id found for req_id %d when processing Add Order Ack" req_id))
                   | None -> Lwt_log_core.warning ~section "Successful response but no result data"
                 else
                    (* Error already logged by the waiting promise/loop *) 
                    Lwt.return_unit
                ) else (
                  Lwt_log_core.debug ~section (Printf.sprintf "No promise found for req_id: %d. Ignoring." req_id) >>= fun () ->
                  Lwt.return_unit
                )
           | None ->
               Lwt_log_core.debug ~section "No req_id found in response JSON. Cannot process." >>= fun () ->
               Lwt.return_unit
           (* End manual parsing *) 
      )

  | Frame.Opcode.Ping ->
      Websocket_lwt_unix.write state.conn (Frame.create ~opcode:Frame.Opcode.Pong ())
  | Frame.Opcode.Close ->
      Lwt_log_core.info ~section "Received Close frame"
  | _ ->
      Lwt_log_core.warning ~section
        (Printf.sprintf "Unhandled frame opcode: %s" (Frame.Opcode.to_string msg.Frame.opcode))

(* Main loop - REVISED *) 
let rec start_loop state token ~on_event =
  let section = Lwt_log_core.Section.make "kraken_ws_exec" in
  (* Check internal queue first (non-blocking) *) 
  match Queue.take_opt state.Common.cmd_queue with
  | Some cmd ->
      (* Process command from internal queue *) 
      Lwt_log_core.debug ~section "Processing command from internal queue" >>= fun () ->
      send_order_command state token cmd >>= fun () -> (* Use renamed function *)
      (* No response logging here anymore *) 
      start_loop state token ~on_event (* Loop immediately to check queue / send next order *) 
  | None ->
      (* Queue empty, wait for WebSocket message, new command signal, OR timeout *) 
      Lwt_log_core.debug ~section "Queue empty, waiting for message, signal, or timeout..." >>= fun () ->
      let timeout_duration = 30.0 (* seconds *) 
      in
      let read_promise = Lwt.catch
        (fun () ->
           Websocket_lwt_unix.read state.conn >>= fun msg ->
           Lwt_log_core.debug ~section "WebSocket read resolved" >>= fun () ->
           handle_message state msg ~on_event >>= fun () ->
           Lwt.return (Some `MsgHandled) (* Indicate success *)
        )
        (fun ex ->
           (* Handle read/connection errors *) 
           Lwt_log_core.error ~section
             (Printf.sprintf "Error reading from WebSocket: %s. Closing loop." (Printexc.to_string ex)) >>= fun () ->
           (* Attempt to explicitly close the transport *) 
           Lwt.catch (fun () -> Websocket_lwt_unix.close_transport state.conn) (fun _ -> Lwt.return_unit) >>= fun () ->
           connection_state := None; (* Reset global state to allow reconnect *)
           Lwt.return None (* Indicate failure *) 
        ) 
      in

      let wait_promise = 
        Lwt_condition.wait state.cmd_cond >>= fun () ->
        Lwt_log_core.debug ~section "Command condition signaled" >>= fun () ->
        Lwt.return (Some `CondSignaled) (* Indicate success *)
      in
      
      let timeout_promise =
        Lwt_unix.timeout timeout_duration >>= fun () ->
        Lwt.return (Some `Timeout)
      in

      Lwt.pick [ read_promise; wait_promise; timeout_promise ] >>= function
        | Some `MsgHandled
        | Some `CondSignaled -> 
            (* Success from either read or wait, continue loop *) 
            start_loop state token ~on_event
        | Some `Timeout ->
            (* Idle timeout: Disconnect instead of pinging *) 
            Lwt_log_core.info ~section "Idle timeout, disconnecting WS Exec connection." >>= fun () ->
            Lwt.catch 
              (fun () -> Websocket_lwt_unix.close_transport state.conn) 
              (fun ex -> 
                 Lwt_log_core.warning ~section (Printf.sprintf "Error closing transport on timeout: %s" (Printexc.to_string ex)) 
              ) >>= fun () ->
            connection_state := None; (* Clear global state *) 
            Lwt.return_unit (* Stop the loop *) 
        | None -> 
            (* read_promise failed, transport already closed and state cleared in its handler, just stop loop *) 
            Lwt.return_unit

(* Public interface for router - REVISED *)
let handle_router_command (cfg : Config.engine_config) cmd exec_buffer : unit Lwt.t =
  let section = Lwt_log_core.Section.make "kraken_ws_exec" in
  match !connection_state with
  | Some state ->
      (* Connection exists, enqueue command and signal *) 
      Lwt_log_core.debug ~section "Connection exists, enqueuing command..." >>= fun () ->
      Queue.push cmd state.Common.cmd_queue;
      Lwt_condition.signal state.Common.cmd_cond ();
      Lwt.return_unit (* Return unit Lwt.t *)

  | None ->
      (* Start WebSocket connection if not already running *) 
      Lwt_log_core.info ~section "No active connection, creating state and starting WS Exec loop..." >>= fun () ->
      match cfg.auth_token with
      | None -> Lwt.fail_with "Authentication token required for order execution"
      | Some token ->
          (* Connect first *) 
          connect cfg >>= fun conn ->
          (* Create initial state *) 
          let initial_state = { 
            Common.conn; 
            Common.next_req_id = 1;
            Common.req_to_client = Hashtbl.create 16;
            Common.response_promises = Hashtbl.create 16;
            Common.cmd_queue = Queue.create ();
            Common.cmd_cond = Lwt_condition.create ();
          } in
          (* Set global state SYNCHRONOUSLY *) 
          connection_state := Some initial_state;
          Lwt_log_core.debug ~section "Global connection_state set" >>= fun () ->
          
          (* Enqueue the FIRST command *) 
          Queue.push cmd initial_state.cmd_queue;
          Lwt_log_core.debug ~section "Initial command enqueued" >>= fun () ->
          
          (* Define on_event here *) 
          let on_event event = 
            let _ = Ringbuffer.push exec_buffer event in
            Lwt.return_unit
          in
          
          (* Launch the loop asynchronously *) 
          Lwt.async (fun () -> 
            Lwt.catch 
              (fun () -> start_loop initial_state token ~on_event) 
              (fun ex -> 
                 Lwt_log_core.error ~section 
                   (Printf.sprintf "WS Exec loop crashed: %s" (Printexc.to_string ex))
              )
          );
          Lwt_log_core.info ~section "Kraken WS Exec loop launched asynchronously" >>= fun () ->
          Lwt.return_unit