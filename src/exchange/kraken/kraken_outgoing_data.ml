(* src/exchange/kraken/ws_exec.ml *)
open Lwt.Infix
open Dio_types
open Cohttp_lwt_unix 


(* Helper functions *)
let float_of_price price =
  float_of_string (Primitives.Price.to_string price)

let float_of_qty qty =
  float_of_string (Primitives.Qty.to_string qty)

(* Order command processing via REST *)
let send_order_command (cfg : Config.engine_config) (cmd : Core.order_cmd) ~on_event : unit Lwt.t = (* Removed _state *)
  let section = Lwt_log_core.Section.make "kraken_rest_exec" in (* Renamed section for clarity *)
  match cmd with
  | Add { symbol; side; price; qty; client_id; _ } -> 
      Lwt_log_core.debug ~section (Printf.sprintf "Processing REST Add Order for client_id: %s" client_id) >>= fun () ->
      let api_path = "/0/private/AddOrder" in
      let api_host = "api.kraken.com" in 
      let url = Uri.of_string (Printf.sprintf "https://%s%s" api_host api_path) in
      let nonce = Kraken_common_types.nonce () in 
      let price_str =
        let raw_price_float = float_of_price price in
        match Kraken_incoming_data.get_precisions symbol with 
        | Some (price_prec, _qty_prec) -> Primitives.format_float_precision raw_price_float price_prec
        | None -> 
            Lwt_log_core.warning ~section (Printf.sprintf "No price precision found for %s in AddOrder, sending raw price." symbol) |> Lwt.ignore_result;
            string_of_float raw_price_float
      in
      let qty_str = Primitives.Qty.to_string qty in
      let order_type_str = "limit" in 
      let side_str = match side with Core.Buy -> "buy" | Core.Sell -> "sell" in 
      let oflags = "post" in 
      let time_in_force_str = "gtc" in 
      let truncated_client_id = 
        if String.length client_id > 18 then String.sub client_id 0 18
        else client_id
      in
      Lwt_log_core.debug ~section (Printf.sprintf "Using cl_ord_id: %s (original: %s)" truncated_client_id client_id) >>= fun () ->
      let params = [
        ("nonce", [nonce]);
        ("pair", [symbol]);
        ("type", [side_str]);
        ("ordertype", [order_type_str]);
        ("price", [price_str]);
        ("volume", [qty_str]);
        ("cl_ord_id", [truncated_client_id]); 
      ] in
      let params = if oflags <> "" then params @ [("oflags", [oflags])] else params in
      let params = params @ [("timeinforce", [time_in_force_str])] in
      let sorted_params = List.sort (fun (k1,_) (k2,_) -> String.compare k1 k2) params in
      let encoded_post_data = Uri.encoded_of_query sorted_params in
      Lwt_log_core.debug ~section (Printf.sprintf "REST AddOrder POST data: %s" encoded_post_data) >>= fun () ->
      let signature = Kraken_common_types.sign ~secret:cfg.kraken_api_secret ~path:api_path ~body:encoded_post_data ~nonce in
      let headers = Cohttp.Header.of_list [
        ("API-Key", cfg.kraken_api_key);
        ("API-Sign", signature); 
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let body_cohttp = Cohttp_lwt.Body.of_string encoded_post_data in 
      Lwt.catch (fun () ->
        Client.post ~headers ~body:body_cohttp url >>= fun (resp, response_body_lwt) -> 
        let http_status = Cohttp.Response.status resp in
        Cohttp_lwt.Body.to_string response_body_lwt >>= fun body_str ->
        Lwt_log_core.debug ~section (Printf.sprintf "REST AddOrder response status: %s, body: %s" (Cohttp.Code.string_of_status http_status) body_str) >>= fun () ->
        let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
        let json = Yojson.Safe.from_string body_str in
        let errors = Yojson.Safe.Util.(member "error" json |> to_list) in
        if List.length errors > 0 then
          let error_msgs = String.concat "; " (List.map Yojson.Safe.Util.to_string errors) in
          Lwt_log_core.error ~section (Printf.sprintf "REST AddOrder failed for client_id %s: %s" truncated_client_id error_msgs) >>= fun () ->
          let ack = Core.Ack { order_id = "ERROR_" ^ truncated_client_id; client_id = truncated_client_id; state = Core.Rejected; ts } in
          on_event ack
        else
          match Yojson.Safe.Util.(member "result" json |> member "txid" |> to_list |> List.map Yojson.Safe.Util.to_string_option) with
          | (Some kraken_order_id) :: _ ->
              Lwt_log_core.info ~section (Printf.sprintf "REST AddOrder successful for client_id %s. Kraken Order ID: %s" truncated_client_id kraken_order_id) >>= fun () ->
              let ack = Core.Ack { order_id = kraken_order_id; client_id = truncated_client_id; state = Core.Open; ts } in
              on_event ack
          | _ ->
              Lwt_log_core.error ~section (Printf.sprintf "REST AddOrder: txid not found or invalid in response for client_id %s. Body: %s" truncated_client_id body_str) >>= fun () ->
              let ack = Core.Ack { order_id = "ERROR_NO_TXID_" ^ truncated_client_id; client_id = truncated_client_id; state = Core.Rejected; ts } in
              on_event ack
      ) (fun ex ->
        let err_msg = Printexc.to_string ex in
        Lwt_log_core.error ~section (Printf.sprintf "Exception during REST AddOrder for client_id %s: %s" truncated_client_id err_msg) >>= fun () ->
        let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
        let ack = Core.Ack { order_id = "EXCEPTION_" ^ truncated_client_id; client_id = truncated_client_id; state = Core.Rejected; ts } in
        on_event ack
      )

  | Amend { order_id; symbol; new_price; new_qty; _ } -> 
      Lwt_log_core.debug ~section (Printf.sprintf "Processing REST Amend Order for order_id: %s" order_id) >>= fun () ->
      let api_path = "/0/private/AmendOrder" in
      let api_host = "api.kraken.com" in
      let url = Uri.of_string (Printf.sprintf "https://%s%s" api_host api_path) in
      let nonce = Kraken_common_types.nonce () in
      let price_prec_opt, qty_prec_opt = 
        match Kraken_incoming_data.get_precisions symbol with
        | Some (pp, qp) -> (Some pp, Some qp)
        | None -> (None, None)
      in
      let price_str =
        let raw_price_float = float_of_price new_price in
        match price_prec_opt with
        | Some precision -> Primitives.format_float_precision raw_price_float precision
        | None -> 
            Lwt_log_core.warning ~section (Printf.sprintf "No price precision found for %s in amend, sending raw price." symbol) |> Lwt.ignore_result;
            string_of_float raw_price_float
      in
      let qty_str =
        let raw_qty_float = float_of_qty new_qty in
        match qty_prec_opt with
        | Some precision -> Primitives.format_float_precision raw_qty_float precision
        | None -> 
            Lwt_log_core.warning ~section (Printf.sprintf "No qty precision found for %s in amend, sending raw qty." symbol) |> Lwt.ignore_result;
            string_of_float raw_qty_float
      in
      let params = [
        ("nonce", [nonce]);
        ("txid", [order_id]); 
        ("order_qty", [qty_str]);
        ("limit_price", [price_str]);
        ("post_only", ["true"]); 
      ] in
      let encoded_post_data = Uri.encoded_of_query params in 
      Lwt_log_core.debug ~section (Printf.sprintf "REST AmendOrder POST data: %s" encoded_post_data) >>= fun () ->
      let signature = Kraken_common_types.sign ~secret:cfg.kraken_api_secret ~path:api_path ~body:encoded_post_data ~nonce in
      let headers = Cohttp.Header.of_list [
        ("API-Key", cfg.kraken_api_key);
        ("API-Sign", signature);
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let body_cohttp = Cohttp_lwt.Body.of_string encoded_post_data in
      Lwt.catch (fun () ->
        Client.post ~headers ~body:body_cohttp url >>= fun (resp, response_body_lwt) ->
        let http_status = Cohttp.Response.status resp in
        Cohttp_lwt.Body.to_string response_body_lwt >>= fun body_str ->
        Lwt_log_core.debug ~section (Printf.sprintf "REST AmendOrder response status: %s, body: %s" (Cohttp.Code.string_of_status http_status) body_str) >>= fun () ->
        let json_resp = Yojson.Safe.from_string body_str in
        let errors = Yojson.Safe.Util.(member "error" json_resp |> to_list) in
        if List.length errors > 0 then
          let error_msgs = String.concat "; " (List.map Yojson.Safe.Util.to_string errors) in
          Lwt_log_core.error ~section (Printf.sprintf "REST AmendOrder failed for order_id %s: %s" order_id error_msgs)
        else
          match Yojson.Safe.Util.(member "result" json_resp |> member "amend_id" |> to_string_option) with
          | Some amend_id ->
              Lwt_log_core.info ~section (Printf.sprintf "REST AmendOrder successful for order_id %s. Amend ID: %s" order_id amend_id)
          | None -> 
              Lwt_log_core.error ~section (Printf.sprintf "REST AmendOrder: amend_id not found in successful response for order_id %s. Body: %s" order_id body_str)
      ) (fun ex ->
        let err_msg = Printexc.to_string ex in
        Lwt_log_core.error ~section (Printf.sprintf "Exception during REST AmendOrder for order_id %s: %s" order_id err_msg)
      )
      
  | Cancel { order_id; dst = _ } -> 
      Lwt_log_core.debug ~section (Printf.sprintf "Processing REST Cancel Order for order_id: %s" order_id) >>= fun () ->
      let api_path = "/0/private/CancelOrder" in
      let api_host = "api.kraken.com" in
      let url = Uri.of_string (Printf.sprintf "https://%s%s" api_host api_path) in
      let nonce = Kraken_common_types.nonce () in
      let params = [
        ("nonce", [nonce]);
        ("txid", [order_id]); 
      ] in
      let encoded_post_data = Uri.encoded_of_query params in
      Lwt_log_core.debug ~section (Printf.sprintf "REST CancelOrder POST data: %s" encoded_post_data) >>= fun () ->
      let signature = Kraken_common_types.sign ~secret:cfg.kraken_api_secret ~path:api_path ~body:encoded_post_data ~nonce in
      let headers = Cohttp.Header.of_list [
        ("API-Key", cfg.kraken_api_key);
        ("API-Sign", signature);
        ("Content-Type", "application/x-www-form-urlencoded");
      ] in
      let body_cohttp = Cohttp_lwt.Body.of_string encoded_post_data in
      Lwt.catch (fun () ->
        Client.post ~headers ~body:body_cohttp url >>= fun (resp, response_body_lwt) ->
        let http_status = Cohttp.Response.status resp in
        Cohttp_lwt.Body.to_string response_body_lwt >>= fun body_str ->
        Lwt_log_core.debug ~section (Printf.sprintf "REST CancelOrder response status: %s, body: %s" (Cohttp.Code.string_of_status http_status) body_str) >>= fun () ->
        let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
        let json_resp = Yojson.Safe.from_string body_str in
        let errors = Yojson.Safe.Util.(member "error" json_resp |> to_list) in
        if List.length errors > 0 then
          let error_msgs = String.concat "; " (List.map Yojson.Safe.Util.to_string errors) in
          Lwt_log_core.error ~section (Printf.sprintf "REST CancelOrder failed for order_id %s: %s" order_id error_msgs) >>= fun () ->
          let ack = Core.Ack { order_id; client_id = "N/A_CANCEL_FAIL"; state = Core.Rejected; ts } in (* Or Open if unsure *)
          on_event ack
        else
          match Yojson.Safe.Util.(member "result" json_resp |> member "count" |> to_int_option) with
          | Some count when count > 0 ->
              Lwt_log_core.info ~section (Printf.sprintf "REST CancelOrder successful for order_id %s. Count: %d" order_id count) >>= fun () ->
              let ack = Core.Ack { order_id; client_id = "N/A_CANCEL"; state = Core.Canceled; ts } in
              on_event ack
          | Some _ (* count = 0 or other value *) | None -> 
              Lwt_log_core.error ~section (Printf.sprintf "REST CancelOrder: 'count' not positive or not found for order_id %s. Body: %s" order_id body_str) >>= fun () ->
              let ack = Core.Ack { order_id; client_id = "N/A_CANCEL_NO_COUNT"; state = Core.Rejected; ts } in (* Or Open *)
              on_event ack
      ) (fun ex ->
        let err_msg = Printexc.to_string ex in
        Lwt_log_core.error ~section (Printf.sprintf "Exception during REST CancelOrder for order_id %s: %s" order_id err_msg) >>= fun () ->
        let ts = Unix.gettimeofday () *. 1_000_000. |> Int64.of_float in
        let ack = Core.Ack { order_id; client_id = "N/A_CANCEL_EXN"; state = Core.Rejected; ts } in (* Or Open *)
        on_event ack
      )

let handle_router_command (cfg : Config.engine_config) cmd exec_buffer : unit Lwt.t =
  let section = Lwt_log_core.Section.make "kraken_rest_exec" in 
  Lwt_log_core.debug ~section (Printf.sprintf "Handling router command via REST: %s" (Core.order_cmd_to_yojson cmd |> Yojson.Safe.to_string)) >>= fun () ->
  
  let on_event event = 
    let _ = Ringbuffer.push exec_buffer event in
    Lwt.return_unit
  in
  
  send_order_command cfg cmd ~on_event
