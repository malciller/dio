(* src/engine/strategy.ml *)
open Lwt.Infix  (* for >>= *)


open Types.Core

open Types (* For Event.tick type *)
open Types.Primitives (* Needed for Qty, Price etc. *)

open Kraken.Ws_feed (* To get open orders *)

let start _cfg ~tick_buffer ~cmd_buffer =
  (* Grid strategy: consume ticks, emit Core.order_cmd via cmd_buffer *)
  let rec loop () =
    match Ringbuffer.pop_opt tick_buffer with
    | Some (tick : Event.tick) -> 
        (* Log the popped tick *)
        Lwt_log_core.info ~section:(Lwt_log_core.Section.make "engine.strategy") 
          (Printf.sprintf "Popped Tick: %s" (Yojson.Safe.to_string (Event.tick_to_yojson tick))) 
        >>= fun () -> 
        let symbol_str = tick.symbol in 
        (* TODO: apply grid logic using tick *) 
        (* Example: Get open orders for logic *) 
        let _open_orders = get_open_buy_orders () in 
        (* Example: Create a command *) 
        let add_cmd = Add {
            dst = "kraken"; (* Example destination *)
            client_id = "strategy-" ^ Int64.to_string (Unix.time () *. 1_000_000. |> Int64.of_float); 
            symbol = symbol_str; (* Use the string *) 
            side = Buy; (* Example side *)
            price = tick.bid; (* This should now work *) 
            qty = Qty.of_string_exn ~scale:8 "0.001"; (* Use of_string_exn with scale *)
            tif = GTC;
            tags = [`Grid];
          } in
        (* Push command to buffer *) 
        if not (Ringbuffer.push cmd_buffer add_cmd) then
             Lwt_log_core.warning ~section:(Lwt_log_core.Section.make "engine.strategy") "Command buffer full! Dropping command."
        else
             Lwt.return_unit
        >>= fun () -> 
        loop () (* Continue loop *) 
    | None -> 
        Lwt_unix.sleep 0.01 >>= loop (* Sleep briefly if buffer is empty *)
  in
  loop ()