(* src/engine/strategy.ml *)
open Lwt.Infix  (* for >>= *)

let start _cfg ~tick_stream ~send_cmd =
  (* Grid strategy: consume ticks, emit Core.order_cmd via send_cmd *)
  let rec loop () =
    match tick_stream () with
    | Some tick ->
        (* TODO: apply grid logic *)
        send_cmd _cfg tick >>= fun () -> loop ()
    | None -> Lwt_unix.sleep 0.01 >>= loop
  in
  loop ()