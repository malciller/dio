open Lwt.Infix
open Notty
module Stats = Stats (* Use Dio.Stats *)
module M = Stats.SMap

(* ─── ASCII sprites ───────────────────────────────────────── *)
let pacman   = I.string A.(fg yellow)  "\u{25BA}"       (* ►  *)
let ghost    = I.string A.(fg red)     "\u{2584}"       (* ▄  *)
let pill     = I.string A.(fg lightblue) "\u{25CF}"    (* ●  *)

let header_lines = [
  "  ____  _             _                  _     ____        _       _   _                 ";
  " |  _ \\(_) ___  _ __ | |__   __ _ _ __ | |_  / ___|  ___ | |_   _| |_(_) ___  _ __  ___";
  " | | | | |/ _ \\| '_ \\| '_ \\ / _` | '_ \\| __| \\___ \\ / _ \\| | | | | __| |/ _ \\| '_ \\/ __|";
  " | |_| | | (_) | |_) | | | | (_| | | | | |_   ___) | (_) | | |_| | |_| | (_) | | | \\__ \\";
  " |____/|_|\\___/| .__/|_| |_|\\__,_|_| |_|\\__| |____/ \\___/|_|\\__,_|\\__|_|\\___/|_| |_|___/";
  " |             |_|                           |                                             |";
  " |__________________________________________________________________________________|"
]

let header =
  header_lines
  |> List.map (I.string A.(fg yellow ++ st bold))
  |> I.vcat
  |> I.pad ~l:1

(* ─── helpers ─────────────────────────────────────────────── *)
let fmt_runtime start =
  let secs = int_of_float (Unix.gettimeofday () -. start) in
  Printf.sprintf "%02dh:%02dm:%02ds"
    (secs / 3600) (secs mod 3600 / 60) (secs mod 60)

let bar ?(max_len=20) n =
  let len = min max_len n in
  I.hcat (List.init len (fun _ -> ghost))

(* Build one row per asset *)
let row_of_asset asset pending trades =
  let open I in
  let left  = pacman <|> string A.empty (" " ^ asset) in
  let mid   = bar pending in
  let right =
    string A.empty
      (Printf.sprintf " pending:%3d | trades:%5Ld" pending trades)
  in
  I.pad ~r:2 left <|> I.pad ~l:2 mid <|> right

(* ─── main render ─────────────────────────────────────────── *)
let render () =
  let open I in

  (* Log the contents of the pending_orders map *)
  let map_size = M.cardinal !Stats.pending_orders in
  let map_content_str = 
    if map_size = 0 then "<empty>" 
    else 
      !Stats.pending_orders
      |> M.bindings
      |> List.map (fun (k, v) -> Printf.sprintf "%s:%d" k v)
      |> String.concat "; "
  in
  (* Use Lwt_log for consistency, even if it might not show if logging is off in dash mode *) 
  Lwt_log_core.info ~section:(Lwt_log_core.Section.make "pacdash") 
    (Printf.sprintf "Rendering dashboard. Pending Orders Map (%d entries): %s" map_size map_content_str) 
  |> Lwt.ignore_result; (* Don't block rendering *)

  let rows =
    M.fold (fun asset pending acc ->
        let trades = Option.value ~default:Int64.zero
            (M.find_opt asset !Stats.trades_executed)
        in
        row_of_asset asset pending trades :: acc)
      !Stats.pending_orders []
  in
  let hud =
    string A.(fg lightgreen ++ st bold)
      ("Runtime: " ^ fmt_runtime Stats.start_ts)
  in
  vcat (header :: rows @ [void 0 1; hud])
  |> pad ~t:1 ~l:1 ~b:1 ~r:1

(* ─── loop ────────────────────────────────────────────────── *)
let start () : Notty_lwt.Term.t = (* Explicit return type for clarity *)
  let term_instance = Notty_lwt.Term.create ~mouse:false () in
  let rec tick () =
    Notty_lwt.Term.image term_instance (render ()) >>= fun () ->
    Lwt_unix.sleep 0.5 >>= tick
  in
  Lwt.async tick;
  term_instance (* Ensure this is the returned value *)

(* Potentially the end of the file or other functions follow *) 