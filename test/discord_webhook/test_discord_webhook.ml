open Alcotest_lwt

let test_order_completion_message _switch () =
  let old_cwd = Unix.getcwd () in
  (match Sys.getenv_opt "DUNE_SOURCEROOT" with
  | Some root -> Unix.chdir root
  | None -> ());
  Dotenv.export ();
  Unix.chdir old_cwd;
  match Sys.getenv_opt "DISCORD_WEBHOOK_URL" with
  | None ->
      Lwt_io.printl
        "DISCORD_WEBHOOK_URL not set, skipping real Discord webhook test."
  | Some _ ->
      let current_time = Unix.localtime (Unix.time ()) in
      let time_str =
        Printf.sprintf "%04d-%02d-%02d %02d:%02d:%02d"
          (current_time.tm_year + 1900)
          (current_time.tm_mon + 1)
          current_time.tm_mday current_time.tm_hour current_time.tm_min
          current_time.tm_sec
      in
      let test_message = Printf.sprintf "Dio Webhook Test: %s - SUCCESS" time_str in
      Discord_webhook.send_message test_message

let suite =
  [
    ( "Discord_webhook",
      [ test_case "order completion message" `Quick test_order_completion_message
      ] );
  ]

let () = Lwt_main.run (run "Discord_webhook" suite)
