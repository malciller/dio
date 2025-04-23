open Kraken.Token (* Try path based on library name 'kraken' *)

let main () = 
  (* Load .env file - assumes .env is in the current working directory *) 
  (* Note: If running via dune exec, CWD might be different. *) 
  (* Consider specifying the path if needed: Dotenv.export ~path:"path/to/.env" () *) 
  (try Dotenv.export ~path:"src/exchange/kraken/.env" () with _ -> Printf.eprintf "Warning: Failed to load .env file.\n%!");

  (* Call the get_token function from the Kraken library *) 
  Lwt_main.run (
    Lwt.catch 
      (fun () -> 
        let%lwt _ = get_token () in (* Ignore the returned token *) 
        (* The get_token function already prints confirmation, *) 
        (* you could add more printing here if needed, e.g.: *) 
        (* Printf.printf "Full token: %s\n%!" token; *) 
        Lwt.return_unit
      )
      (fun exn -> 
        Printf.eprintf "Error retrieving token: %s\n%!" (Printexc.to_string exn);
        Lwt.return_unit
      )
  )

let () = main ()
