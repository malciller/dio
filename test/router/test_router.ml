open Lwt.Syntax (* enable let* syntax *)
open Alcotest_lwt
open Dio_types

(* Define default scales *)
let default_price_scale = 8
let default_qty_scale = 8

(* Helper to create a dummy Add command *)
let create_add_cmd 
    ?(dst = "kraken") 
    ?(symbol = "BTC/USD") 
    ?(side = Core.Buy) (* Fully qualify *)
    ?(price_str = "50000.0") 
    ?(qty_str = "0.1") 
    ?(tif = Core.GTC) (* Add default TIF, fully qualified *)
    ?(client_id = "test_client_id_1")
    ?(tags = [`Manual]) 
    () : Core.order_cmd = (* Fully qualify, corrected type *)
  let price = Primitives.Price.of_string_exn ~scale:default_price_scale price_str in 
  let qty = Primitives.Qty.of_string_exn ~scale:default_qty_scale qty_str in 
  Core.Add { dst; symbol; side; price; qty; tif; client_id; tags } 

(* --- Test Cases --- *)

(* Test that an identical Add order is marked as duplicate immediately *)
let test_duplicate_add_order _ () =
  let cmd = create_add_cmd () in
  (* Fully qualify access to OrderCache members *)
  let key = Router.OrderCache.make_order_key cmd in 
  
  (* Clear cache for this key *)
  Hashtbl.remove Router.OrderCache.recent_orders key;

  let* () = Lwt.return_unit in (* Start Lwt context *)
  let is_dup1 = Router.OrderCache.is_duplicate cmd in
  Alcotest.(check bool "First order should not be duplicate" false is_dup1);

  let is_dup2 = Router.OrderCache.is_duplicate cmd in
  Alcotest.(check bool "Second identical order should be duplicate" true is_dup2);
  Lwt.return_unit

(* Test that an identical Add order is NOT marked duplicate after timeout *)
let test_expired_add_order _ () =
  let cmd = create_add_cmd ~client_id:"test_client_id_expired" () in
  let key = Router.OrderCache.make_order_key cmd in

  (* Clear cache for this key *)
  Hashtbl.remove Router.OrderCache.recent_orders key;

  let* () = Lwt.return_unit in
  let is_dup1 = Router.OrderCache.is_duplicate cmd in
  Alcotest.(check bool "First order should not be duplicate" false is_dup1);

  (* Access cache_timeout via full path *)
  let cache_timeout_value = Router.OrderCache.cache_timeout in
  let* () = Lwt_unix.sleep (cache_timeout_value +. 0.1) in

  let is_dup2 = Router.OrderCache.is_duplicate cmd in
  Alcotest.(check bool "Order after timeout should not be duplicate" false is_dup2);
  
  (* Let's verify the timestamp was updated *)
  let is_dup3 = Router.OrderCache.is_duplicate cmd in
  Alcotest.(check bool "Immediate check after expired-add should be duplicate" true is_dup3);
  
  Lwt.return_unit

(* Test that different Add orders are not marked as duplicates *)
let test_different_add_orders _ () =
   let cmd1 = create_add_cmd ~side:Core.Buy ~client_id:"diff_buy_1" () in (* Fully qualify *)
   let cmd2 = create_add_cmd ~side:Core.Sell ~client_id:"diff_sell_1" () in (* Fully qualify *)
   let key1 = Router.OrderCache.make_order_key cmd1 in
   let key2 = Router.OrderCache.make_order_key cmd2 in

   (* Clear cache *)
   Hashtbl.remove Router.OrderCache.recent_orders key1;
   Hashtbl.remove Router.OrderCache.recent_orders key2;
   
   let* () = Lwt.return_unit in
   let is_dup1 = Router.OrderCache.is_duplicate cmd1 in
   Alcotest.(check bool "First order (buy) should not be duplicate" false is_dup1);

   let is_dup2 = Router.OrderCache.is_duplicate cmd2 in
   Alcotest.(check bool "Second different order (sell) should not be duplicate" false is_dup2);
   Lwt.return_unit

(* --- Test Suite --- *)

let suite =
  [
    test_case "Duplicate Add Order" `Quick test_duplicate_add_order;
    test_case "Expired Add Order" `Slow test_expired_add_order;
    test_case "Different Add Orders" `Quick test_different_add_orders;
  ]

(* Run the tests *)
let () = Lwt_main.run (Alcotest_lwt.run "Router.OrderCache" [ ("OrderCache", suite) ])
