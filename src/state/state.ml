open Dio_types
module SMap = Map.Make(String)

let pending_orders : int SMap.t ref = ref SMap.empty
let trades_executed : Int64.t SMap.t ref = ref SMap.empty
let current_prices : Primitives.Price.t SMap.t ref = ref SMap.empty

let inc_pending asset =
  pending_orders := SMap.update asset
      (fun v -> Some ((Option.value ~default:0 v) + 1)) !pending_orders

let dec_pending asset =
  pending_orders := SMap.update asset
      (fun v_opt ->
         match v_opt with
         | Some n when n > 1 -> Some (n - 1)
         | Some 1 -> None
         | Some _
         | None -> None
      ) !pending_orders

let inc_trades asset =
  trades_executed := SMap.update asset
      (fun v -> Some (Int64.succ (Option.value ~default:Int64.zero v)))
      !trades_executed 

let update_price symbol price =
  current_prices := SMap.add symbol price !current_prices

let get_price symbol =
  SMap.find_opt symbol !current_prices

(* ... other stats functions ... *)
