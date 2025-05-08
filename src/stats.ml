module SMap = Map.Make(String)

let pending_orders = ref SMap.empty
let trades_executed = ref SMap.empty
let start_ts = Unix.gettimeofday ()

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