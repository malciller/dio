(* src/types/core.ml *)
open Primitives

type side  = Buy | Sell                 [@@deriving yojson]
type tif   = GTC | IOC | FOK            [@@deriving yojson]

type order_tag = [`Grid | `Manual | `Rebalance] [@@deriving yojson]

type order_cmd =
  | Add of {
      client_id : string ;
      symbol    : symbol ;
      side      : side ;
      price     : Price.t ;
      qty       : Qty.t ;
      tif       : tif ;
      tags      : order_tag list ;
    }
  | Amend of { order_id : string ; new_price : Price.t option ; new_qty : Qty.t option }
  | Cancel of { order_id : string }
[@@deriving yojson]

type order_state = Open | Filled | Canceled | Rejected [@@deriving yojson]

type market_event =
  | Book  of { symbol : symbol ; bid : Price.t ; ask : Price.t ; ts : timestamp }
  | Trade of { symbol : symbol ; price : Price.t ; qty : Qty.t ; side : side ; ts : timestamp }
  | Fill  of { symbol : symbol ; order_id : string ; client_id : string ;
               price : Price.t ; qty : Qty.t ; side : side ; ts : timestamp }
  | Ack   of { order_id : string ; client_id : string ; state : order_state ; ts : timestamp }
  | Heartbeat of timestamp
[@@deriving yojson]

