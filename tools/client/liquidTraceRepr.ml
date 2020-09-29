open LiquidClientSigs
open LiquidClientTypes
open LiquidClientUtils

module type S = sig
  val length : int
  val max_gas : int
  val end_gas : int    

  val item_gas : int -> int
  val item_loc : int -> LiquidTypes.location option
  val item_str_stack_map : (string * string option -> 'a) -> int -> 'a list   
end

type t = (module S)

module MakeTrace(L : LANG) = struct
  let trace_to_trace_repr t : (module S) =
    let array = Array.of_list t in (
      module struct
        let length = Array.length array
        let item_gas i = array.(i).Trace.gas

        let max_gas = item_gas 0
        let end_gas = item_gas length

        let item_loc i = Option.map L.Source.normalize_loc array.(i).loc

        let item_str_stack_map f i =
          List.map
            (fun (c, str) -> f (c#string, str))
            (array.(i).stack)
      end
    )
end

let length (t : t) = 
  let module T = (val t) in T.length

let max_gas (t : t) = 
  let module T = (val t) in T.max_gas

let end_gas (t : t) = 
  let module T = (val t) in T.end_gas

let item_gas (pos : int) (t : t) = 
  let module T = (val t) in T.item_gas pos

let item_loc (pos : int) (t : t) = 
  let module T = (val t) in T.item_loc pos

let item_str_stack_map f (pos : int) (t : t) = 
  let module T = (val t) in T.item_str_stack_map f pos
