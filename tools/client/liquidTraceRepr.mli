type t

val length : t -> int

val max_gas : t -> int

val end_gas : t -> int

val item_gas : int -> t -> int

val item_loc : int -> t -> LiquidTypes.location option

val item_str_stack_map : (string * string option -> 'a) -> int -> t -> 'a list

module MakeTrace(L : LiquidClientSigs.LANG) :
sig
  val trace_to_trace_repr :
    (L.Source.location, L.Source.const Lazy_superposed.t)
      LiquidClientTypes.Trace.trace_item list -> t
end
