include module type of Set.Make (Symbol)

val decouple : ('a * t) list -> 'a list * t

val decouple_map : f:('a -> 'b * t) -> 'a list -> 'b list * t
