include module type of Map.Make (Symbol)

val extend : bindings:(Symbol.t * 'a) list -> 'a t -> 'a t

val extend_map : f:('a -> 'b) -> bindings:(Symbol.t * 'a) list -> 'b t -> 'b t

val extend2 : keys:Symbol.t list -> values:'a list -> 'a t -> 'a t

val setup : (Symbol.t * 'a) list -> 'a t

val setup2 : Symbol.t list * 'a list -> 'a t
