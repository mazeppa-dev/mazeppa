type t = Symbol.t Symbol_map.t [@@deriving eq, show]

val insert : gensym:Gensym.t -> t * Symbol.t -> t * Symbol.t

val insert_list : gensym:Gensym.t -> t * Symbol.t list -> t * Symbol.t list
