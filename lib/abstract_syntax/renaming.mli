type t = Symbol.t Symbol_map.t [@@deriving eq, show]

val insert : gensym:Gensym.t -> ?renaming:t -> t * Symbol.t -> t * Symbol.t

val insert_list : gensym:Gensym.t -> ?renaming:t -> t * Symbol.t list -> t * Symbol.t list
