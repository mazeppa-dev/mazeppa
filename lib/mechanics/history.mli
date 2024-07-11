type t

type node = Symbol.t * Term.t

val empty : t

val memoize : suspect:node -> t -> node option * t
