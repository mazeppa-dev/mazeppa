module Make (_ : sig end) : sig
  type t

  type node = Symbol.t * Term.t

  val empty : t

  val memoize : suspect:node -> t -> node option * t
end
