type t

val create : prefix:string -> unit -> t

(* Generates symbols of the form [prefix ^ i], where [prefix] is the prefix passed to
   [create] and [i] is the counter. The counter starts at 0 and increases by 1 each time
   [emit] is called on this gensym. *)
val emit : t -> Symbol.t

val current : t -> Symbol.t

val emit_list : length_list:'a list -> t -> Symbol.t list

val clone : t -> t

val assign : other:t -> t -> unit

val reset : t -> unit
