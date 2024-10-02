(** A mutable symbol generator (traditionally called "gensym"). *)
type t

(** Creates a gensym with a given prefix and counter set to 0. *)
val create : prefix:string -> unit -> t

(** Generates a new symbol and then increments the counter.

    The symbol will have the form [prefix ^ i], where [prefix] is the prefix passed to
    {!create} and [i] is the current counter value. *)
val emit : t -> Symbol.t

(** Returns the most recently generated symbol; panics if the counter is 0. *)
val latest : t -> Symbol.t

(** Sequentially generates a symbol list with as many elements as in [length_list]. *)
val emit_list : length_list:'a list -> t -> Symbol.t list

(** Clones the state of this gensym. *)
val clone : t -> t

(** Assigns the state of [other] to this gensym. *)
val assign : other:t -> t -> unit

(** Resets the inner counter to 0. *)
val reset : t -> unit
