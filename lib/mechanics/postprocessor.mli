(* [is_safe t] is an approximation of whether [t] diverges or panics. *)
val is_safe : Raw_term.t -> bool

val handle_term : gensym:Gensym.t -> env:Renaming.t -> Raw_term.t -> Raw_term.t

val handle_rule : Raw_program.rule -> Raw_program.rule
