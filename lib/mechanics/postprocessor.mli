val handle_term
  :  gensym:Gensym.t
  -> env:Renaming.t
  -> renaming:Renaming.t
  -> Raw_term.t
  -> Raw_term.t

val handle_rule : renaming:Renaming.t -> Raw_program.rule -> Raw_program.rule
