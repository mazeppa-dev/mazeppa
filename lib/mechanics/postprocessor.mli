val handle_rule : fresh_to_source_vars:Renaming.t -> Raw_program.rule -> Raw_program.rule

val handle_main_body
  :  fresh_to_source_vars:Renaming.t
  -> unknowns:Symbol.t list
  -> Raw_term.t
  -> Raw_term.t
