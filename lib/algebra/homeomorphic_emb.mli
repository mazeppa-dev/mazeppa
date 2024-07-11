(* [decide (t1, t2)] tells if [t1] is homeomorphically embedded into [t2] or not.

   Note: it makes sense to only compare terms having the same syntactic "category", as
   determined by [Term.classify]. *)
val decide : Term.t * Term.t -> bool
