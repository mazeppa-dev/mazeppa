open Raw_program

(* Converts [t] into [Program.t] while checking its well-formedness.

   Invariant: if [Program.t] was constructed using this function, supercompilation should
   not raise internal compiler errors. *)
val to_program : t -> Program.t
