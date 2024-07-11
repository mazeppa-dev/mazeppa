type t = Term.t Symbol_map.t [@@deriving eq, show]

(* Tests if a substitution maps variables to variables. *)
val is_renaming : t -> bool

(* Tests if a substitution maps variables to "safe" terms: variables, constructors, and
   constants, which terminate and do not panic. *)
val is_safe : t -> bool
