type t = Term.t * Subst.t * Subst.t [@@deriving eq, show]

(* Computes a Most Specific Generalization (MSG) of two terms. *)
val compute : gensym:Gensym.t -> Term.t * Term.t -> t
