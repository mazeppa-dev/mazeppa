open Term

(* Substitutes [args] for [params] and applies a number of simplification rules. By
   simplifying function bodies while expanding them, we avoid over-generalization
   (otherwise [1i32] and [-(1i32, 1i32)] would blow the whistle!) and reduce some amount
   of expensive homeomorphic embedding checks.

   In a sense, this function _partially evaluates_ a given term. However, to guarantee
   termination, it does not unfold function calls. *)
val handle_term : params:Symbol.t list -> args:t list -> t -> t

val handle_op1 : op:Symbol.t -> t -> t

val handle_op2 : op:Symbol.t -> t * t -> t
