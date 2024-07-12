open Term

(* Substitutes [args] for [params] and applies a number of simplification rules. By
   simplifying function bodies while expanding them, we avoid over-generalization
   (otherwise [1i32] and [-(1i32, 1i32)] would blow the whistle!) and reduce some amount
   of expensive homeomorphic embedding checks.

   In a sense, this function _partially evaluates_ a given term. However, to guarantee
   termination, it does not unfold function calls.

   [args] must contain only evaluated terms. *)
val handle_term : params:Symbol.t list -> args:t list -> t -> t

(* Performs [op] on a single value. *)
val handle_op1 : op:Symbol.t -> t -> t

(* Performs [op] on a pair of values. *)
val handle_op2 : op:Symbol.t -> t * t -> t
