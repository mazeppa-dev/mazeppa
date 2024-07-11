type 'a step =
  (* Stop on a free variable. *)
  | Var of Symbol.t
  (* Stop on a constant value. *)
  | Const of Const.t
  (* Decompose a constructor. *)
  | Decompose of Symbol.t * 'a list
  (* Unfold a call. *)
  | Unfold of 'a
  (* Case analysis of a g-call. *)
  | Analyze of Symbol.t * 'a * (contraction * 'a) list

and contraction = Symbol.t * Symbol.t list

module Make (_ : sig
    val program : Program.t

    (* This is only used to preserve terms in exception messages. *)
    val inspect : bool

    (* This is for generating fresh variables. *)
    val gensym : Gensym.t
  end) : sig
  val run : f:(Term.t -> 'a) -> Term.t -> 'a step
end
