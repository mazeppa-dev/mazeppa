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
  | Analyze of Symbol.t * 'a * (contraction * 'a case_body) list
  (* Extract a binding. *)
  | Extract of (Symbol.t * 'a) * 'a

and contraction =
  { c : Symbol.t
  ; fresh_vars : Symbol.t list
  ; source_vars : Symbol.t list
  }

and 'a case_body = (Symbol.t * 'a) option * 'a

module Make (_ : sig
    val program : Program.t

    (* This is only used to preserve terms in exception messages. *)
    val inspect : bool

    (* This is for generating fresh variables. *)
    val gensym : Gensym.t
  end) : sig
  val run : f:(Term.t -> 'a) -> Term.t -> 'a step
end
