[@@@coverage off]

type 'a step =
  | Var of Symbol.t
  | Const of Const.t
  | Decompose of Symbol.t * 'a list
  | Unfold of 'a
  | Analyze of Symbol.t * 'a * (contraction * 'a) list

and contraction = Symbol.t * Symbol.t list

let step_of_term : Term.t -> Term.t step = function
  | Term.Var x -> Var x
  | Term.Const const -> Const const
  | Term.Call (op, args) -> Decompose (op, args)
;;

let map ~(f : 'a -> 'b) : 'a step -> 'b step = function
  | Var x -> Var x
  | Const const -> Const const
  | Decompose (op, args) -> Decompose (op, List.map f args)
  | Unfold t -> Unfold (f t)
  | Analyze (x, t, variants) ->
    let t = f t in
    let variants = List.map (fun (contraction, t) -> contraction, f t) variants in
    Analyze (x, t, variants)
;;

let unify ~x ~contraction:(c, fresh_vars) list =
    match x with
    | None -> list
    | Some x ->
      let unifier = Term.(Call (c, var_list fresh_vars)) in
      let subst t = Term.subst_params ~params:[ x ] ~args:[ unifier ] t in
      List.map subst list
;;

let symbol = Symbol.of_string

let invalid_arg_list ~op args =
    Util.panic
      "Unexpected argument list for %s: %s"
      (Symbol.verbatim op)
      (args |> List.map Term.verbatim |> String.concat ",")
;;

module Make (S : sig
    val program : Program.t

    val inspect : bool

    val gensym : Gensym.t
  end) =
struct
  open S

  let rec reduce : Term.t -> Term.t step = function
    | Term.Var x -> Var x
    | Term.Const const -> Const const
    | Term.Call (op, args) when Symbol.is_lazy op -> reduce_call ~op (Symbol.kind op, args)
    | Term.Call (op, args) -> reduce_args (op, args)

  and reduce_args (op, args) =
      let rec go ~acc = function
        | [] -> reduce_call ~op (Symbol.kind op, List.rev acc)
        | t :: rest when Term.is_value t -> go ~acc:(t :: acc) rest
        | t :: rest -> reduce_amidst ~op (List.rev acc, t, rest)
      in
      go ~acc:[] args

  and reduce_amidst ~op (before, t, after) =
      match reduce t with
      | Var x -> Unfold Term.(Call (op, before @ [ Var x ] @ after))
      | Const const -> Unfold Term.(Call (op, before @ [ Const const ] @ after))
      | Decompose (c, [ _ ]) as step when c = symbol "Panic" -> step
      | Decompose (c, args) ->
        Unfold Term.(Call (op, before @ [ Call (c, args) ] @ after))
      | Unfold t -> Unfold (Term.Call (op, before @ [ t ] @ after))
      | Analyze (x, t, variants) ->
        variants
        |> List.map (fun (contraction, t) ->
          let unify list = unify ~x:(Some x) ~contraction list in
          contraction, Term.Call (op, unify before @ [ t ] @ unify after))
        |> fun variants -> Analyze (x, t, variants)

  and reduce_call ~op = function
    | `CCall, args -> Decompose (op, args)
    | `FCall, [ t ] when Symbol.is_op1 op -> step_of_term (Simplifier.handle_op1 ~op t)
    | `FCall, [ t1; t2 ] when Symbol.is_op2 op ->
      step_of_term (Simplifier.handle_op2 ~op (t1, t2))
    | `FCall, args when Symbol.is_primitive_op op -> invalid_arg_list ~op args
    | `FCall, args ->
      let params, body = Program.find_f_rule ~program op in
      Unfold (Simplifier.handle_term ~params ~args body)
    | `GCall, (([] | Term.Const _ :: _) as args) -> invalid_arg_list ~op args
    | `GCall, Term.Var x :: args ->
      Analyze (x, Term.Var x, unfold_g_rules ~x:(Some x) ~args op)
    | `GCall, Term.Call (op', args') :: args ->
      reduce_g_call ~op (Symbol.kind op', (op', args'), args)

  (* Reduces a g-function call where the first argument is also a call. *)
  and reduce_g_call ~op = function
    | `CCall, (c, c_args), args ->
      let c_params, params, body = Program.find_g_rule ~program (op, c) in
      Unfold
        (Simplifier.handle_term ~params:(c_params @ params) ~args:(c_args @ args) body)
    (* TODO: handle [!=]. *)
    | _, (op', (([ Var x; t ] | [ t; Var x ]) as args')), args when op' = symbol "=" ->
      let scrutinee = Term.Call (op', args') in
      let variants = unfold_g_rules_t_f ~test:(x, t) ~args op in
      Analyze (Gensym.emit gensym, scrutinee, variants)
    | _, (op', args'), args ->
      let scrutinee = Term.Call (op', args') in
      let variants = unfold_g_rules ~x:None ~args op in
      Analyze (Gensym.emit gensym, scrutinee, variants)

  and unfold_g_rules ~x ~args g =
      Program.(G_rules_by_name.bindings (find_g_rule_list ~program g))
      |> List.map (fun (c, (c_params, params, body)) ->
        let fresh_vars = Gensym.emit_list ~length_list:c_params gensym in
        let contraction = c, fresh_vars in
        let t =
            Simplifier.handle_term
              ~params:(c_params @ params)
              ~args:(Term.var_list fresh_vars @ unify ~x ~contraction args)
              body
        in
        contraction, t)

  and unfold_g_rules_t_f ~test:(x, unifier) ~args g =
      let unify t = Term.subst_params ~params:[ x ] ~args:[ unifier ] t in
      Program.(G_rules_by_name.bindings (find_g_rule_list ~program g))
      |> List.map (fun (c, (_c_params, params, body)) ->
        match Symbol.to_string c with
        | "T" -> (c, []), Simplifier.handle_term ~params ~args:(List.map unify args) body
        | "F" -> (c, []), Simplifier.handle_term ~params ~args body
        | _ -> Util.panic "Impossible")
  ;;

  let run ~f t = map ~f (reduce t)

  let try_run ~f t =
      try run ~f t with
      | Util.Panic { msg; reduction_path } ->
        Util.panic ~reduction_path:(Term.verbatim t :: reduction_path) "%s" msg
  ;;

  (* Notice that this function is not recursive! To enable continuous driving, [f] should
     call [run] inside itself. *)
  let run : f:(Term.t -> 'a) -> Term.t -> 'a step =
      (* Do not install the exception handler if inspection is disabled. *)
      if inspect then try_run else run
  ;;
end
