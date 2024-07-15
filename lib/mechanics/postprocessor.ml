[@@@coverage off]

open Raw_term

let symbol = Symbol.of_string

(* Shares equal arguments in a call [op(t1, ..., tN)] by nested let-bindings. The rules
   are: 1) variables, constants, and zero-arity constructor calls (such as [T()] or [F()])
   are always inlined, 2) the original order of evaluating arguments (eager,
   left-to-right) is preserved, and 3) [gensym] is triggered only as many times as new
   let-bindings are (physically) generated. The result is a term that shares _all_
   syntactically equal arguments from the set [{ t1, ..., tN }].

   In order to meet the above criteria, we 1) use lazy symbols for expected let-bindings
   (which may or may not materialize) and 2) associate mutable cells called "levers" to
   every expected binding, so that unnecessary bindings can be easily and efficiently
   "inlined". Incidentally, lazy symbols are useful to track if some expected binding is
   shared or not: if the symbol was forced, then it was used as a variable at least twice
   in the call; otherwise, it was used only once. *)
let share_args ~gensym (op, initial_args) =
    let bindings, args = ref [], ref [] in
    let rec finalize_bindings () =
        match !bindings with
        | (x, t, lever) :: rest when not (Lazy.is_val x) ->
          lever := lazy t;
          bindings := rest;
          finalize_bindings ()
        | rest -> List.rev rest
    in
    let rec go = function
      | [] ->
        let bindings = finalize_bindings () in
        let args = List.(rev (map (fun lever -> Lazy.force !lever) !args)) in
        List.fold_right
          (fun ((lazy x), t, _lever) acc -> Let (x, t, acc))
          bindings
          (Call (op, args))
      | t :: rest when Raw_term.is_immediate t ->
        args := ref (lazy t) :: !args;
        go rest
      | t :: rest ->
        (match
           List.find_map
             (fun (x, t', _lever) -> if equal t t' then Some (Lazy.force x) else None)
             !bindings
         with
         | Some x ->
           args := ref (lazy (Var x)) :: !args;
           go rest
         | None ->
           let x = lazy (Gensym.emit gensym) in
           let lever = ref (lazy (Var (Lazy.force x))) in
           bindings := (x, t, lever) :: !bindings;
           args := lever :: !args;
           go rest)
    in
    if Symbol.is_lazy op
    then (* Let-bindings might affect the order of evaluation. *)
      Call (op, initial_args)
    else go initial_args
;;

let eliminate_match ~gensym_backup ~gensym (t, cases) =
    let exception Found of t in
    let try_alternatives = function
      (* Before: [match Ci() { C1(...) -> t1, ..., Ci() -> tI, ..., Cn(...) -> tN }] *)
      (* After: [tI] *)
      | Call (c, []), cases
        when List.exists
               (function
                 | (c', []), t when c = c' -> raise_notrace (Found t)
                 | _ -> false)
               cases -> ()
      (* Before: [match x { C1(...) -> op(), ..., Cn(...) -> op() }] *)
      (* After: [op()] *)
      | Var _x, (_pattern, Call (op, [])) :: rest
        when List.for_all
               (function
                 | _pattern', Call (op', []) when op = op' -> true
                 | _ -> false)
               rest -> raise_notrace (Found (Call (op, [])))
      (* Before: [match t { C1(x1...) -> C1(x1...), ..., Cn(xN...) -> Cn(xN...) }] *)
      (* After: [t] *)
      | t, cases
        when List.for_all
               (function
                 | (c, c_params), Call (c', c_params') when c = c' ->
                   List.for_all2 (fun x y -> equal (Var x) y) c_params c_params'
                 | _ -> false)
               cases -> raise_notrace (Found t)
      (* Nothing above applies. *)
      | _ -> ()
    in
    match try_alternatives (t, cases) with
    | exception Found better ->
      (* To avoid "hidden" variable symbols, restore the state of [gensym] before
         postprocessing the subterms. *)
      Gensym.assign ~other:gensym_backup gensym;
      better
    | () -> Match (t, cases)
;;

let query_env ~env x = Option.value ~default:x (Symbol_map.find_opt x env)

(* TODO: handle other types of restrictions as well. *)
type restriction = NotEqual of t

let handle_term ~(gensym : Gensym.t) ~(env : Renaming.t) t =
    (* We propagate negative information about primitives at residualization-time. During
       driving, we only propagate positive information. *)
    let restrictions = Hashtbl.create 128 in
    let is_conflict = function
      | x, ((Var _ | Const _) as t) ->
        (match Hashtbl.find_opt restrictions x with
         | Some (NotEqual t') when equal t t' -> true
         | _ -> false)
      | _ -> false
    in
    let rec go ~env = function
      | Var x -> Var (query_env ~env x)
      | Const const -> Const const
      | Call (op, [ Var x; t ]) when op = symbol "=" && is_conflict (x, go_subterm ~env t)
        -> Call (symbol "F", [])
      | Call (op, [ t; Var x ]) when op = symbol "=" && is_conflict (x, go_subterm ~env t)
        -> Call (symbol "F", [])
      | Call (op, args) ->
        (* The problem of sharing arguments has optimal substructure: by postprocessing
           arguments before feeding them to [share_args], we make the resulting term share
           all equal arguments which reside on the same syntactical level (a function
           call), throughout all levels in the term. *)
        share_args ~gensym (op, List.map (go_subterm ~env) args)
      | Match
          ( (Call (op, ([ Var x; negation ] | [ negation; Var x ])) as t)
          , [ ((c_f, []), case_f); ((c_t, []), case_t) ] )
        when op = symbol "=" && c_f = symbol "F" && c_t = symbol "T" ->
        let gensym_backup = Gensym.clone gensym in
        let t = go ~env t in
        let case_f = (c_f, []), go_restrict ~env ~x ~negation case_f in
        let case_t = (c_t, []), go ~env case_t in
        let cases = [ case_f; case_t ] in
        eliminate_match ~gensym_backup ~gensym (t, cases)
      | Match (t, cases) ->
        let gensym_backup = Gensym.clone gensym in
        let t = go ~env t in
        let cases = List.map (go_case ~env) cases in
        eliminate_match ~gensym_backup ~gensym (t, cases)
      | Let (x, t, u) ->
        let t = go_subterm ~env t in
        let gensym_backup = Gensym.clone gensym in
        let x' = Gensym.emit gensym in
        let u = go_extend ~env (([ x ], [ x' ]), u) in
        (* We could generalize the following rewriting rules for any operator arity, but
           it is enough to handle arities 1 and 2 for now. *)
        (match u with
         | Call (op, [ Var y ]) when x' = y && not (Symbol.is_lazy op) ->
           Gensym.assign ~other:gensym_backup gensym;
           Call (op, [ t ])
         | Let (y, s, Call (op, [ Var v1; Var v2 ]))
           when x' = v1 && y = v2 && not (Symbol.is_lazy op) ->
           Gensym.assign ~other:gensym_backup gensym;
           Call (op, [ t; s ])
         | _ -> Let (x', t, u))
    and go_subterm ~env t =
        let gensym_backup = Gensym.clone gensym in
        let t = go ~env t in
        Gensym.assign ~other:gensym_backup gensym;
        t
    and go_extend ~env ((params, params'), t) =
        let env = Symbol_map.extend2 ~keys:params ~values:params' env in
        go ~env t
    and go_restrict ~env ~x ~negation t =
        let negation = go_subterm ~env negation in
        match negation with
        | Var _ | Const _ ->
          Hashtbl.add restrictions x (NotEqual negation);
          let t = go ~env t in
          Hashtbl.remove restrictions x;
          t
        | _ -> go ~env t
    and go_case ~env ((c, c_params), t) =
        let c_params' = Gensym.emit_list ~length_list:c_params gensym in
        (c, c_params'), go_extend ~env ((c_params, c_params'), t)
    in
    go ~env t
;;

let handle_rule ((attrs, f, params, body) : Raw_program.rule) : Raw_program.rule =
    let gensym = Gensym.create ~prefix:"x" () in
    let params' = Gensym.emit_list ~length_list:params gensym in
    let env = Symbol_map.setup2 (params, params') in
    attrs, f, params', handle_term ~gensym ~env body
;;
