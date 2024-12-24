[@@@coverage off]

(* Invariants: 1) We arrange function parameters lexicographically, so that all functions
   are called with proper argument positions. 2) We only generate function definitions
   which are called 2+ times -- useless functions are "inlined" automatically without
   further transformation. 3) When processing a graph node, the history length must be the
   same as during construction of a graph. *)

(* This is the environment for efficient explicit substitutions. *)
type environment = Raw_term.t Symbol_map.t

let match' (t, cases) =
    let open Raw_term in
    let exception Replace of Raw_term.t in
    let go = function
      (* Before: [match Ci() { C1(...) -> t1, ..., Ci() -> tI, ..., Cn(...) -> tN }] *)
      (* After: [tI] *)
      | Call (c, []), cases
        when List.exists
               (function
                 | (c', []), t when c = c' -> raise_notrace (Replace t)
                 | _ -> false)
               cases -> ()
      (* Before: [match x { C1(...) -> op(), ..., Cn(...) -> op() }] *)
      (* After: [op()] *)
      | Var _x, (_pattern, Call (op, [])) :: rest
        when List.for_all
               (function
                 | _pattern', Call (op', []) when op = op' -> true
                 | _ -> false)
               rest -> raise_notrace (Replace (Call (op, [])))
      (* Before: [match t { C1(x1...) -> C1(x1...), ..., Cn(xN...) -> Cn(xN...) }] *)
      (* After: [t] *)
      | t, cases
        when List.for_all
               (function
                 | (c, c_params), Call (c', c_params') when c = c' ->
                   List.for_all2 (fun x y -> equal (Var x) y) c_params c_params'
                 | _ -> false)
               cases -> raise_notrace (Replace t)
      (* Nothing above applies. *)
      | _ -> ()
    in
    try
      go (t, cases);
      Match (t, cases)
    with
    | Replace better -> better
;;

(* Since all fresh variables in the process graph are distinct, there is no need to worry
   about potential shadowing. *)
let count_occurences ~x =
    let open Raw_term in
    let rec go = function
      | Var y when x = y -> 1
      | Var _ | Const _ -> 0
      | Call (_op, args) -> List.fold_left (fun acc t -> acc + go t) 0 args
      | Match (t, cases) ->
        go t + List.fold_left (fun acc (_pattern, t) -> acc + go t) 0 cases
      | Let (_x, t, u) -> go t + go u
    in
    go
;;

(* As with [count_occurences], there is no need to worry about name capture. *)
let subst ~x ~value =
    let open Raw_term in
    let rec go = function
      | Var y when x = y -> value
      | (Var _ | Const _) as t -> t
      | Call (op, args) -> Call (op, List.map go args)
      | Match (t, cases) ->
        Match (go t, List.map (fun (pattern, t) -> pattern, go t) cases)
      | Let (x, t, u) -> Let (x, go t, go u)
    in
    go
;;

(* [is_innocent t] is true whenever [t] has size 1 and does not diverge or panic, so that
   it can be safely substituted in a term. *)
let is_innocent =
    let open Raw_term in
    function
    | Var _ | Const (Const.Int _) -> true
    | Call (c, []) when Symbol.op_kind c = `CCall -> true
    | Const (Const.String _) | Call _ | Match _ | Let _ -> false
;;

(* [t] is substituted for [x] in [u] if either of the following holds: 1) [u] is linear
   with respect to [x] (i.e., [x] occurs only once in [u]) and it occurs in a redex
   position, or 2) [t] is an "innocent" term (whose size is always 1). The reasoning is to
   maintain call-by-value semantics while ensuring that the resulting term is not
   syntactically bigger than the initial one. *)
let let' (x, t, u) =
    let open Raw_term in
    let exception Rebuild of Raw_term.t in
    let rebuild better = raise_notrace (Rebuild better) in
    let rec go = function
      | Var y -> if x = y then rebuild t
      | Const _ -> ()
      | Call (op, _args) when Symbol.is_lazy_op op -> ()
      | Call (op, args) -> go_call ~op ~acc:Fun.id args
      | Match (t, cases) -> try_go ~f:(fun better -> Match (better, cases)) t
      | Let (x', t, u) -> try_go ~f:(fun better -> Let (x', better, u)) t
    and go_call ~op ~acc = function
      | [] -> ()
      | t :: rest ->
        try_go
          ~f:(fun better ->
            let args = acc [] @ [ better ] @ rest in
            Call (op, args))
          t;
        go_call ~op ~acc:(fun xs -> acc (t :: xs)) rest
    and try_go ~f t =
        try go t with
        | Rebuild better -> rebuild (f better)
    in
    try
      if count_occurences ~x u = 1 then go u;
      if is_innocent t then rebuild (subst ~x ~value:t u);
      Let (x, t, u)
    with
    | Rebuild better -> better
;;

let query_env ~env x = Option.value ~default:(Raw_term.Var x) (Symbol_map.find_opt x env)

module Memoizer (S : sig
    val symbol_table : Process_graph.symbol_table
  end) : sig
  val bind : environment -> (environment -> Raw_term.t) -> Raw_term.t

  val finalize : unit -> Raw_program.t
end = struct
  open S

  let node_gensym = Gensym.create ~prefix:"n" ()

  let f_rules : Raw_program.t ref = ref []

  let bind env k =
      let node_id = Gensym.emit node_gensym in
      match Symbol_map.find_opt node_id symbol_table with
      | Some (f, params) ->
        (* This will be the body of [f]; do not make any substitution. *)
        let t_res = k Symbol_map.empty in
        (* [params] contains all free variables in [t_res]. *)
        f_rules := Postprocessor.handle_rule ([], f, params, t_res) :: !f_rules;
        (* Some parameters may refer to bound variables; substitute. *)
        Raw_term.Call (f, List.map (query_env ~env) params)
      | None -> k env
  ;;

  let finalize () =
      List.sort
        (fun (_attrs1, f1, _params1, _t1) (_attrs2, f2, _params2, _t2) ->
           (* We do not generate duplicate function names. *)
           assert (f1 <> f2);
           Symbol.compare f1 f2)
        !f_rules
  ;;
end

let run ~(unknowns : Symbol.t list) (graph : Process_graph.t) : Raw_term.t * Raw_program.t
  =
    let symbol_table = Process_graph.compute_symbol_table graph in
    let module Memoizer =
      Memoizer (struct
        let symbol_table = symbol_table
      end)
    in
    let rec go ~(env : environment) (graph : Process_graph.t) : Raw_term.t =
        Memoizer.bind env (fun env ->
          match graph with
          | Process_graph.Step step -> go_step ~env step
          | Process_graph.Bind (bindings, binder) -> go_binder ~env ~bindings binder)
    and go_extract ~env ((x, call), graph) =
        let call_res = go ~env call in
        let t_res = go ~env graph in
        let' (x, call_res, t_res)
    and go_step ~(env : environment) = function
      | Driver.Var x -> query_env ~env x
      | Driver.Const const -> Raw_term.Const const
      | Driver.Decompose (op, args) ->
        let args_res = List.map (go ~env) args in
        Raw_term.Call (op, args_res)
      | Driver.Unfold graph -> go ~env graph
      | Driver.Analyze (_x, graph, variants) ->
        let t_res = go ~env graph in
        let cases_res =
            variants
            |> List.map (fun (contraction, (binding, graph)) ->
              match binding with
              | Some binding -> contraction, go_extract ~env (binding, graph)
              | None -> contraction, go ~env graph)
        in
        match' (t_res, cases_res)
      | Driver.Extract (binding, graph) -> go_extract ~env (binding, graph)
    and go_binder ~(env : environment) ~bindings = function
      | Process_graph.Fold node_id ->
        let f, _f_params = Symbol_map.find node_id symbol_table in
        let args_res = List.map (fun (_x, graph) -> go ~env graph) bindings in
        Raw_term.Call (f, args_res)
      | Process_graph.Generalize graph ->
        let env = Symbol_map.extend_map ~f:(go ~env) ~bindings env in
        go ~env graph
      | Process_graph.Split graph ->
        let innocent_bindings_res, other_bindings_res =
            partition_bindings ~env bindings
        in
        let env = Symbol_map.extend ~bindings:innocent_bindings_res env in
        let t_res = go ~env graph in
        List.fold_right
          (fun (x, t_res) res -> let' (x, t_res, res))
          other_bindings_res
          t_res
    and partition_bindings ~env bindings =
        bindings
        |> List.partition_map (fun (x, graph) ->
          let t_res = go ~env graph in
          if is_innocent t_res then Left (x, t_res) else Right (x, t_res))
    in
    let t_res = go ~env:Symbol_map.empty graph in
    let gensym = Gensym.create ~prefix:"x" () in
    let env, _ = Renaming.insert_list ~gensym (Symbol_map.empty, unknowns) in
    Postprocessor.handle_term ~gensym ~env t_res, Memoizer.finalize ()
;;
