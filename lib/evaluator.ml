let find_rule ~program f =
    try Hashtbl.find program f with
    | Not_found -> Util.panic "No such function %s" (Symbol.verbatim f)
;;

let find_case ~c cases =
    match
      List.find_map
        (fun ((c', c_params), t) -> if c = c' then Some (c_params, t) else None)
        cases
    with
    | Some (c_params, t) -> c_params, t
    | None -> Util.panic "No such case %s" (Symbol.verbatim c)
;;

let to_const = function
  | Raw_term.Const const -> Term.Const const
  | t -> Util.panic "Expected a constant: %s" (Raw_term.verbatim t)
;;

let of_const = function
  | Term.Const const -> Raw_term.Const const
  | Term.Call (c, []) when (Symbol.op_kind c = `CCall) [@coverage off] ->
    Raw_term.Call (c, []) [@coverage off]
  | Term.(Call (c, [ Const (Const.String s) ])) when c = Symbol.of_string "Panic" ->
    Raw_term.(Call (c, [ string s ]))
  | t -> Util.panic "Cannot reduce: %s" (Term.verbatim t)
;;

let to_c_call = function
  | Raw_term.Call (c, args) when Symbol.op_kind c = `CCall -> c, args
  | t -> Util.panic "Expected a constructor call: %s" (Raw_term.verbatim t)
;;

let find_var ~env x =
    match Symbol_map.find x env with
    | exception Not_found -> Util.panic "Unbound variable %s" (Symbol.verbatim x)
    | t -> t
;;

let rec subst ~env = function
  | Raw_term.Var x -> find_var ~env x
  | Raw_term.Const _ as t -> t
  | Raw_term.Call (op, args) -> Raw_term.Call (op, List.map (subst ~env) args)
  | Raw_term.Match (t, cases) ->
    Raw_term.Match
      (subst ~env t, List.map (fun (pattern, t) -> pattern, subst ~env t) cases)
  | Raw_term.Let (x, t, u) -> Raw_term.Let (x, subst ~env t, subst ~env u)
[@@coverage off]
;;

let invalid_arg_list ~op args =
    Util.panic
      "Unexpected argument list for %s: %s"
      (Symbol.verbatim op)
      (args |> List.map Raw_term.verbatim |> String.concat ",")
;;

(* We could use OCaml's exceptions for that, but let us keep the style as close as
   possible to the language definition. *)
let ( let$ ) t k =
    match t with
    | Raw_term.Call (c, [ _ ]) when c = Symbol.of_string "Panic" -> t
    | _ -> k t
;;

let run_exn (input : Raw_program.t) =
    let program =
        input
        |> List.map (fun (_attrs, f, params, body) -> f, (params, body))
        |> List.to_seq
        |> Hashtbl.of_seq
    in
    let rec go ~env = function
      | Raw_term.Var x -> go ~env:Symbol_map.empty (find_var ~env x)
      | Raw_term.Const _ as t -> t
      | Raw_term.Call (c, args) when Symbol.op_kind c = `CCall ->
        Raw_term.Call (c, List.map (subst ~env) args)
      | Raw_term.Call (op, [ t ]) when Symbol.is_op1 op ->
        (let$ t_val = go ~env t in
         of_const (Simplifier.handle_op1 ~op (to_const t_val))) [@coverage off]
      | Raw_term.Call (op, [ t1; t2 ]) when Symbol.is_op2 op ->
        let$ t1_val = go ~env t1 in
        let$ t2_val = go ~env t2 in
        of_const (Simplifier.handle_op2 ~op (to_const t1_val, to_const t2_val))
      | Raw_term.Call (op, args) when Symbol.is_primitive_op op ->
        invalid_arg_list ~op args
      | Raw_term.Call (f, args) -> go_args ~env ~f ~acc:Fun.id args
      | Raw_term.Match (t, cases) ->
        let$ t_val = go ~env t in
        let c, c_args = to_c_call t_val in
        let c_params, body = find_case ~c cases in
        let env = Symbol_map.extend2 ~keys:c_params ~values:c_args env in
        go ~env body
      | Raw_term.Let (x, t, u) ->
        (let$ t_val = go ~env t in
         go ~env:(Symbol_map.add x t_val env) u) [@coverage off]
    and go_args ~env ~f ~acc = function
      | [] -> go_call ~f (acc [])
      | t :: rest ->
        let$ t_val = go ~env t in
        go_args ~env ~f ~acc:(fun xs -> acc (t_val :: xs)) rest
    and go_call ~f args =
        let params, body = find_rule ~program f in
        let env = Symbol_map.setup2 (params, args) in
        go ~env body
    in
    let main_params, t = find_rule ~program (Symbol.of_string "main") in
    match main_params with
    | [] -> go ~env:Symbol_map.empty t
    | _ -> (Util.panic [@coverage off]) "The main function cannot accept parameters"
;;
