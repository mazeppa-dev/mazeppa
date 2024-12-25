[@@@coverage off]

let symbol = Symbol.of_string

(* TODO: handle other types of restrictions as well. *)
type restriction = NotEqual of Raw_term.t

(* We propagate negative information about primitives at residualization-time.
   During driving, we only propagate positive information. *)
let propagate_restrictions : env:restriction Symbol_map.t -> Raw_term.t -> Raw_term.t =
    let open Raw_term in
    let is_conflict ~env = function
      | x, ((Var _ | Const _) as t) ->
        (match Symbol_map.find_opt x env with
         | Some (NotEqual t') when equal t t' -> true
         | _ -> false)
      | _ -> false
    in
    let exception Select of Raw_term.t in
    let rec go ~env = function
      | (Var _ | Const _) as t -> t
      | Call (op, [ Var x; t ]) when op = symbol "=" && is_conflict ~env (x, t) ->
        Call (symbol "F", [])
      | Call (op, [ t; Var x ]) when op = symbol "=" && is_conflict ~env (x, t) ->
        Call (symbol "F", [])
      | Call (op, args) -> Call (op, List.map (go ~env) args)
      | Match
          ( (Call (op, ([ Var x; negation ] | [ negation; Var x ])) as t)
          , ([ ((c_f, []), case_f); ((c_t, []), case_t) ] as cases) )
        when op = symbol "=" && c_f = symbol "F" && c_t = symbol "T" ->
        (try
           let t = go_scrutinee ~env ~cases t in
           let case_f = (c_f, []), go_restrict ~env ~x ~negation case_f in
           let case_t = (c_t, []), go ~env case_t in
           Match (t, [ case_f; case_t ])
         with
         | Select t -> t)
      | Match (t, cases) ->
        (try
           let t = go_scrutinee ~env ~cases t in
           let cases = List.map (fun (pattern, t) -> pattern, go ~env t) cases in
           Match (t, cases)
         with
         | Select t -> t)
      | Let (x, t, u) -> Let (x, go ~env t, go ~env u)
    and go_restrict ~env ~x ~negation t =
        match go ~env negation with
        | (Var _ | Const _) as negation ->
          go ~env:(Symbol_map.add x (NotEqual negation) env) t
        | _ -> go ~env t
    and go_scrutinee ~env ~cases t =
        let t = go ~env t in
        (match t with
         | Call (c, []) ->
           cases
           |> List.iter (function
             | (c', []), t when c = c' -> raise_notrace (Select (go ~env t))
             | _ -> ())
         | _ -> ());
        t
    in
    go
;;

(* Generate proper symbols (i.e., without a leading dot), recovering as much
   original program symbols as possible. *)
let rename ~gensym ~fresh_to_source_vars : env:Renaming.t -> Raw_term.t -> Raw_term.t =
    let open Raw_term in
    let rec go ~env = function
      | Var x -> Var (Symbol_map.find x env)
      | Const _ as t -> t
      | Call (op, args) -> Call (op, List.map (go ~env) args)
      | Match (t, cases) ->
        let t = go ~env t in
        let cases = List.map (go_case ~env) cases in
        Match (t, cases)
      | Let (x, t, u) ->
        let t = go ~env t in
        let env, y = Renaming.insert ~gensym ~fresh_to_source_vars (env, x) in
        let u = go ~env u in
        Let (y, t, u)
    and go_case ~env ((c, c_params), t) =
        let env, c_params' =
            Renaming.insert_list ~gensym ~fresh_to_source_vars (env, c_params)
        in
        (c, c_params'), go ~env t
    in
    go
;;

let handle_rule
      ~(fresh_to_source_vars : Renaming.t)
      ((attrs, f, params, body) : Raw_program.rule)
  : Raw_program.rule
  =
    let body = propagate_restrictions ~env:Symbol_map.empty body in
    let gensym = Gensym.create ~prefix:"v" () in
    let env, params =
        Renaming.insert_list ~gensym ~fresh_to_source_vars (Symbol_map.empty, params)
    in
    let body = rename ~gensym ~fresh_to_source_vars ~env body in
    attrs, f, params, body
;;

let handle_main_body
      ~(fresh_to_source_vars : Renaming.t)
      ~(unknowns : Symbol.t list)
      (body : Raw_term.t)
  : Raw_term.t
  =
    let _, _, _, body =
        handle_rule ~fresh_to_source_vars ([], symbol "main", unknowns, body)
    in
    body
;;
