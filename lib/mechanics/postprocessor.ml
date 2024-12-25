[@@@coverage off]

open Raw_term

let symbol = Symbol.of_string

(* TODO: handle other types of restrictions as well. *)
type restriction = NotEqual of Raw_term.t

let handle_term ~(gensym : Gensym.t) ~(env : Renaming.t) ~(renaming : Renaming.t) t =
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
    let hermetic k force =
        let gensym_backup = Gensym.clone gensym in
        let result = k force in
        Gensym.assign ~other:gensym_backup gensym;
        result
    in
    let exception Select of Raw_term.t in
    let rec go ~env = function
      | Var x -> Var (Symbol_map.find x env)
      | Const _ as t -> t
      | Call (op, [ Var x; t ])
        when op = symbol "=" && is_conflict (x, hermetic (go ~env) t) ->
        Call (symbol "F", [])
      | Call (op, [ t; Var x ])
        when op = symbol "=" && is_conflict (x, hermetic (go ~env) t) ->
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
           let cases = List.map (go_case ~env) cases in
           Match (t, cases)
         with
         | Select t -> t)
      | Let (x, t, u) ->
        let t = go ~env t in
        let env, y = Renaming.insert ~gensym ~renaming (env, x) in
        let u = go ~env u in
        Let (y, t, u)
    and go_restrict ~env ~x ~negation t =
        let negation = go ~env negation in
        match negation with
        | Var _ | Const _ ->
          Hashtbl.add restrictions x (NotEqual negation);
          let t = go ~env t in
          Hashtbl.remove restrictions x;
          t
        | _ -> go ~env t
    and go_case ~env ((c, c_params), t) =
        let env, c_params' = Renaming.insert_list ~gensym ~renaming (env, c_params) in
        (c, c_params'), go ~env t
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
    go ~env t
;;

let handle_rule ~(renaming : Renaming.t) ((attrs, f, params, body) : Raw_program.rule)
  : Raw_program.rule
  =
    let gensym = Gensym.create ~prefix:"v" () in
    let env = Symbol_map.empty in
    let env, params' = Renaming.insert_list ~gensym ~renaming (env, params) in
    attrs, f, params', handle_term ~gensym ~env ~renaming body
;;
