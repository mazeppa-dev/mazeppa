open Term

[@@@coverage off]

type t = Term.t * Subst.t * Subst.t [@@deriving eq, show]

[@@@coverage on]

let scan_subst ~f subst = Seq.find_map f (Symbol_map.to_seq subst)

let common_functor ~gensym (g, subst_1, subst_2) =
    subst_1
    |> scan_subst ~f:(function
      | x, Call (op, args) ->
        subst_2
        |> scan_subst ~f:(function
          | y, Call (op', args') when x = y && op = op' -> Some (x, op, args, args')
          | _ -> None)
      | _ -> None)
    |> Option.map (fun (x, op, args, args') ->
      let fresh_vars = Gensym.emit_list ~length_list:args gensym in
      let common_call = Call (op, var_list fresh_vars) in
      let resubst ~args subst =
          Symbol_map.(extend2 ~keys:fresh_vars ~values:args (remove x subst))
      in
      let g = subst ~x ~value:common_call g in
      let subst_1, subst_2 = resubst ~args subst_1, resubst ~args:args' subst_2 in
      g, subst_1, subst_2)
;;

let common_subst (g, subst_1, subst_2) =
    subst_1
    |> scan_subst ~f:(fun (x, s1) ->
      subst_1
      |> scan_subst ~f:(fun (y, s2) ->
        if x <> y
           && Term.equal s1 s2
           && Option.bind (Symbol_map.find_opt x subst_2) (fun t1 ->
                Symbol_map.find_opt y subst_2 |> Option.map (fun t2 -> Term.equal t1 t2))
              |> Option.value ~default:false
        then Some (x, y)
        else None))
    |> Option.map (fun (x, y) ->
      let g = subst ~x ~value:(Var y) g in
      let subst_1, subst_2 = Symbol_map.(remove x subst_1, remove x subst_2) in
      g, subst_1, subst_2)
;;

let step ~rules triple = List.find_map (fun f -> f triple) rules

(* Exhaustively applies the two rewrite rules. *)
let rec loop ~gensym triple =
    match step ~rules:[ common_functor ~gensym; common_subst ] triple with
    | Some triple' -> loop ~gensym triple'
    | None -> triple
;;

let compute ~(gensym : Gensym.t) ((t1, t2) : Term.t * Term.t) : t =
    let seed = Gensym.emit gensym in
    let g, subst_1, subst_2 =
        Var seed, Symbol_map.singleton seed t1, Symbol_map.singleton seed t2
    in
    loop ~gensym (g, subst_1, subst_2)
;;
