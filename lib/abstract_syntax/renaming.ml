[@@@coverage off]

let pp_as_list fmt subst =
    subst
    |> Symbol_map.bindings
    |> [%derive.show: (Symbol.t * Symbol.t) list]
    |> Format.pp_print_string fmt
;;

type t = (Symbol.t Symbol_map.t[@printer pp_as_list]) [@@deriving eq, show]

[@@@coverage on]

let not_in_codomain ~env y = Symbol_map.for_all (fun _ y' -> y <> y') env

let fresh_symbol ~gensym ~fresh_to_source_vars ~env = function
  (* [x] was generated in the process of driving; we must generate a new symbol that has
     not been used yet. *)
  | x when (Symbol.to_string x).[0] = '.' ->
    let x' =
        match Symbol_map.find_opt x fresh_to_source_vars with
        | Some x' -> x'
        | None -> Gensym.emit gensym
    in
    Symbol.freshen ~p:(not_in_codomain ~env) x'
  (* [x] was taken from the original program; leave it as it is. *)
  | x -> x
;;

let insert ~(gensym : Gensym.t) ~(fresh_to_source_vars : t) ((env, x) : t * Symbol.t)
  : t * Symbol.t
  =
    let y = fresh_symbol ~gensym ~fresh_to_source_vars ~env x in
    Symbol_map.add x y env, y
;;

let insert_list
      ~(gensym : Gensym.t)
      ~(fresh_to_source_vars : t)
      ((env, list) : t * Symbol.t list)
  : t * Symbol.t list
  =
    let env, list =
        list
        |> List.fold_left
             (fun (env, list) x ->
                let env, y = insert ~gensym ~fresh_to_source_vars (env, x) in
                env, fun xs -> list (y :: xs))
             (env, Fun.id)
    in
    env, list []
;;
