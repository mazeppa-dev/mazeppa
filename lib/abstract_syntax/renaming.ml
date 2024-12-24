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

let fresh_symbol ~gensym ~env x =
    if (Symbol.to_string x).[0] = '.'
    then
      (* [x] is generated in the process of driving; if it clashes with some
         other symbol, we must rename it. *)
      Symbol.freshen ~p:(not_in_codomain ~env) (Gensym.emit gensym)
    else
      (* [x] is taken from the original input program. *)
      x
;;

let insert ~gensym ((env : t), x) : t * Symbol.t =
    let y = fresh_symbol ~gensym ~env x in
    Symbol_map.add x y env, y
;;

let insert_list ~gensym ((env : t), list) : t * Symbol.t list =
    let env, list =
        list
        |> List.fold_left
             (fun (env, list) x ->
                let env, y = insert ~gensym (env, x) in
                env, fun xs -> list (y :: xs))
             (env, Fun.id)
    in
    env, list []
;;
