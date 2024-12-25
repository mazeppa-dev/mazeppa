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

let fresh_symbol ~gensym ~renaming ~env = function
  (* [x] was generated in the process of driving; we must generate a new symbol that has
     not been used yet. *)
  | x when (Symbol.to_string x).[0] = '.' ->
    let x' =
        (* [renaming] maps driver-generated variables to their counterparts from the
           original user program. If it contains a mapping for [x], we use it; otherwise,
           we generate a new proper symbol to replace [x]. *)
        match Symbol_map.find_opt x renaming with
        | Some x' -> x'
        | None -> Gensym.emit gensym
    in
    Symbol.freshen ~p:(not_in_codomain ~env) x'
  (* [x] was taken from the original program; leave it as it is. *)
  | x -> x
;;

let insert
      ~(gensym : Gensym.t)
      ?(renaming = ((Symbol_map.empty : t) [@coverage off]))
      ((env, x) : t * Symbol.t)
  : t * Symbol.t
  =
    let y = fresh_symbol ~gensym ~renaming ~env x in
    Symbol_map.add x y env, y
;;

let insert_list
      ~(gensym : Gensym.t)
      ?(renaming = (Symbol_map.empty : t))
      ((env, list) : t * Symbol.t list)
  : t * Symbol.t list
  =
    let env, list =
        list
        |> List.fold_left
             (fun (env, list) x ->
                let env, y = insert ~gensym ~renaming (env, x) in
                env, fun xs -> list (y :: xs))
             (env, Fun.id)
    in
    env, list []
;;
