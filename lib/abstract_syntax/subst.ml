[@@@coverage exclude_file]

let pp_as_list fmt subst =
    subst
    |> Symbol_map.bindings
    |> [%derive.show: (Symbol.t * Term.t) list]
    |> Format.pp_print_string fmt
;;

type t = (Term.t Symbol_map.t[@printer pp_as_list]) [@@deriving eq, show]

let is_renaming subst =
    Symbol_map.for_all
      (fun _x -> function
         | Term.Var _ -> true
         | _ -> false)
      subst
;;

let is_safe subst =
    Symbol_map.for_all
      (fun _x -> function
         | Term.(Var _ | Const _) -> true
         | Term.Call (op, _args) -> Symbol.kind op = `CCall)
      subst
;;
