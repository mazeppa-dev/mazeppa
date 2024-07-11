let pp_as_list fmt subst =
    subst
    |> Symbol_map.bindings
    |> [%derive.show: (Symbol.t * Symbol.t) list]
    |> Format.pp_print_string fmt
;;

type t = (Symbol.t Symbol_map.t[@printer pp_as_list]) [@@deriving eq, show]
