[@@@coverage off]

type t =
  | Var of Symbol.t
  | Const of Const.t
  | Call of Symbol.t * t list
  | Match of t * match_case list
  | Let of Symbol.t * t * t

and match_case = pattern * t

and pattern = Symbol.t * Symbol.t list [@@deriving eq, show]

let var x = Var (Symbol.of_string x)

let int x = Const (Const.Int x)

let string s = Const (Const.String s)

let call (op, args) = Call (Symbol.of_string op, args)

let var_list symbols = List.map (fun x -> Var x) symbols

let is_immediate = function
  | Var _ | Const _ -> true
  | Call (c, []) when Symbol.kind c = `CCall -> true
  | Call _ | Match _ | Let _ -> false
;;

let to_string =
    let rec go = function
      | Var x -> Symbol.to_string x
      | Const const -> Const.to_string const
      | Call (op, args) ->
        let op, args = Symbol.to_string op, String.concat ", " (List.map go args) in
        [%string "$op($args)"]
      | Match (t, cases) ->
        let t, cases = go t, String.concat " | " (List.map go_case cases) in
        [%string "match $t { $cases }"]
      | Let (x, t, u) ->
        let x, t, u = Symbol.to_string x, go t, go u in
        [%string "let $x = $t; $u"]
    and go_case ((c, c_params), t) =
        let c, c_params, t = Symbol.to_string c, Symbol.comma_sep c_params, go t in
        [%string "$c($c_params) -> $t"]
    in
    go
;;

let verbatim t = "`" ^ to_string t ^ "`"

let pattern_to_string (c, c_params) =
    let c, c_args = Symbol.to_string c, Symbol.comma_sep c_params in
    [%string "$c($c_args)"]
;;

let pattern_verbatim pattern = "`" ^ pattern_to_string pattern ^ "`"
