type t =
  | Var of Symbol.t
  | Const of Const.t
  | Call of Symbol.t * t list
[@@deriving eq, show]

type category =
  | Global
  | Local
  | Trivial
[@@deriving eq, show]

type value_category =
  | VConst
  | VCCall of Symbol.t
  | VNeutral
[@@deriving eq, show]

type redex_sig = (Symbol.t * value_category list) option [@@deriving eq, show]

val var : string -> t

val int : Checked_oint.generic -> t

val string : string -> t

val call : string * t list -> t

val var_list : Symbol.t list -> t list

val of_bool : bool -> t

val panic : ('a, unit, string, t) format4 -> 'a

val subst : env:t Symbol_map.t -> t -> t

val subst_params : params:Symbol.t list -> args:t list -> t -> t

(* If [t2] is an instance of [t1], [match_against (t1, t2)] returns an environment [env]
   such that [equal (subst ~env t1) t2].

   The map will contain bindings for all free variables in [t1]. *)
val match_against : t * t -> t Symbol_map.t option

(* Just like [match_against] but produces a renaming or returns [None]. *)
val rename_against : t * t -> t Symbol_map.t option

val is_var : t -> bool

val is_const : t -> bool

val is_value : t -> bool

val is_neutral : t -> bool

val classify : t -> category

val redex_sig : t -> redex_sig

val to_string : t -> string

val verbatim : t -> string
