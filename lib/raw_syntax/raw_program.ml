[@@@coverage off]

type t = rule list

and rule = attr_list * Symbol.t * Symbol.t list * Raw_term.t

and attr_list = [ `Extract ] list [@@deriving eq, show]
