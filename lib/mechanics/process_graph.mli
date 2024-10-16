(* Each node in a graph is implicitly labelled with its unique identifier. To derive
   identifiers for each node, setup [Gensym.create ~prefix:"n" ()] and invoke
   [Gensym.emit] on it, using a depth-first traversal. This convention allows us to
   reference graph nodes in a convenient and systematic way. *)
type t =
  | Step of t Driver.step
  (* Bindings must 1) contain _all_ free variables of a respective term and 2) be sorted
     in the lexicographic order. *)
  | Bind of (Symbol.t * t) list * binder

and binder =
  | Fold of node_id
  | Generalize of t
  | Split of t

and node_id = Symbol.t

type symbol_table = (Symbol.t * Program.param_list) Symbol_map.t

(* Computes a map from node identifiers to residualized function signatures. *)
val compute_symbol_table : t -> symbol_table
