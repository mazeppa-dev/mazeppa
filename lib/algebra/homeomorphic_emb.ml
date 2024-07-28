(* TODO: try to make the algorithm quadratic in time by memoizing terms. *)

open Term

(* Since the set of all Mazeppa integers is finite, we can treat each integer as a unique
   constructor. See issue #12 for more discussion. *)
let decide_ints (x, y) = Checked_oint.equal_generic x y

(* Strings are compared as sequences (lazy lists) of single-arity constructors: e.g.,
   "abc" is broken up into [A(B(C(Nil())))]. *)
let decide_strings (s1, s2) =
    let rec go (s1, s2) =
        match s1 (), s2 () with
        | Seq.(Cons (c1, s1), Cons (c2, s2)) when c1 = c2 -> go (s1, s2)
        | _, Seq.Cons (_c2, s2) -> go (s1, s2)
        | Seq.Nil, _ -> true
        | Seq.Cons _, _ -> false
    in
    go (String.to_seq s1, String.to_seq s2)
;;

let decide_const =
    let open Const in
    function
    | Int x, Int y -> decide_ints (x, y)
    | String s1, String s2 -> decide_strings (s1, s2)
    | (Int _ | String _), _ -> false
;;

let rec decide : Term.t * Term.t -> bool = function
  | Var _x, Var _y -> true
  | Const const, Const const' -> decide_const (const, const')
  | t1, (Call (_op, args) as t2) ->
    decide_by_diving (t1, args) || decide_by_coupling (t1, t2)
  | (Var _ | Const _ | Call _), _ -> false

and decide_by_diving (t1, args) = List.exists (fun t2 -> decide (t1, t2)) args

and decide_by_coupling = function
  | Call (op, args), Call (op', args') when op = op' ->
    List.for_all2 (fun t1 t2 -> decide (t1, t2)) args args'
  | _, _ -> false
;;
