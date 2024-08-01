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

module Make (_ : sig end) = struct
  external address_of_value : 'a -> int = "address_of_value"

  module Eph = Ephemeron.K1.Make (struct
      type t = Term.t

      let equal = ( == )

      let hash = address_of_value
    end)

  let table = Eph.create 1024

  let rec memoize = function
    | Var _ | Const _ -> 1
    | Call (_op, args) as t ->
      let result = 1 + List.fold_left (fun acc t -> acc + memoize t) 0 args in
      Eph.replace table t result;
      result
  ;;

  let size t =
      match Eph.find_opt table t with
      | Some result -> result
      | None -> memoize t
  ;;

  let rec decide : Term.t * Term.t -> bool = function
    | Var _x, Var _y -> true
    | Const const, Const const' -> decide_const (const, const')
    | t1, (Call (_op, args) as t2) ->
      let t1_size, t2_size = size t1, size t2 in
      if t1_size = t2_size
      then decide_by_coupling (t1, t2)
      else if t1_size < t2_size
      then decide_by_diving (t1, args) || decide_by_coupling (t1, t2)
      else false
    | (Var _ | Const _ | Call _), _ -> false

  and decide_by_diving (t1, args) = List.exists (fun t2 -> decide (t1, t2)) args

  and decide_by_coupling = function
    | Call (op, args), Call (op', args') when op = op' ->
      List.for_all2 (fun t1 t2 -> decide (t1, t2)) args args'
    | _, _ -> false
  ;;
end
