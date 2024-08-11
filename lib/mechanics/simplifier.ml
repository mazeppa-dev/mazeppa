[@@@coverage off]

(* To avoid confusion with variables, integers are named [m] or [n] in this file.
   (Normally, we name both [x], [y], or [z].) *)

open Term
open Checked_oint

let symbol = Symbol.of_string

let invalid_arg_list ~op args =
    Util.panic
      "Unexpected argument list for %s: %s"
      (Symbol.verbatim op)
      (args |> List.map Term.verbatim |> String.concat ",")
;;

let to_int (n : Checked_oint.generic) : int =
    let (module Singleton) = Checked_oint.singleton n in
    let repr : string = Singleton.(to_string value) in
    try int_of_string repr with
    | Failure _ ->
      (* This is possible but should not happen normally. *)
      Util.panic "Out of bounds: %s" repr
;;

let do_int_op1 ~op (module S : Checked_oint.Singleton) =
    let of_int n = int (S.to_generic n) in
    let cast ty =
        let repr = S.(show value) in
        let (module Target) = Checked_oint.ops ty in
        let result = Target.(to_generic (of_string_exn repr)) in
        int result
    in
    try
      match Symbol.to_string op with
      | "~" -> S.(bit_not value) |> of_int
      | "string" -> S.(show value) |> string
      | "u8" -> cast (Unsigned, Bits8)
      | "u16" -> cast (Unsigned, Bits16)
      | "u32" -> cast (Unsigned, Bits32)
      | "u64" -> cast (Unsigned, Bits64)
      | "u128" -> cast (Unsigned, Bits128)
      | "i8" -> cast (Signed, Bits8)
      | "i16" -> cast (Signed, Bits16)
      | "i32" -> cast (Signed, Bits32)
      | "i64" -> cast (Signed, Bits64)
      | "i128" -> cast (Signed, Bits128)
      | _ -> Util.panic "Unexpected integer unary operator: %s" (Symbol.verbatim op)
    with
    | Checked_oint.Out_of_range ->
      panic
        "out of range: %s(%s)"
        (Symbol.to_string op)
        Const.(to_string (Int S.(to_generic value)))
;;

let do_int_op2 ~op (module S : Checked_oint.Pair) =
    let of_int n = int (S.to_generic n) in
    let m, n = S.value in
    try
      match Symbol.to_string op with
      | "+" -> S.add_exn m n |> of_int
      | "-" -> S.sub_exn m n |> of_int
      | "*" -> S.mul_exn m n |> of_int
      | "/" -> S.div_exn m n |> of_int
      | "%" -> S.rem_exn m n |> of_int
      | "|" -> S.bit_or m n |> of_int
      | "&" -> S.bit_and m n |> of_int
      | "^" -> S.bit_xor m n |> of_int
      | "<<" -> S.shift_left_exn m n |> of_int
      | ">>" -> S.shift_right_exn m n |> of_int
      | "=" -> S.equal m n |> of_bool
      | "!=" -> (not (S.equal m n)) |> of_bool
      | ">" -> S.compare m n > 0 |> of_bool
      | ">=" -> S.compare m n >= 0 |> of_bool
      | "<" -> S.compare m n < 0 |> of_bool
      | "<=" -> S.compare m n <= 0 |> of_bool
      | _ -> Util.panic "Unexpected integer binary operator: %s" (Symbol.verbatim op)
    with
    | Checked_oint.Out_of_range ->
      panic
        "out of range: %s(%s, %s)"
        (Symbol.to_string op)
        Const.(to_string (Int (S.to_generic m)))
        Const.(to_string (Int (S.to_generic n)))
;;

let do_string_op1 ~op s =
    match Symbol.to_string op with
    | "length" ->
      let n = U64.of_int_exn (String.length s) in
      int (U64 n)
    | "string" -> string s
    | _ -> Util.panic "Unexpected string unary operator: %s" (Symbol.verbatim op)
;;

let do_string_op2 ~op (s1, s2) =
    match Symbol.to_string op with
    | "=" -> s1 = s2 |> of_bool
    | "!=" -> s1 <> s2 |> of_bool
    | ">" -> s1 > s2 |> of_bool
    | ">=" -> s1 >= s2 |> of_bool
    | "<" -> s1 < s2 |> of_bool
    | "<=" -> s1 <= s2 |> of_bool
    | "++" -> string (s1 ^ s2)
    | _ -> Util.panic "Unexpected string binary operator: %s" (Symbol.verbatim op)
;;

let handle_op1 ~(op : Symbol.t) : t -> t =
    let involutory_ops, idempotent_ops =
        ( [ "~" ] |> Symbol.list
        , [ "u8"; "u16"; "u32"; "u64"; "u128" ]
          @ [ "i8"; "i16"; "i32"; "i64"; "i128" ]
          @ [ "string" ]
          |> Symbol.list )
    in
    function
    | Const (Const.Int (U8 value)) when op = symbol "#" ->
      let c = char_of_int (to_int (U8 value)) in
      string (String.make 1 c)
    | Const (Const.Int n) ->
      let (module Singleton) = Checked_oint.singleton n in
      do_int_op1 ~op (module Singleton)
    | Const (Const.String s) -> do_string_op1 ~op s
    (* op(op(t)) -> t *)
    | Call (op', [ t ]) when List.mem op involutory_ops && op = op' -> t
    (* op(op(t)) -> op(t) *)
    | Call (op', [ t ]) when List.mem op idempotent_ops && op = op' -> Call (op', [ t ])
    (* The catch-call rule. *)
    | t -> Call (op, [ t ])
;;

let handle_op2 ~(op : Symbol.t) : t * t -> t = function
  | (Const (Const.Int m) as t1), (Const (Const.Int n) as t2) ->
    (match Checked_oint.pair_exn (m, n) with
     | (module Pair) -> do_int_op2 ~op (module Pair)
     | exception Invalid_argument _ -> invalid_arg_list ~op [ t1; t2 ])
  | Const (Const.String s1), Const (Const.String s2) -> do_string_op2 ~op (s1, s2)
  | Const (Const.String s), Const (Const.Int (U64 idx)) when op = symbol "get" ->
    let idx = to_int (U64 idx) in
    (match s.[idx] with
     | c -> int (U8 (U8.of_int_exn (int_of_char c)))
     | exception Invalid_argument _ -> panic "out of bounds: %du64" idx)
  (* +(t, 0), +(0, t) -> t *)
  (* |(t, 0), |(0, t) -> t *)
  | ((_t1 as t), (Const (Const.Int n) as _t2) | (Const (Const.Int n) as _t1), (_t2 as t))
    when (op = symbol "+" || op = symbol "|") && is_zero n -> t
  (* -(t, 0) -> t *)
  | t, Const (Const.Int n) when op = symbol "-" && is_zero n -> t
  (* *(t, 1), *(1, t) -> t *)
  | ((_t1 as t), (Const (Const.Int n) as _t2) | (Const (Const.Int n) as _t1), (_t2 as t))
    when op = symbol "*" && is_one n -> t
  (* *(t, 0), *(0, t) -> 0 *)
  (* &(t, 0), &(0, t) -> 0 *)
  | (_t1, (Const (Const.Int n) as _t2) | (Const (Const.Int n) as _t1), _t2)
    when (op = symbol "*" || op = symbol "&") && is_zero n -> int n
  (* |(t, all ones), |(all ones, t) -> all ones *)
  | (_t1, (Const (Const.Int n) as _t2) | (Const (Const.Int n) as _t1), _t2)
    when op = symbol "|" && is_all_ones n -> int n
  (* &(t, all ones), &(all ones, t) -> t *)
  | ((_t1 as t), (Const (Const.Int n) as _t2) | (Const (Const.Int n) as _t1), (_t2 as t))
    when op = symbol "&" && is_all_ones n -> t
  (* /(t, 1) -> t *)
  | t, Const (Const.Int n) when op = symbol "/" && is_one n -> t
  (* /(0, t) -> 0 *)
  | Const (Const.Int n), _t when op = symbol "/" && is_zero n -> int n
  (* %(t, 1) -> 0 *)
  | _t, Const (Const.Int n) when op = symbol "%" && is_one n ->
    let (module Singleton) = Checked_oint.singleton n in
    int Singleton.(to_generic zero)
  (* /(t, 0), %(t, 0) -> out of range *)
  | _t, Const (Const.Int n) when (op = symbol "/" || op = symbol "%") && is_zero n ->
    do_int_op2 ~op (Checked_oint.pair_exn (n, n))
  (* |(t, t), &(t, t) -> t *)
  | t1, t2 when (op = symbol "|" || op = symbol "&") && equal t1 t2 -> t1
  (* =(t, t), >=(t, t), <=(t, t) -> T() *)
  | t1, t2 when (op = symbol "=" || op = symbol ">=" || op = symbol "<=") && equal t1 t2
    -> Call (symbol "T", [])
  (* !=(t, t), >(t, t), <(t, t) -> F() *)
  | t1, t2 when (op = symbol "!=" || op = symbol ">" || op = symbol "<") && equal t1 t2 ->
    Call (symbol "F", [])
  (* The catch-call rule. *)
  | t1, t2 -> Call (op, [ t1; t2 ])
;;
