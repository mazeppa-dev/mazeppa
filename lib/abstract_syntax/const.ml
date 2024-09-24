[@@@coverage off]

type t =
  | Int of Checked_oint.generic
  | String of string
[@@deriving eq, show]

let signedness_to_string =
    let open Checked_oint in
    function
    | Signed -> "i"
    | Unsigned -> "u"
;;

let signedness_to_string_uppercase =
    let open Checked_oint in
    function
    | Signed -> "I"
    | Unsigned -> "U"
;;

let bitness_to_string =
    let open Checked_oint in
    function
    | Bits8 -> "8"
    | Bits16 -> "16"
    | Bits32 -> "32"
    | Bits64 -> "64"
    | Bits128 -> "128"
;;

let int_ty_to_string (signedness, bitness) =
    signedness_to_string signedness ^ bitness_to_string bitness
;;

let int_ty_to_string_uppercase (signedness, bitness) =
    signedness_to_string_uppercase signedness ^ bitness_to_string bitness
;;

let int_to_string x =
    let (module Singleton) = Checked_oint.singleton x in
    Singleton.(show value) ^ int_ty_to_string (Checked_oint.generic_int_ty x)
;;

[@@@coverage on]

let escape_char = function
  (* We would need the below rule for printing single characters, but we only need to
     print strings. *)
  (* | '\'' -> "\\'" *)
  | '\\' -> "\\\\"
  | '\012' -> "\\f"
  | '\n' -> "\\n"
  | '\r' -> "\\r"
  | '\t' -> "\\t"
  | '\011' -> "\\v"
  | c ->
    (match Char.escaped c with
     | s when s.[0] = '\\' -> Printf.sprintf "\\x%02X" (int_of_char c)
     | s -> s)
;;

let escape_string_char = function
  | '\'' -> "'"
  | '"' -> "\\\""
  | c -> escape_char c
;;

let escape_string s =
    "\"" ^ String.fold_left (fun acc c -> acc ^ escape_string_char c) "" s ^ "\""
;;

let to_string = function
  | Int x -> int_to_string x
  | String s -> escape_string s
;;
