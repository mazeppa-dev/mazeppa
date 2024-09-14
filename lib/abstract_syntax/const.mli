type t =
  | Int of Checked_oint.generic
  | String of string
[@@deriving eq, show]

val signedness_to_string : Checked_oint.signedness -> string

val signedness_to_string_uppercase : Checked_oint.signedness -> string

val bitness_to_string : Checked_oint.bitness -> string

val int_ty_to_string : Checked_oint.int_ty -> string

val int_ty_to_string_uppercase : Checked_oint.int_ty -> string

val int_to_string : Checked_oint.generic -> string

val escape_char : char -> string

val escape_string : string -> string

val to_string : t -> string
