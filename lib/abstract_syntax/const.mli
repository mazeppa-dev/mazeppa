type t =
  | Int of Checked_oint.generic
  | String of string
[@@deriving eq, show]

val to_string : t -> string
