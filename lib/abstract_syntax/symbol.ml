[@@@coverage exclude_file]

open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type t = string [@@deriving eq, show, ord, yojson_of]

let of_string x = x

let to_string x = x

let verbatim x = "`" ^ x ^ "`"

let is_op1 = function
  | "~"
  | "#"
  | "length"
  | "string"
  | "u8"
  | "u16"
  | "u32"
  | "u64"
  | "u128"
  | "i8"
  | "i16"
  | "i32"
  | "i64"
  | "i128" -> true
  | _ -> false
;;

let is_op2 = function
  | "+"
  | "-"
  | "*"
  | "/"
  | "%"
  | "|"
  | "&"
  | "^"
  | "<<"
  | ">>"
  | "="
  | "!="
  | ">"
  | ">="
  | "<"
  | "<="
  | "++"
  | "get" -> true
  | _ -> false
;;

let is_primitive_op op = is_op1 op || is_op2 op

let kind op : [ `CCall | `FCall | `GCall ] =
    if op.[0] >= 'A' && op.[0] <= 'Z'
    then `CCall
    else if op.[0] >= 'a' && op.[0] <= 'z'
    then `FCall
    else if op.[0] = '.' && op.[1] = 'f'
    then `FCall
    else if op.[0] = '.' && op.[1] = 'g'
    then `GCall
    else if is_primitive_op op
    then `FCall
    else Util.panic "An unknown symbol kind: `%s`" op
;;

let is_lazy op : bool =
    match kind op with
    | `CCall -> true
    | `FCall | `GCall -> false
;;

let comma_sep list = String.concat ", " list

let comma_sep_verbatim list = String.concat ", " (List.map verbatim list)
