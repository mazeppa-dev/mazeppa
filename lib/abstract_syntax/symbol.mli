type t [@@deriving eq, show, ord, yojson_of]

val of_string : string -> t

val list : string list -> t list

val to_string : t -> string

val verbatim : t -> string

(* Determines the kind of a functional symbol. *)
val kind : t -> [ `CCall | `FCall | `GCall ]

val is_lazy : t -> bool

val is_op1 : t -> bool

val is_op2 : t -> bool

val is_primitive_op : t -> bool

val comma_sep : t list -> string

val comma_sep_verbatim : t list -> string
