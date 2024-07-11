[@@@ocamlformat "module-item-spacing = compact"]

type document

(* These sigature types ensure that documents are "breakable" up to atoms (i.e., atoms can
   be printed on separate lines). *)

val atom : ?break_with:document -> string -> document
val empty : document
val space : document
val hardline : document
val break : int -> document
val group : document -> document
val nest : offset:int -> document -> document
val combine : ?group:bool -> ?nest:int -> ?sep:document list -> document list -> document
val parens : ?group:bool -> ?nest:int -> ?sep:document list -> document list -> document
val braces : ?group:bool -> ?nest:int -> ?sep:document list -> document list -> document
val brackets : ?group:bool -> ?nest:int -> ?sep:document list -> document list -> document
val comma_sep : ?group:bool -> ?nest:int -> document list -> document

type printable

val build : ?width:int -> document -> printable
val to_string : printable -> string
val output : oc:out_channel -> printable -> unit
