[@@@coverage exclude_file]

type document = PPrint.document

let atom ?(break_with = PPrint.hardline) s =
    PPrint.(group (ifflat empty break_with ^^ string s))
;;

let empty = PPrint.empty

let space = PPrint.space

let hardline = PPrint.hardline

let break = PPrint.break

let group = PPrint.group

let nest ~offset doc = PPrint.nest offset doc

let combine ?(group = false) ?(nest = 0) ?(sep = [ empty ]) list =
    let doc = PPrint.nest nest PPrint.(separate (concat sep) list) in
    if group then PPrint.group doc else doc
;;

let parens ?group ?nest ?sep list =
    combine ?group ?nest ?sep [ atom "("; combine list; atom ")" ]
;;

let braces ?group ?nest ?sep list =
    combine ?group ?nest ?sep [ atom "{"; combine list; atom "}" ]
;;

let brackets ?group ?nest ?sep list =
    combine ?group ?nest ?sep [ atom "["; combine list; atom "]" ]
;;

let comma_sep ?group ?nest list = combine ?group ?nest ~sep:[ atom ","; space ] list

type printable =
  { to_string : unit -> string
  ; output : oc:out_channel -> unit
  }

let build ?(width = 80) doc =
    let to_string () =
        let buffer = Buffer.create 1024 in
        PPrint.ToBuffer.pretty 1.0 width buffer doc;
        Buffer.contents buffer
    in
    let output ~oc = PPrint.ToChannel.pretty 1.0 width oc doc in
    { to_string; output }
;;

let to_string { to_string; _ } = to_string ()

let output ~oc { output; _ } = output ~oc
