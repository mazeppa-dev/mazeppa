[@@@coverage exclude_file]

type t =
  { prefix : string ref
  ; counter : int ref
  }

let create ~prefix () = { prefix = ref prefix; counter = ref 0 }

let make_symbol ~prefix i = Symbol.of_string (prefix ^ string_of_int i)

let emit gensym =
    let i = !(gensym.counter) in
    incr gensym.counter;
    make_symbol ~prefix:!(gensym.prefix) i
;;

let latest gensym =
    let i = !(gensym.counter) in
    assert (i > 0);
    make_symbol ~prefix:!(gensym.prefix) i
;;

let emit_list ~length_list gensym = List.map (fun _ -> emit gensym) length_list

let clone gensym = { prefix = ref !(gensym.prefix); counter = ref !(gensym.counter) }

let assign ~other gensym =
    gensym.prefix := !(other.prefix);
    gensym.counter := !(other.counter)
;;

let reset gensym = gensym.counter := 0
