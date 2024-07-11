[@@@coverage exclude_file]

type t =
  { prefix : string ref
  ; counter : int ref
  }

let create ~prefix () = { prefix = ref prefix; counter = ref 0 }

let emit gensym =
    let x = !(gensym.counter) in
    incr gensym.counter;
    Symbol.of_string (!(gensym.prefix) ^ string_of_int x)
;;

let current gensym =
    decr gensym.counter;
    emit gensym
;;

let emit_list ~length_list gensym = List.map (fun _ -> emit gensym) length_list

let clone gensym = { prefix = ref !(gensym.prefix); counter = ref !(gensym.counter) }

let assign ~other gensym =
    gensym.prefix := !(other.prefix);
    gensym.counter := !(other.counter)
;;

let reset gensym = gensym.counter := 0
