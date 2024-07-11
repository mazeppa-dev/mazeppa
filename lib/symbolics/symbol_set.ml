[@@@coverage exclude_file]

include Set.Make (Symbol)

let decouple (list : ('a * t) list) : 'a list * t =
    List.fold_right (fun (x, x_acc) (xs, acc) -> x :: xs, union x_acc acc) list ([], empty)
;;

let decouple_map ~(f : 'a -> 'b * t) (list : 'a list) : 'b list * t =
    List.fold_right
      (fun item k () ->
         (* Invoke [f] _before_ forcing the continuation to guarantee the left-to-right
            order of evaluation. *)
         let x, x_acc = f item in
         let xs, acc = k () in
         x :: xs, union x_acc acc)
      list
      (fun () -> [], empty)
      ()
;;
