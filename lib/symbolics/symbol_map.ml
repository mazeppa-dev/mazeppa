[@@@coverage exclude_file]

include Map.Make (Symbol)

let extend ~bindings env = List.fold_left (fun acc (x, t) -> add x t acc) env bindings

let extend_map ~f ~bindings env =
    List.fold_left (fun acc (x, t) -> add x (f t) acc) env bindings
;;

let extend2 ~keys ~values env =
    List.fold_left2 (fun acc x t -> add x t acc) env keys values
;;

let setup bindings = extend ~bindings empty

let setup2 (keys, values) = extend2 ~keys ~values empty
