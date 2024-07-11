[@@@coverage exclude_file]

exception
  Panic of
    { msg : string
    ; reduction_path : string list
    }

let panic ?(reduction_path = []) fmt =
    Format.ksprintf (fun msg -> raise (Panic { msg; reduction_path })) fmt
;;
