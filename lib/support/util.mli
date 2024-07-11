exception
  Panic of
    { msg : string
    ; reduction_path : string list
    }

val panic : ?reduction_path:string list -> ('a, unit, string, 'b) format4 -> 'a
