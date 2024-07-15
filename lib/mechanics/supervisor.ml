[@@@coverage off]

module MakeMutable (S : sig
    val program : Program.t

    val inspect : bool

    val observe_node : Symbol.t * Term.t -> unit

    val unobserve_node : Symbol.t -> unit
  end) : sig
  val run : Term.t -> Process_graph.t
end = struct
  open Process_graph

  let var_gensym = Gensym.create ~prefix:".v" ()

  module Driver = Driver.Make (struct
      include S

      let gensym = var_gensym
    end)

  module State = struct
    let node_gensym : Gensym.t = Gensym.create ~prefix:"n" ()

    let processed_globals : (Term.t * Symbol.t) list ref = ref []

    let backup () =
        let gensym, globals = Gensym.clone node_gensym, !processed_globals in
        gensym, globals
    ;;

    let update (gensym, globals) =
        Gensym.assign ~other:gensym node_gensym;
        processed_globals := globals
    ;;
  end

  (* We do not store terms in the resulting process graph. Logging is used to recover them
     for inspection purposes. *)
  let report_node (n : Term.t) : Symbol.t =
      let n_id = Gensym.emit State.node_gensym in
      S.observe_node (n_id, n);
      n_id
  ;;

  (* Unreporting is used to "forget" nodes when generalization is performed. *)
  let unreport_nodes (gensym_backup : Gensym.t) : unit =
      let iter = Gensym.clone gensym_backup in
      let until = Gensym.current State.node_gensym in
      let rec go () =
          let node_id = Gensym.emit iter in
          S.unobserve_node node_id;
          if node_id <> until then go ()
      in
      go ()
  ;;

  (* When node [m] (the ancestor) is homeomorphically embedded into [n] (the child), we
     issue a generalization of [m]. Alternatively, we could generalize [n], but doing so
     increases the size of residual code.

     When the generalization of [m] is not safe (i.e., the substitution contains potential
     loops and panics), we resort to splitting [m]. *)
  exception Failback of Symbol.t * [ `Generalize of Subst.t * Term.t | `Split ]

  (* When there is a _nested_ redex call of a function marked [@extract], we replace this
     call with a fresh variable and proceed supercompiling the extracted call and the
     enclosing shell separately. This mechanism can be seen as giving the supercompiler
     information about "dangerous" functions. *)
  exception Extract of ((Symbol.t * Term.t) * Term.t)

  let is_extractable f = Symbol_set.mem f S.program.extract_f_rules

  let is_extracted_call = function
    | Term.Call (f, _args) when is_extractable f -> true
    | _ -> false
  ;;

  let extract_call : Term.t -> unit =
      let open Term in
      let rec go = function
        | Call (f, _args) as t when is_extractable f ->
          let x = Gensym.emit var_gensym in
          raise_notrace (Extract ((x, t), Var x))
        | Call (op, args) when not (Symbol.is_lazy op) -> go_args ~op ~acc:[] args
        | Var _ | Const _ | Call _ -> ()
      and go_args ~op ~acc = function
        | [] -> ()
        | t :: rest when is_value t -> go_args ~op ~acc:(t :: acc) rest
        | t :: rest ->
          (try go t with
           | Extract ((x, call), shell) ->
             let shell = Call (op, List.rev acc @ [ shell ] @ rest) in
             raise_notrace (Extract ((x, call), shell)))
      in
      fun t -> if not (is_extracted_call t) then go t
  ;;

  let rec run ~(history : History.t) (n : Term.t) : Process_graph.t =
      let n_id = report_node n in
      let gensym_backup, globals_backup = State.backup () in
      try check_extract ~history (n_id, n) with
      | Failback (m_id, action) as exn ->
        if m_id = n_id
        then (
          unreport_nodes gensym_backup;
          State.update (gensym_backup, globals_backup);
          match action with
          | `Generalize (subst, g) -> generalize ~history ~bindings:subst g
          | `Split -> split ~history n)
        else (* Rethrow the exception until [m_id] is found. *)
          raise_notrace exn

  and check_extract ~history (n_id, n) =
      match extract_call n with
      | exception Extract extraction -> extract ~history extraction
      | () -> check_globals ~history (n_id, n)

  (* If a node is global, try to fold it against existing global nodes. If the node is not
     global or folding failed, continue with [check_whistle].

     This technique helps us eliminate many equivalent function definitions in residual
     programs. Since driving blindly duplicates the whole context when analyzing a
     g-function call, we essentially eschew re-supervising a potentially very large space
     of program states. Since there are not many global nodes (in comparison with locals
     and trivials), checking them all is doable.

     We also treat calls to extractable f-functions as globals. *)
  and check_globals ~history (n_id, n) =
      let rec go = function
        | [] ->
          let graph = check_whistle ~history (n_id, n) in
          State.processed_globals := (n, n_id) :: !State.processed_globals;
          graph
        | (m, m_id) :: rest ->
          (match Term.match_against (m, n) with
           | Some subst when Subst.is_safe subst -> fold ~history ~bindings:subst m_id
           | _ -> go rest)
      in
      match Term.classify n with
      | Term.Global -> go !State.processed_globals
      | _ when is_extracted_call n -> go !State.processed_globals
      | _ -> check_whistle ~history (n_id, n)

  (* If some ancestor node [m] is homeomorphically embedded into [n], do something with
     either [m] or [n]. Otherwise, drive [n].

     When the whistle blows, we must ensure that we break terms into structurally smaller
     components. For splitting, it means that we break calls with at least one argument
     that is not a variable. Consider that [m] is embedded into [n] either by coupling or
     diving. If [g] is a variable, [m] must be embedded by diving (because the uppermost
     operators are different), so at least one argument in [n] is not a variable. If [g]
     is not a variable, [m] is embedded by coupling, but after we check that [m] is not a
     safe instance of [n], we are sure that there is at least one argument in [n] that is
     not variable, so splitting [n] is again permitted.

     Before generalizing/splitting [m] at any point, we must check that doing so for [m]
     also makes it smaller; otherwise, we split [n]. Finally, before folding/generalizing
     any node, we must check that the suggested substitution is safe, so as to preserve
     call-by-value semantics of code. *)
  and check_whistle ~history (n_id, n) =
      match History.memoize ~suspect:(n_id, n) history with
      | Some (m_id, m), history ->
        (match Term.match_against (m, n) with
         | Some subst when Subst.is_safe subst -> fold ~history ~bindings:subst m_id
         | _ ->
           let g, subst_1, _subst_2 = Msg.compute ~gensym:var_gensym (m, n) in
           (match g with
            (* Split: there is no meaningful generalization of [m] and [n]. *)
            | Term.Var _x -> split ~history n
            (* Split: [g] is simply a renaming of [m]. *)
            | _ when Subst.is_renaming subst_1 -> split ~history n
            (* Split: there is a danger of removing some loops and panics. *)
            | _ when not (Subst.is_safe subst_1) ->
              (match m with
               (* Splitting [m] would not make it smaller, so split [n] instead. *)
               | Term.Call (_op, args) when List.for_all Term.is_var args ->
                 split ~history n
               (* Split upwards to optimize residual program size. *)
               | _ -> raise_notrace (Failback (m_id, `Split)))
            (* Perform upwards generalization otherwise. *)
            | _ -> raise_notrace (Failback (m_id, `Generalize (subst_1, g)))))
      | None, history -> Step (Driver.run ~f:(run ~history) n)

  and supercompile_bindings ~history subst =
      subst |> Symbol_map.bindings |> List.map (fun (x, t) -> x, run ~history t)

  and fold ~history ~bindings m_id =
      let bindings_sup = supercompile_bindings ~history bindings in
      Bind (bindings_sup, Fold m_id)

  and generalize ~history ~bindings g =
      let bindings_sup = supercompile_bindings ~history bindings in
      let g_sup = run ~history g in
      Bind (bindings_sup, Generalize g_sup)

  and split ~history = function
    | Term.Call (op, args) ->
      let fresh_vars = Gensym.emit_list ~length_list:args var_gensym in
      let bindings_sup = List.map2 (fun x t -> x, run ~history t) fresh_vars args in
      let g_sup = run ~history Term.(Call (op, var_list fresh_vars)) in
      Bind (bindings_sup, Split g_sup)
    | _ -> Util.panic "Impossible"

  and extract ~history ((x, call), shell) =
      let call_sup = run ~history call in
      let shell_sup = run ~history shell in
      Process_graph.Extract ((x, call_sup), shell_sup)
  ;;

  let run (t : Term.t) : Process_graph.t = run ~history:History.empty t
end

module Make (S : sig
    val program : Program.t

    val inspect : bool

    val observe_node : Symbol.t * Term.t -> unit

    val unobserve_node : Symbol.t -> unit
  end) =
struct
  let run (t : Term.t) : Process_graph.t =
      let module Runner = MakeMutable (S) in
      Runner.run t
  ;;
end

module MakeSimple (S : sig
    val program : Program.t
  end) =
struct
  let run (t : Term.t) : Process_graph.t =
      let module Runner =
        MakeMutable (struct
          include S

          let inspect = false

          let observe_node (_id, _node) = ()

          let unobserve_node _id = ()
        end)
      in
      Runner.run t
  ;;
end
