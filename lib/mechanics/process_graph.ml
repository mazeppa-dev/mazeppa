[@@@coverage off]

type t =
  | Step of t Driver.step
  | Bind of (Symbol.t * t) list * binder

and binder =
  | Fold of node_id
  | Generalize of t
  | Split of t

and node_id = Symbol.t

type symbol_table = (Symbol.t * Program.param_list) Symbol_map.t

let compute_symbol_table (graph : t) : symbol_table =
    let f_gensym = Gensym.create ~prefix:"f" () in
    let table = ref Symbol_map.empty in
    let rec go = function
      | Step step -> go_step step
      | Bind (bindings, binder) -> go_binder ~bindings binder
    and go_step = function
      | Driver.(Var _ | Const _) -> ()
      | Driver.Decompose (_op, params) -> List.iter go params
      | Driver.Unfold graph -> go graph
      | Driver.Analyze (_x, graph, variants) ->
        go graph;
        List.iter
          (fun (_contraction, (binding, graph)) ->
             match binding with
             | Some binding -> go_extract (binding, graph)
             | None -> go graph)
          variants
      | Driver.Extract (binding, graph) -> go_extract (binding, graph)
    and go_extract ((x, call), graph) = go_binder ~bindings:[ x, call ] (Split graph)
    and go_binder ~bindings binder =
        List.iter (fun (_x, graph) -> go graph) bindings;
        match binder with
        | Fold node_id when not (Symbol_map.mem node_id !table) ->
          let f, params =
              Gensym.emit f_gensym, List.map (fun (x, _graph) -> x) bindings
          in
          table := Symbol_map.add node_id (f, params) !table
        (* We have already generated a symbol for this function. *)
        | Fold _ -> ()
        | Generalize graph | Split graph -> go graph
    in
    go graph;
    !table
;;
