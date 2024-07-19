let tab = "    "

let write_endline ~oc fmt =
    Printf.ksprintf (fun s -> Out_channel.output_string oc (s ^ "\n")) fmt
;;

let escape_double_quotes s = Str.(global_replace (regexp "\"") "\\\"" s)

let add_attrs attrs = if attrs = "" then "" else Printf.sprintf " [%s]" attrs

let draw_node ~oc ~label ?(attrs = "") current_node =
    write_endline
      ~oc
      "%s%s [label=\"%s: %s\"]%s;"
      tab
      (Symbol.to_string current_node)
      (Symbol.to_string current_node)
      (escape_double_quotes label)
      (add_attrs attrs)
;;

let draw_edge ~oc ~from_node ~to_node ~attrs =
    write_endline
      ~oc
      "%s%s -> %s%s;"
      tab
      (Symbol.to_string from_node)
      (Symbol.to_string to_node)
      (add_attrs attrs)
;;

let label fmt =
    Printf.ksprintf (fun s -> "label=\" " ^ escape_double_quotes s ^ " \"") fmt
;;

let connect ~oc ~attrs ~parent_node current_node =
    match parent_node with
    | Some from_node -> draw_edge ~oc ~from_node ~to_node:current_node ~attrs
    | None -> ()
;;

let run ~(oc : out_channel Lazy.t) (graph : Process_graph.t) : unit =
    let oc = Lazy.force oc in
    let node_gensym, inv_gensym =
        (* Nodes starting with [n] are "physical"; nodes starting with [inv] are
           "invisible", i.e., they are not real addresses. *)
        Gensym.(create ~prefix:"n" (), create ~prefix:"inv" ())
    in
    let rec go ?(attrs = "") ~parent_node graph =
        let current_node = Gensym.emit node_gensym in
        match graph with
        | Process_graph.Step (Driver.Var x) ->
          draw_node ~oc ~label:(Symbol.to_string x) current_node;
          connect ~oc ~attrs ~parent_node current_node
        | Process_graph.Step (Driver.Const const) ->
          draw_node ~oc ~label:(Const.to_string const) current_node;
          connect ~oc ~attrs ~parent_node current_node
        | Process_graph.Step (Driver.Decompose (op, args)) ->
          draw_node ~oc ~label:(Symbol.to_string op) current_node;
          connect ~oc ~attrs ~parent_node current_node;
          let parent_node = Some current_node in
          List.iteri (fun i graph -> go ~attrs:(label "#%d" i) ~parent_node graph) args
        | Process_graph.Step (Driver.Unfold graph) ->
          draw_node ~oc ~label:"<unfold>" current_node;
          connect ~oc ~attrs ~parent_node current_node;
          go ~parent_node:(Some current_node) graph
        | Process_graph.Step (Driver.Analyze (x, graph, variants)) ->
          draw_node ~oc ~label:"<analyze>" ~attrs:"shape=box style=bold" current_node;
          connect ~oc ~attrs ~parent_node current_node;
          let parent_node = Some current_node in
          go ~parent_node ~attrs:(label "%s" (Symbol.to_string x)) graph;
          go_variants ~parent_node (x, variants)
        | Process_graph.(Bind (bindings, Fold node_id)) ->
          draw_node ~oc ~label:"<fold>" ~attrs:"style=filled" current_node;
          connect ~oc ~attrs ~parent_node current_node;
          let parent_node = Some current_node in
          draw_edge ~oc ~from_node:current_node ~to_node:node_id ~attrs:"style=dashed";
          go_bindings ~parent_node bindings
        | Process_graph.(Bind (bindings, Generalize graph)) ->
          draw_node ~oc ~label:"<generalize>" ~attrs:"style=filled" current_node;
          connect ~oc ~attrs ~parent_node current_node;
          let parent_node = Some current_node in
          go_bindings ~parent_node bindings;
          go_body ~parent_node graph
        | Process_graph.(Bind (bindings, Split graph)) ->
          draw_node ~oc ~label:"<split>" ~attrs:"style=filled" current_node;
          connect ~oc ~attrs ~parent_node current_node;
          let parent_node = Some current_node in
          go_bindings ~parent_node bindings;
          go_body ~parent_node graph
        | Process_graph.(Extract ((x, call), graph)) ->
          go_extract ~attrs ~parent_node ~current_node ((x, call), graph)
    and go_extract ~attrs ~parent_node ~current_node ((x, call), graph) =
        draw_node ~oc ~label:"<extract>" ~attrs:"style=filled" current_node;
        connect ~oc ~attrs ~parent_node current_node;
        let parent_node = Some current_node in
        go_bindings ~parent_node [ x, call ];
        go_body ~parent_node graph
    and go_variants ~parent_node (x, variants) =
        List.iter
          (fun ((c, c_params), (binding, graph)) ->
             let attrs =
                 Symbol.(
                   label "%s=%s(%s)" (to_string x) (to_string c) (comma_sep c_params))
             in
             match binding with
             | Some binding ->
               let current_node = Gensym.emit inv_gensym in
               go_extract ~attrs ~parent_node ~current_node (binding, graph)
             | None -> go ~attrs ~parent_node graph)
          variants
    and go_bindings ~parent_node bindings =
        List.iter
          (fun (x, graph) ->
             go ~attrs:(label "%s" (Symbol.to_string x)) ~parent_node graph)
          bindings
    and go_body ~parent_node body =
        go ~attrs:"penwidth=1.7 arrowhead=dot" ~parent_node body
    in
    write_endline ~oc "digraph {";
    write_endline ~oc "%snode [fontname=\"bold helvetica\"];" tab;
    write_endline ~oc "%sedge [fontname=\"bold helvetica\"];" tab;
    go ~parent_node:None graph;
    Out_channel.output_string oc "}"
;;
