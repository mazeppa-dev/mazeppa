open Mazeppa
open Mazeppa.Internals
open Ppx_yojson_conv_lib.Yojson_conv.Primitives

let github_repo = "https://github.com/mazeppa-dev/mazeppa"

let version = "0.1.0"

let tab = "    "

let main_symbol = Symbol.of_string "main"

let position_to_string ({ pos_fname; pos_lnum; pos_bol; pos_cnum } : Lexing.position) =
    Spectrum.Simple.sprintf
      "@{<bold>%s:%d:%d@}"
      pos_fname
      pos_lnum
      (pos_cnum - pos_bol + 1)
;;

let read_file_exn filename =
    In_channel.with_open_text filename (fun ic ->
      let lexbuf = Lexing.from_channel ic in
      Lexing.set_filename lexbuf filename;
      try Parser.program Lexer.read lexbuf with
      | Lexer.SyntaxError msg ->
        Util.panic "%s: %s" (position_to_string lexbuf.lex_curr_p) msg
      | Parser.Error ->
        Util.panic "%s: syntax error" (position_to_string lexbuf.lex_curr_p))
;;

module Make_nodes (S : sig end) : sig
  val memoize : Symbol.t * Term.t -> unit

  val unmemoize : Symbol.t -> unit

  val output : oc:out_channel Lazy.t -> unit -> unit
end = struct
  let history = Hashtbl.create 1024

  let memoize (id, node) =
      let label, contents = Symbol.to_string id, Term.to_string node in
      Hashtbl.add history label contents
  ;;

  let unmemoize id = Hashtbl.remove history (Symbol.to_string id)

  let output ~oc () =
      let module Printable = struct
        type t = (string * string) list [@@deriving yojson_of]
      end
      in
      List.of_seq (Hashtbl.to_seq history)
      |> List.sort (fun (l1, _) (l2, _) ->
        let i1, i2 = Scanf.(sscanf l1 "n%d" Fun.id, sscanf l2 "n%d" Fun.id) in
        Int.compare i1 i2)
      |> Printable.yojson_of_t
      |> Yojson.Safe.pretty_to_channel (Lazy.force oc)
  ;;
end

type channels =
  { (* For printing the core program representation. *)
    program_oc : out_channel Lazy.t option
  ; (* For printing the overall graph structure (without term contents). *)
    graph_oc : out_channel Lazy.t option
  ; (* For printing the term contents of corresponding graph nodes. *)
    nodes_oc : out_channel Lazy.t option
  ; (* For printing the residual program. *)
    output_oc : out_channel Lazy.t
  }

let supercompile ~(channels : channels) (input : Raw_program.t) : unit =
    let program = Converter.to_program input in
    (match channels.program_oc with
     | Some oc -> Program.output ~oc program
     | None -> ());
    let module Nodes = Make_nodes (struct end) in
    let module Supervisor =
      Supervisor.Make (struct
        let program = program

        let inspect = Option.is_some channels.program_oc

        let observe_node =
            match channels.nodes_oc with
            | Some _oc -> Nodes.memoize
            | None -> ignore
        ;;

        let unobserve_node = Nodes.unmemoize
      end)
    in
    let main_params, _ = Program.find_f_rule ~program main_symbol in
    let t = Term.(Call (main_symbol, var_list main_params)) in
    let graph = Supervisor.run t in
    (match channels.graph_oc with
     | Some oc -> Visualizer.run ~oc graph
     | None -> ());
    (match channels.nodes_oc with
     | Some oc -> Nodes.output ~oc ()
     | None -> ());
    let t_res, program_res = Residualizer.run graph in
    let residue = ([], main_symbol, main_params, t_res) :: program_res in
    Pretty.print_program ~oc:channels.output_oc residue
;;

type config =
  { target_dir : string
  ; inspect : bool
  }

let prepare_target_dir (conf : config) : unit =
    if Sys.file_exists conf.target_dir
    then
      if not (Sys.is_directory conf.target_dir)
      then Util.panic "`%s` is not a directory" conf.target_dir
      else ()
    else Sys.mkdir conf.target_dir 0o775
;;

let ( let$ ) filename k =
    let oc = lazy (Out_channel.open_text filename) in
    Fun.protect
      ~finally:(fun () ->
        if Lazy.is_val oc
        then (
          Out_channel.output_string (Lazy.force oc) "\n";
          Stdlib.close_out_noerr (Lazy.force oc)))
      (fun () -> k oc)
;;

let supercompile ~(conf : config) (program_raw : Raw_program.t) : unit =
    let$ output_oc = conf.target_dir ^ "/output.mz" in
    if conf.inspect
    then
      let$ program_oc = conf.target_dir ^ "/program.json" in
      let$ graph_oc = conf.target_dir ^ "/graph.dot" in
      let$ nodes_oc = conf.target_dir ^ "/nodes.json" in
      supercompile
        ~channels:
          { program_oc = Some program_oc
          ; graph_oc = Some graph_oc
          ; nodes_oc = Some nodes_oc
          ; output_oc
          }
        program_raw
    else
      supercompile
        ~channels:{ program_oc = None; graph_oc = None; nodes_oc = None; output_oc }
        program_raw
;;

let parse_cli () =
    [ (Clap.case "run" ~description:"Run the supercompiler."
       @@ fun () ->
       let target_dir =
           Clap.default_string
             ~long:"target-dir"
             ~description:"The target directory to use."
             "target"
       in
       let inspect =
           Clap.flag
             ~set_long:"inspect"
             ~description:
               "Inspect the work of the supercompiler. The resulting files will be in \
                the target directory."
             false
       in
       { target_dir; inspect })
    ]
    |> Clap.subcommand
;;

let its_over ?(reduction_path = []) msg =
    Spectrum.Simple.eprintf "@{<bold,red>error:@} %s\n" msg;
    if not (List.is_empty reduction_path)
    then
      Spectrum.Simple.eprintf
        "%s@{<bold,aqua>note:@} While reducing %s\n"
        tab
        (String.concat " -> " reduction_path);
    exit 1
;;

let () =
    Printexc.record_backtrace true;
    Clap.description (Printf.sprintf "The Mazeppa supercompiler (v%s)." version);
    let conf = parse_cli () in
    Clap.close ();
    try
      let program_raw = read_file_exn "main.mz" in
      prepare_target_dir conf;
      supercompile ~conf program_raw
    with
    | Util.Panic { msg; reduction_path } -> its_over ~reduction_path msg
    | Sys_error msg -> its_over msg
    | e ->
      Spectrum.Simple.eprintf
        "@{<bold,red>internal compiler error:@} %s\n\n\
         @{<bold>Please, file an issue in <%s/issues>. Include the compiler's version \
         and a Minimal Reproducible Example (MRE).@}\n\n"
        (Printexc.to_string e)
        github_repo;
      Printexc.print_backtrace stderr;
      exit 1
;;
