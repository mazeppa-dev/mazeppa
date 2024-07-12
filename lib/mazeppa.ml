module Checked_oint = Checked_oint
module Symbol = Symbol
module Raw_term = Raw_term
module Raw_program = Raw_program

exception Panic of string

let supercompile_exn input =
    let program = Converter.to_program input in
    let module Supervisor =
      Supervisor.Make (struct
        let program = program

        let inspect = false

        let observe_node (_id, _node) = ()

        let unobserve_node _id = ()
      end)
    in
    let main_symbol = Symbol.of_string "main" in
    let main_params, _ = Program.find_f_rule ~program main_symbol in
    let t = Term.(Call (main_symbol, var_list main_params)) in
    let graph = Supervisor.run t in
    let t_res, program_res = Residualizer.run graph in
    ([], main_symbol, main_params, t_res) :: program_res
;;

let supercompile (input : Raw_program.t) : Raw_program.t =
    try supercompile_exn input with
    | Util.Panic { msg; reduction_path } ->
      (* Inspection is disabled. *)
      assert (List.is_empty reduction_path);
      raise (Panic msg)
;;

module Internals = struct
  module Parser = Parser
  module Lexer = Lexer
  module Util = Util
  module Symbol_map = Symbol_map
  module Subst = Subst
  module Gensym = Gensym
  module Const = Const
  module Term = Term
  module Program = Program
  module Homeomorphic_emb = Homeomorphic_emb
  module Msg = Msg
  module Converter = Converter
  module Supervisor = Supervisor
  module Visualizer = Visualizer
  module Residualizer = Residualizer
  module Pretty = Pretty
end
