module Checked_oint = Checked_oint
module Symbol = Symbol
module Raw_term = Raw_term
module Raw_program = Raw_program

(** Something went wrong during supercompilation. *)
exception Panic of string

(** Supercompiles an input program to an equivalent output program. May raise {!Panic}
    during the process.

    You can call this function as many times as you want, including in parallel. *)
val supercompile : Raw_program.t -> Raw_program.t

(**/**)

module Internals : sig
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

(**/**)
