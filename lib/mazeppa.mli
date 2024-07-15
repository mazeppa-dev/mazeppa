module Checked_oint = Checked_oint
module Symbol = Symbol
module Const = Const
module Raw_term = Raw_term
module Raw_program = Raw_program

(** Something went wrong... *)
exception Panic of string

(** Supercompiles an input program to an equivalent output program. May raise {!Panic}
    during the process.

    You can call this function as many times as you want, including in parallel. *)
val supercompile : Raw_program.t -> Raw_program.t

(** Evaluates a given program to a value, as defined by the language semantics. May raise
    {!Panic} during the process.

    The input program must be well-formed as per the language definition. The [main]
    function must be defined with zero parameters. If any of these conditions is violated,
    the result is either {!Panic} or an incorrect value.

    Just as {!supercompile}, this function can be called in parallel. *)
val eval : Raw_program.t -> Raw_term.t

(**/**)

module Internals : sig
  module Parser = Parser
  module Lexer = Lexer
  module Util = Util
  module Symbol_map = Symbol_map
  module Subst = Subst
  module Gensym = Gensym
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
