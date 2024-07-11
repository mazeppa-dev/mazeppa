module Make (_ : sig
    val program : Program.t

    (* This is only used to preserve terms in exception messages. *)
    val inspect : bool

    val observe_node : Symbol.t * Term.t -> unit

    val unobserve_node : Symbol.t -> unit
  end) : sig
  val run : Term.t -> Process_graph.t
end
