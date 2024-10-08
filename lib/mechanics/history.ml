[@@@coverage off]

(* Our algorithm is the following: 1) compare global nodes only with global nodes, 2)
   compare local nodes only up to the first global ancestor (not including it), and 3) do
   not compare trivial nodes.

   Why should it work? Because if we exclude trivial nodes from any infinite sequence, the
   sequence will remain infinite (this is a property of our supercompilation algorithm).
   Since homeomorphic embedding holds true for _any_ infinite sequence, it is safe to
   ignore trivial nodes altogether. The sets of global and local nodes are disjoint, so we
   can compare them separately. However, local nodes are compared only up the first global
   ancestor: this is because if there is a finite amount of global nodes in any infinite
   sequence, the subsequence of local nodes following after the last global node must be
   infinite, so again homeomorphic embedding holds.

   The possibility of global/local checking was discussed in detail in [1] (section 4.7)
   and [2] (section 2.6). In our optimized history representation, we do not store
   contents of local nodes before the last global node: when a global node is added, all
   previous local nodes are "forgotten".

   The whistle is only tested on terms with equal redex signatures, which are computed
   only once for each history item. This allows us to 1) eschew some over-generalizations
   and 2) reduce the amount of expensive homeomorphic embedding checks. For the
   discussion, see issue #9 and issue #11.

   [1] Morten Heine Sørensen. 1998. Convergence of Program Transformers in the Metric
   Space of Trees. In Proceedings of the Mathematics of Program Construction (MPC '98).
   Springer-Verlag, Berlin, Heidelberg, 315–337.

   [2] Romanenko, Sergei. (2018). Supercompilation: homeomorphic embedding, call-by-name,
   partial evaluation. Keldysh Institute Preprints. 1-32. 10.20948/prepr-2018-209. *)

module Make (_ : sig end) = struct
  module Emb = Homeomorphic_emb.Make (struct end)

  type node = Symbol.t * Term.t

  type internal_node = Term.redex_sig * node

  type t =
    { locals : internal_node list
    ; globals : internal_node list
    }

  let empty = { locals = []; globals = [] }

  exception Whistle of node

  (* Prepends [suspect] to [history] or raises [Whistle] with a parent node. *)
  let scan ~suspect:(n_id, n) history =
      let n_redex_sig = Term.redex_sig n in
      let rec go = function
        | [] -> (n_redex_sig, (n_id, n)) :: history
        | (m_redex_sig, (m_id, m)) :: _rest
          when Term.equal_redex_sig m_redex_sig n_redex_sig && Emb.decide (m, n) ->
          raise_notrace (Whistle (m_id, m))
        | _ :: rest -> go rest
      in
      go history
  ;;

  let memoize ~suspect ({ locals; globals } as history) =
      let _n_id, n = suspect in
      match Term.classify n with
      | Term.Global -> { locals = []; globals = scan ~suspect globals }
      | Term.Local -> { locals = scan ~suspect locals; globals }
      | Term.Trivial -> history
  ;;

  let memoize ~(suspect : node) (history : t) : node option * t =
      try None, memoize ~suspect history with
      | Whistle (m_id, m) -> Some (m_id, m), history
  ;;
end
