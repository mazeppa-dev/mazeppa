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

   The whistle is only tested on terms with different redex operators. For the discussion,
   see <https://github.com/mazeppa-dev/mazeppa/issues/9>.

   [1] Morten Heine Sørensen. 1998. Convergence of Program Transformers in the Metric
   Space of Trees. In Proceedings of the Mathematics of Program Construction (MPC '98).
   Springer-Verlag, Berlin, Heidelberg, 315–337.

   [2] Romanenko, Sergei. (2018). Supercompilation: homeomorphic embedding, call-by-name,
   partial evaluation. Keldysh Institute Preprints. 1-32. 10.20948/prepr-2018-209. *)

type node = Symbol.t * Term.t

type internal_node =
  { redex_op : [ `Op of Symbol.t | `Undefined ]
  ; contents : node
  }

type t =
  { locals : internal_node list
  ; globals : internal_node list
  }

let empty = { locals = []; globals = [] }

let redex_op =
    let rec go = function
      | Term.(Var _ | Const _) -> failwith "Impossible"
      | Term.Call (op, _args) when Symbol.is_lazy op -> failwith "Impossible"
      | Term.Call (op, args) -> go_args ~op args
    and go_args ~op = function
      | [] -> `Op op
      | t :: rest when Term.is_value t -> go_args ~op rest
      | Term.Call (c, [ _ ]) :: _rest when c = Symbol.of_string "Panic" -> `Undefined
      | t :: _rest -> go t
    in
    go
;;

exception Whistle of node

(* Prepends [suspect] to [history] or raises [Whistle] on homeomorphic embedding. *)
let scan ~suspect:(n_id, n) history =
    let n_redex_op = redex_op n in
    let rec go = function
      | [] -> { redex_op = n_redex_op; contents = n_id, n } :: history
      | { redex_op = m_redex_op; contents = m_id, m } :: _rest
        when m_redex_op = n_redex_op && Homeomorphic_emb.decide (m, n) ->
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
