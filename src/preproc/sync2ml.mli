


type error =
  | Cyclic_causality of Grc.Flowgraph.t
  | Par_leads_to_finish of Grc.Flowgraph.t

exception Error of Location.t * error
val print_error : Format.formatter -> error -> unit

val find_and_replace :
  (Grc.Flowgraph.t -> Grc.Flowgraph.t) ->
  Grc.Flowgraph.t -> Grc.Flowgraph.t -> bool * Grc.Flowgraph.t

val replace_join : Grc.Flowgraph.t -> Grc.Flowgraph.t -> (Grc.Flowgraph.t -> Grc.Flowgraph.t)
  -> Grc.Flowgraph.t * Grc.Flowgraph.t

val children: Grc.Flowgraph.t -> Grc.Flowgraph.t -> Grc.Flowgraph.t -> Grc.Flowgraph.t

val interleave: Grc.Flowgraph.t -> Grc.Flowgraph.t

val generate: Ast.Tagged.t -> unit
