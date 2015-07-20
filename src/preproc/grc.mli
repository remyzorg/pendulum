
module Selection_tree : sig


  type t = {label : int; t : repr; mutable tested : bool}
  and repr =
    | Bottom
    | Pause
    | Par of t list
    | Excl of t list
    | Ref of t

  val print_to_dot : Format.formatter -> t -> unit

end

module Flowgraph : sig

  type action =
    | Emit of Parsetree.expression Ast.valued_signal
    | Atom of Ast.atom
    | Enter of int
    | Exit of int
    | Local_signal of Parsetree.expression Ast.valued_signal

  type test_value =
    | Signal of Ast.signal
    | Selection of int
    | Finished

  type t =
    | Call of action * t
    | Test of test_value * t * t (* then * else *)
    | Fork of t * t * t (* left * right * sync *)
    | Sync of (int * int) * t * t
    | Pause
    | Finish

  type flowgraph = t

  module Fgtbl : Hashtbl.S with type key = flowgraph

  val print_to_dot : Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit

end

type error =
  | Unbound_label of string
  | Cyclic_causality of Flowgraph.t
  | Par_leads_to_finish of Flowgraph.t

exception Error of Location.t * error

val print_error : Format.formatter -> error -> unit


module Of_ast : sig
  val flowgraph : Ast.Tagged.t -> Flowgraph.t
  val construct : Ast.Tagged.t -> Selection_tree.t * Flowgraph.t
end

module Schedule : sig

  val check_causality_cycles : 'a * Flowgraph.t -> Flowgraph.t list Ast.SignalMap.t

  val tag_tested_stmts : Selection_tree.t -> Flowgraph.t -> unit

  val find : Flowgraph.t -> Flowgraph.t -> Flowgraph.t option

  val find_and_replace :
    (Flowgraph.t -> Flowgraph.t) ->
    Flowgraph.t -> Flowgraph.t -> bool * Flowgraph.t

  val find_join : Flowgraph.t -> Flowgraph.t -> Flowgraph.t option

  val replace_join : Flowgraph.t -> Flowgraph.t -> (Flowgraph.t -> Flowgraph.t)
    -> Flowgraph.t * Flowgraph.t

  val children: Flowgraph.t -> Flowgraph.t -> Flowgraph.t -> Flowgraph.t

  val interleave: Flowgraph.t -> Flowgraph.t

end
