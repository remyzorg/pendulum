
module Selection_tree : sig


  type t = {label : int; t : repr}
      [@@deriving show]
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
    | Emit of string
    | Atom of Ast.atom
    | Enter of int
    | Exit of int

  type test_value =
    | Signal of string
    | Selection of int
    | Finished

  type t =
    | Call of action * t
    | Test of test_value * t * t (* then * else *)
    | Fork of t * t * t (* left * right * sync *)
    | Sync of (int * int) * t * t
    | Pause
    | Finish
    [@@deriving show]

  type flowgraph = t

  module Fgtbl : Hashtbl.S with type key = flowgraph

  val print_to_dot : Format.formatter -> t -> unit
  val pp : Format.formatter -> t -> unit

end

module Of_ast : sig
  val flowgraph : Ast.Tagged.t -> Flowgraph.t
  val construct : Ast.Tagged.t -> Selection_tree.t * Flowgraph.t
end

module Schedule : sig

  val check_causality_cycles : 'a * Flowgraph.t -> Flowgraph.t list Utils.StringMap.t

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
