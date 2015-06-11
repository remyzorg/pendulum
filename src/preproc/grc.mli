
module Selection_tree : sig

  type t

  module Primitive : sig
    val exit : t -> unit
    val enter : t -> unit
    val sync : t -> bool
  end


end

module Flowgraph : sig

  type action =
    | Emit of string
    | Atom of Parsetree.expression
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

val flowgraph : Ast.Tagged.t -> Flowgraph.t
val of_ast : Ast.Tagged.t -> Selection_tree.t * Flowgraph.t
