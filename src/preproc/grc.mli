
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
    | Emit of string [@printer fun fmt -> Format.fprintf fmt "%s"]
    | Atom of Parsetree.expression
    | Enter of int
    | Exit of int
    | Finish
    | SetFinish of bool

  type test_value =
    | Signal of string [@printer fun fmt -> Format.fprintf fmt "%s"]
    | Selection of int
    | Finished

  type node =
    | Call of action
    | Test of test_value
    | Sync of int * int
    | Fork
    | Dep

  type t =
    | Node_bin of node * t * t
    | Node of node * t
    | Leaf of node

  val print_to_dot : Format.formatter -> t -> unit

end

val flowgraph : Ast.Tagged.t -> Flowgraph.t
val of_ast : Ast.Tagged.t -> Selection_tree.t * Flowgraph.t
