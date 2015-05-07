
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

  type test_value =
    | Signal of string [@printer fun fmt -> Format.fprintf fmt "%s"]
    | Selection of int
    | Finished

  type t =
    | Call of action * t
    | Test of test_value * t * t (* then * else *)
    | Fork of t * t * t (* left * right * sync *)
    | Sync of (int * int) * t * t
    | Pause
    | Finish

  val print_to_dot : Format.formatter -> t -> unit

end

val flowgraph : Ast.Tagged.t -> Flowgraph.t
val of_ast : Ast.Tagged.t -> Selection_tree.t * Flowgraph.t
