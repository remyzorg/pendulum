
module type Exp = sig
  type t
  val print : Format.formatter -> t -> unit
end

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

  module type S = sig
    type exp

    type action =
      | Emit of exp Ast.valued_signal
      | Atom of exp Ast.atom
      | Enter of int
      | Exit of int
      | Local_signal of exp Ast.valued_signal

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
    module FgEmitsTbl : Hashtbl.S with type key = flowgraph * flowgraph * Ast.signal
    module Fgtbl2 : Hashtbl.S with type key = flowgraph * flowgraph
    module Fgtbl3 : Hashtbl.S with type key = flowgraph * flowgraph * flowgraph
    module Fgstbl : Hashtbl.S with type key = flowgraph list

    val print_to_dot : Format.formatter -> t -> unit
    val pp : Format.formatter -> t -> unit
    val test_node : test_value -> t * t -> t

    val (>>) : action -> t -> t
    val exit_node : exp Ast.Tagged.t -> t -> t
    val enter_node : exp Ast.Tagged.t -> t -> t


    type error =
      | Unbound_label of string
      | Cyclic_causality of t
      | Par_leads_to_finish of t

    val error : loc:Location.t -> error -> 'a

    exception Error of Location.t * error
    val print_error : Format.formatter -> error -> unit
  end

  module Make (E : Exp) : S with type exp = E.t

end


module Of_ast : sig

  module type S = sig
    type fg
    type exp
    val flowgraph : exp Ast.Tagged.t -> fg
    val construct : exp Ast.Tagged.t -> Selection_tree.t * fg
  end

  module Make (Fg : Flowgraph.S) : S with type fg = Fg.t and type exp = Fg.exp
end

module Schedule : sig

  module type S = sig

    type fg

    val check_causality_cycles : 'a * fg -> fg list Ast.SignalMap.t
    val tag_tested_stmts : Selection_tree.t -> fg -> unit
    val find : fg -> fg -> fg option
    val find_and_replace :
      (fg -> fg) ->
      fg -> fg -> bool * fg

    val find_join : fg -> fg -> fg option
    val replace_join : fg -> fg -> (fg -> fg)
      -> fg * fg
    val children: fg -> fg -> fg -> fg
    val interleave: fg -> fg
  end

  module Make (Fg : Flowgraph.S) : S with type fg = Fg.t

end
