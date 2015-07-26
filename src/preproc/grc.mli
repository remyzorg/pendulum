
module Selection_tree : sig

  module type S = sig

    module Ast : Ast.S

    type t = {label : int; t : repr; mutable tested : bool}
    and repr =
      | Bottom
      | Pause
      | Par of t list
      | Excl of t list
      | Ref of t

    val print_to_dot : Format.formatter -> t -> unit
    val of_ast : Ast.Tagged.t -> t

  end

  module Make (Ast : Ast.S) : S with module Ast = Ast

end

module Flowgraph : sig

  module type S = sig

    module Ast : Ast.S

    type action =
      | Emit of Ast.valued_signal
      | Atom of Ast.atom
      | Enter of int
      | Exit of int
      | Local_signal of Ast.valued_signal

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
    val exit_node : Ast.Tagged.t -> t -> t
    val enter_node : Ast.Tagged.t -> t -> t


    type error =
      | Unbound_label of string
      | Cyclic_causality of t
      | Par_leads_to_finish of t

    val error : loc:Ast.loc -> error -> 'a

    exception Error of Ast.loc * error
    val print_error : Format.formatter -> error -> unit
  end

  module Make (Ast : Ast.S) : S with module Ast = Ast

end


module Of_ast : sig

  module type S = sig

    module Ast : Ast.S
    module Fg : Flowgraph.S
    module St : Selection_tree.S

    val flowgraph : Ast.Tagged.t -> Fg.t
    val construct : Ast.Tagged.t -> St.t * Fg.t
  end

  module Make (Fg : Flowgraph.S) (St : Selection_tree.S with module Ast = Fg.Ast) : S
    with module Ast = Fg.Ast
     and module Fg = Fg
     and module St = St

end

module Schedule : sig

  module type S = sig

    module Ast : Ast.S
    module Fg : Flowgraph.S
    module St : Selection_tree.S

    val check_causality_cycles : 'a * Fg.t -> Fg.t list Ast.SignalMap.t
    val tag_tested_stmts : St.t -> Fg.t -> unit
    val find : Fg.t -> Fg.t -> Fg.t option
    val find_and_replace :
      (Fg.t -> Fg.t) ->
      Fg.t -> Fg.t -> bool * Fg.t

    val find_join : Fg.t -> Fg.t -> Fg.t option
    val replace_join : Fg.t -> Fg.t -> (Fg.t -> Fg.t)
      -> Fg.t * Fg.t
    val children: Fg.t -> Fg.t -> Fg.t -> Fg.t
    val interleave: Fg.t -> Fg.t
  end

  module Make (Fg : Flowgraph.S) (St : Selection_tree.S with module Ast = Fg.Ast) : S
    with module Ast = Fg.Ast
     and module Fg = Fg
     and module St = St

end
