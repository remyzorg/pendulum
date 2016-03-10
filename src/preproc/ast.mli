
(** Ast module

    This is functor interface to create an Esterel-like asbtract syntax tree
    from Location module type and an Exp (expression) module type. Thus, the Ast functions
    are independant from the general purpose language you want to compile to.

    There is a raw version of the Ast, which is more like a Parsetree, called Derived, and
    a more semantic version, Tagged.

    In Tagged, the non-core statements of Esterel are remove, and all the statement are
    indexed uniquely. During the transformation, Derived->Tagged, the system a bit of semantic
    (like unbound signals, labels, and so on).

*)


module type Location = sig
  type t
  val none : t
end

module type Exp = sig
  type t
  type core_type
  val print : Format.formatter -> t -> unit
  module Location : Location
end

module type S = sig

  type loc
  type exp
  type core_type

  val printexp : Format.formatter -> exp -> unit

  type 'a location = {
    loc : loc;
    content : 'a;
  }

  type ident = string location

  type signal_origin = Local | Input | Output

  type gatherer = exp option

  type signal_binder =
    | Access of ident * ident list
    | Event of ident * gatherer
    | No_binding

  type signal = { ident : ident; origin : signal_origin; bind : signal_binder; gatherer : gatherer}
  type label = Label of ident
  type atom = { locals : signal list; exp : exp}

  type test = ident * ident option * exp option

  type valued_signal = {signal : signal ; svalue : atom}
  type valued_ident = {sname : ident ; fields : ident list; ivalue : exp}
  val mk_signal : ?origin:signal_origin -> ?bind:signal_binder -> ?gatherer:exp -> ident -> signal

  val mk_vsig : signal -> signal list -> exp -> valued_signal
  val mk_vid : ?fields:ident list -> ident -> exp -> valued_ident

  val mk_atom : ?locals:signal list -> exp -> atom

  module IdentMap : Map.S with type key = ident
  module IdentSet : Set.S with type elt = ident
  module SignalMap : Map.S with type key = signal
  module SignalSet : Set.S with type elt = signal


  val dummy_loc : loc
  val mk_loc : ?loc:loc -> 'a -> 'a location

  module Derived : sig
    type statement = (statement_tree) location
    and statement_tree =
      | Loop of statement
      | Seq of statement * statement
      | Par of statement * statement
      | Emit of valued_ident
      | Nothing
      | Pause
      | Suspend of statement * test
      | Trap of label * statement
      | Exit of label
      | Present of test * statement * statement
      | Atom of exp
      | Signal of valued_ident * statement
      | Run of ident * ident list * loc

      | Halt
      | Sustain of valued_ident
      | Present_then of test * statement
      | Await of test
      | Await_imm of test
      | Suspend_imm of statement * test
      | Abort of statement * test
      | Weak_abort of statement * test
      | Loop_each of statement * test
      | Every of test * statement
  end

  type error =
    | Unbound_identifier of string
  exception Error of loc * error
  val error : loc:loc -> error -> 'a
  val print_error : Format.formatter -> error -> unit

  module Tagged : sig

    type t = {id : int; st : tagged}
    and test = signal * atom option
    and tagged_ast =
      | Loop of t
      | Seq of t * t
      | Par of t * t
      | Emit of valued_signal
      | Nothing
      | Pause
      | Suspend of t * test
      | Trap of label * t
      | Exit of label
      | Present of test * t * t
      | Atom of atom
      | Signal of valued_signal * t
      | Await of test
      | Run of ident * signal list * loc
    and tagged = (tagged_ast) location


    type env = {
      args_signals : (signal * core_type option) list;
      labels : int IdentMap.t;
      global_occurences : int IdentMap.t ref;
      scope : (int * signal_origin * signal_binder * gatherer) SignalMap.t;
      binders_env : (string, signal_binder list) Hashtbl.t;
      local_only_env : (valued_signal) list ref;
      local_only_scope : valued_signal list;
      machine_runs : (int * (int * signal list) list) IdentMap.t ref;
    }

    val print_env : Format.formatter -> env -> unit

    val of_ast :
      ?sigs:((signal * core_type option) list) ->
      ?binders:((string * signal_binder list) list) ->
      Derived.statement -> t * env

    val print_to_dot : Format.formatter -> t -> unit
  end

  module Analysis : sig
    val blocking : Tagged.t -> bool
    val filter_dead_trees : Tagged.t -> Tagged.t
  end

end

module Make (E : Exp) : S
  with type loc = E.Location.t
   and type exp = E.t
   and type core_type = E.core_type
