
module type Location = sig
  type t
  val none : t
end

module type Exp = sig
  type t
  val print : Format.formatter -> t -> unit
  module Location : Location
end

module type S = sig

  type loc
  type exp

  val printexp : Format.formatter -> exp -> unit

  type 'a location = {
    loc : loc;
    content : 'a;
  }

  type ident = string location

  type signal_origin = Local | Input | Output

  type signal = { ident : ident; origin : signal_origin }
  type label = Label of ident
  type atom = { locals : signal list; exp : exp}

  type valued_signal = {signal : signal ; value : atom}
  type valued_ident = {sname : ident ; value : exp}
  val mk_signal : ?origin:signal_origin -> ident -> signal

  val mk_vsig : signal -> signal list -> exp -> valued_signal
  val mk_vid : ident -> exp -> valued_ident

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
      | Suspend of statement * ident
      | Trap of label * statement
      | Exit of label
      | Present of ident * statement * statement
      | Atom of exp
      | Signal of valued_ident * statement

      | Halt
      | Sustain of valued_ident
      | Present_then of ident * statement
      | Await of ident
      | Await_imm of ident
      | Suspend_imm of statement * ident
      | Abort of statement * ident
      | Weak_abort of statement * ident
      | Loop_each of statement * ident
      | Every of ident * statement
  end

  type error =
    | Unbound_identifier of string
  exception Error of loc * error
  val error : loc:loc -> error -> 'a
  val print_error : Format.formatter -> error -> unit

  module Tagged : sig

    type t = {id : int; st : tagged}
    and tagged_ast =
      | Loop of t
      | Seq of t * t
      | Par of t * t
      | Emit of valued_signal
      | Nothing
      | Pause
      | Suspend of t * signal
      | Trap of label * t
      | Exit of label
      | Present of signal * t * t
      | Atom of atom
      | Signal of valued_signal * t
      | Await of signal
    and tagged = (tagged_ast) location


    type env = {
      labels : int IdentMap.t;
      global_namespace : int IdentMap.t ref;
      signals : (int * signal_origin) SignalMap.t;
      all_local_signals : (valued_signal) list ref;
      local_signals : valued_signal list;
    }

    val of_ast : ?sigs:(signal list) -> Derived.statement -> t * env

    val print_to_dot : Format.formatter -> t -> unit
  end

  module Analysis : sig
    val blocking : Tagged.t -> bool
  end

end

module Make (E : Exp) : S
  with type loc = E.Location.t
   and type exp = E.t
