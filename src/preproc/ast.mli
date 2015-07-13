
type 'a location = {
  loc : Location.t;
  content : 'a;
}

type ident = string location

type signal = ident
type label = Label of ident

type 'a valued_signal = {ident : ident; value : 'a}
val mk_vsig : signal -> 'a -> 'a valued_signal

type atom = { locals : signal list; exp : Parsetree.expression}
val mk_atom : ?locals:signal list -> Parsetree.expression -> atom

module IntMap : Map.S with type key = int
module StringMap : Map.S with type key = string
module IdentMap : Map.S with type key = ident
module IdentSet : Set.S with type elt = ident

val dummy_loc : Location.t
val mk_loc : ?loc:Location.t -> 'a -> 'a location

module Derived : sig
  type statement = statement_tree location
  and statement_tree =
  | Loop of statement
  | Seq of statement * statement
  | Par of statement * statement
  | Emit of Parsetree.expression valued_signal
  | Nothing
  | Pause
  | Suspend of statement * signal
  | Trap of label * statement
  | Exit of label
  | Present of signal * statement * statement
  | Atom of Parsetree.expression
  | Signal of Parsetree.expression valued_signal * statement

  | Halt
  | Sustain of Parsetree.expression valued_signal
  | Present_then of signal * statement
  | Await of signal
  | Await_imm of signal
  | Suspend_imm of statement * signal
  | Abort of statement * signal
  | Weak_abort of statement * signal
  | Loop_each of statement * signal
  | Every of signal * statement
end

type error =
  | Unbound_identifier of string
exception Error of Location.t * error
val error : loc:Location.t -> error -> 'a
val print_error : Format.formatter -> error -> unit

module Tagged : sig


  type t = {id : int; st : tagged}

  and tagged_ast =
    | Loop of t
    | Seq of t * t
    | Par of t * t
    | Emit of Parsetree.expression valued_signal
    | Nothing
    | Pause
    | Suspend of t * signal
    | Trap of label * t
    | Exit of label
    | Present of signal * t * t
    | Atom of atom
    | Signal of Parsetree.expression valued_signal * t
    | Await of signal
  and tagged = tagged_ast location

  type env = {
    labels : int IdentMap.t;
    signals : int IdentMap.t;
    mutable all_local_signals : Parsetree.expression valued_signal list;
    local_signals : Parsetree.expression valued_signal list;
  }

  val of_ast : ?sigs:(signal list) -> Derived.statement -> t * env

  val print_to_dot : Format.formatter -> t -> unit
end

module Analysis : sig
  val blocking : Tagged.t -> bool
end
