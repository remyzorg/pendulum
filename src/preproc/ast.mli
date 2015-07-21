
type 'a location = {
  loc : Location.t;
  content : 'a;
}

type ident = string location

type signal_origin = Local | Input | Output

type signal = { ident : ident; origin : signal_origin }
type label = Label of ident

type 'a valued_ident = {sname : ident ; value : 'a}
type 'a valued_signal = {signal : signal; value : 'a}
val mk_signal : ?origin:signal_origin -> ident -> signal
val mk_vsig : signal -> 'a -> 'a valued_signal
val mk_vid : ident -> 'a -> 'a valued_ident

type atom = { locals : signal list; exp : Parsetree.expression}
val mk_atom : ?locals:signal list -> Parsetree.expression -> atom

module IdentMap : Map.S with type key = ident
module IdentSet : Set.S with type elt = ident
module SignalMap : Map.S with type key = signal
module SignalSet : Set.S with type elt = signal

val dummy_loc : Location.t
val mk_loc : ?loc:Location.t -> 'a -> 'a location

module Derived : sig
  type statement = statement_tree location
  and statement_tree =
  | Loop of statement
  | Seq of statement * statement
  | Par of statement * statement
  | Emit of Parsetree.expression valued_ident
  | Nothing
  | Pause
  | Suspend of statement * ident
  | Trap of label * statement
  | Exit of label
  | Present of ident * statement * statement
  | Atom of Parsetree.expression
  | Signal of Parsetree.expression valued_ident * statement

  | Halt
  | Sustain of Parsetree.expression valued_ident
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
    global_namespace : int SignalMap.t ref;
    signals : (int * signal_origin) SignalMap.t;
    all_local_signals : Parsetree.expression valued_signal list ref;
    local_signals : Parsetree.expression valued_signal list;
  }

  val of_ast : ?sigs:(signal list) -> Derived.statement -> t * env

  val print_to_dot : Format.formatter -> t -> unit
end

module Analysis : sig
  val blocking : Tagged.t -> bool
end
