
type 'a location = {
  loc : Location.t;
  content : 'a;
}

type ident = string location

type signal_origin = Local | Input | Output

type signal = { ident : ident; origin : signal_origin }
type label = Label of ident
type 'a atom = { locals : signal list; exp : 'a}

type 'a valued_signal = {signal : signal ; value : 'a atom}
type 'a valued_ident = {sname : ident ; value : 'a}
val mk_signal : ?origin:signal_origin -> ident -> signal

val mk_vsig : signal -> signal list -> 'a -> 'a valued_signal
val mk_vid : ident -> 'a -> 'a valued_ident

val mk_atom : ?locals:signal list -> 'a -> 'a atom

module IdentMap : Map.S with type key = ident
module IdentSet : Set.S with type elt = ident
module SignalMap : Map.S with type key = signal
module SignalSet : Set.S with type elt = signal

val dummy_loc : Location.t
val mk_loc : ?loc:Location.t -> 'a -> 'a location

module Derived : sig
  type 'a statement = ('a statement_tree) location
  and 'a statement_tree =
  | Loop of 'a statement
  | Seq of 'a statement * 'a statement
  | Par of 'a statement * 'a statement
  | Emit of 'a valued_ident
  | Nothing
  | Pause
  | Suspend of 'a statement * ident
  | Trap of label * 'a statement
  | Exit of label
  | Present of ident * 'a statement * 'a statement
  | Atom of 'a
  | Signal of 'a valued_ident * 'a statement

  | Halt
  | Sustain of 'a valued_ident
  | Present_then of ident * 'a statement
  | Await of ident
  | Await_imm of ident
  | Suspend_imm of 'a statement * ident
  | Abort of 'a statement * ident
  | Weak_abort of 'a statement * ident
  | Loop_each of 'a statement * ident
  | Every of ident * 'a statement
end

type error =
  | Unbound_identifier of string
exception Error of Location.t * error
val error : loc:Location.t -> error -> 'a
val print_error : Format.formatter -> error -> unit

module Tagged : sig


  type 'a t = {id : int; st : 'a tagged}
  and 'a tagged_ast =
    | Loop of 'a t
    | Seq of 'a t * 'a t
    | Par of 'a t * 'a t
    | Emit of 'a valued_signal
    | Nothing
    | Pause
    | Suspend of 'a t * signal
    | Trap of label * 'a t
    | Exit of label
    | Present of signal * 'a t * 'a t
    | Atom of 'a atom
    | Signal of 'a valued_signal * 'a t
    | Await of signal
  and 'a tagged = ('a tagged_ast) location


  type 'a env = {
    labels : int IdentMap.t;
    global_namespace : int IdentMap.t ref;
    signals : (int * signal_origin) SignalMap.t;
    all_local_signals : ('a valued_signal) list ref;
    local_signals : 'a valued_signal list;
  }

  val of_ast : ?sigs:(signal list) -> 'a Derived.statement -> 'a t * 'a env

  val print_to_dot : Format.formatter -> 'a t -> unit
end

module Analysis : sig
  val blocking : 'a Tagged.t -> bool
end
