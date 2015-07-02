
type 'a location = {
  loc : Location.t [@printer Location.print_loc] ;
  content : 'a;
}[@@deriving show]

type ident = string location [@@deriving show]

type signal = ident [@@deriving show]
type label = Label of ident [@@deriving show]

module IntMap : Map.S with type key = int
module StringMap : Map.S with type key = string

val dummy_loc : Location.t
val mk_loc : ?loc:Location.t -> 'a -> 'a location

module Derived : sig
  type statement = statement_tree location
  and statement_tree =
  | Loop of statement
  | Seq of statement * statement
  | Par of statement * statement
  | Emit of signal
  | Nothing
  | Pause
  | Suspend of statement * signal
  | Trap of label * statement
  | Exit of label
  | Present of signal * statement * statement
  | Atom of Parsetree.expression [@printer Printast.expression 0]
  | Signal of signal * statement

  | Halt
  | Sustain of signal
  | Present_then of signal * statement
  | Await of signal
  | Await_imm of signal
  | Suspend_imm of statement * signal
  | Abort of statement * signal
  | Weak_abort of statement * signal
  | Loop_each of statement * signal
  | Every of signal * statement
      [@@deriving show]
end

type error =
  | Unbound_identifier of string
  | Syntax
exception Error of Location.t * error
val error : loc:Location.t -> error -> 'a
val print_error : Format.formatter -> error -> unit
val syntax_error : loc:Location.t -> string -> 'a

module Tagged : sig
  type t = {id : int; st : tagged}
  [@@deriving show]

  and tagged_ast =
    | Loop of t
    | Seq of t * t
    | Par of t * t
    | Emit of signal
    | Nothing
    | Pause
    | Suspend of t * signal
    | Trap of label * t
    | Exit of label
    | Present of signal * t * t
    | Atom of Parsetree.expression [@printer Printast.expression 0]
    | Signal of signal * t
    | Await of signal
  [@@deriving show]
  and tagged = tagged_ast location
[@@deriving show]

  type env = {
    labels : int StringMap.t;
    signals : int StringMap.t;
  }

  val of_ast : ?sigs:signal list -> Derived.statement -> t

  val print_to_dot : Format.formatter -> t -> unit
end

module Analysis : sig
  val blocking : Tagged.t -> bool
end
