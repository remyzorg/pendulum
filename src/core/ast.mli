

type signal = string
type label = Label of string

module IntMap : Map.S with type key = int
module StringMap : Map.S with type key = string
module SignalSet : Set.S with type elt = signal

type dstatement =
  | Loop of dstatement
  | Seq of dstatement list
  | Par of dstatement list
  | Emit of signal
  | Nothing
  | Pause
  | Suspend of dstatement * signal
  | Trap of label * dstatement
  | Exit of label
  | Present of signal * dstatement * dstatement
  | Atom of (unit -> unit)
  | Signal of signal * dstatement
  | Halt
  | Sustain of signal
  | Present_then of signal * dstatement
  | Await of signal
  | Await_imm of signal
  | Suspend_imm of dstatement * signal
  | Abort of dstatement * signal
  | Weak_abort of dstatement * signal
  | Loop_each of dstatement * signal
  | Every of signal * dstatement
[@@deriving show]

type statement =
  | Loop of statement
  | Seq of statement list
  | Par of statement list
  | Emit of signal
  | Nothing
  | Pause
  | Suspend of statement * signal
  | Trap of label * statement
  | Exit of label
  | Present of signal * statement * statement
  | Atom of (unit -> unit)
  | Signal of signal * statement
  | Await of signal
[@@deriving show]


type error = Unbound_identifier of string * statement
exception Error of error
val error : error -> exn
val print_error : Format.formatter -> error -> unit




module Tagged : sig
  type tagged =
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
    | Atom of (unit -> unit)
    | Signal of signal * t
    | Await of signal
  and t = {id : int; st : tagged}
[@@deriving show]

  type env = {
    labels : int StringMap.t;
    signals : int StringMap.t;
  }

  val of_ast : ?env:string list -> statement -> t

  val print_to_dot : Format.formatter -> t -> unit
end

module Analysis : sig
  val blocking : Tagged.t -> bool
end

val flatten : statement list -> statement list
val normalize : dstatement -> statement
val (//) : dstatement -> dstatement -> dstatement
val (!!) : dstatement list -> dstatement
val loop_each : signal -> dstatement -> dstatement
val loop: dstatement list -> dstatement
val atom: (unit -> unit) -> dstatement
val await : signal -> dstatement
val trap : string -> dstatement -> dstatement
val emit : signal -> dstatement
val pause : dstatement
val exit_l : string -> dstatement
val abort : signal -> dstatement -> dstatement
