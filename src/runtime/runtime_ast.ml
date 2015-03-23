


type signal = string
type label = Label of string

type statement =
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
  | Atom of (unit -> unit)
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

let list_to_seq l =
  let rec step l = match l with
  | [] -> Nothing
  | [e] -> e
  | h :: t -> Seq (h, step t)
  in step l

let (//) a b =  Par (a, b)
let (!!) l = list_to_seq l
let loop_each r p =  Loop_each (p, r)
let loop l =  Loop (!! l)
let atom f =  Atom f
let await a =  Await a
let emit a =  Emit a
let pause = Pause
let trap s st = Trap (Label s, st)
let exit_l s = Exit (Label s)
let abort s p = Abort (p ,s)
