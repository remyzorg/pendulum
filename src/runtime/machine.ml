


type selection_tree = Runtime_misc.Bitset.t

type machine_state = Pause | Finish
type signal_state = Absent | Present

exception Pause_exc
exception Finish_exc

(* type 'a t = { *)
(*   instantiate : 'a -> ('a * (unit -> machine_state)); *)
(* } *)

type 'a signal = {
  mutable value : 'a;
  mutable state : signal_state;
  mutable pre : 'a;
}


let set_absent s = s.state <- Absent; s.pre <- s.value
let set_present s = s.state <- Present
let set_present_local s = s.state <- Present
let set_present_value s v = s.state <- Present; s.value <- v; s.pre <- v
let setval s v = s.value <- v

let make_signal value =
  { value; state = Absent; pre = value}

let pre s = s.pre
let (!!) s = s.value
let (!?) s = s.state = Present
