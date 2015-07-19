


type selection_tree = Runtime_misc.Bitset.t

type machine_state = Pause | Finish
type signal_state = Absent | Present

type 'a t = {
  instantiate : 'a -> ('a * (unit -> machine_state));
}

type 'a signal = {
  mutable value : 'a;
  mutable state : signal_state;
}


let set_absent s = s.state <- Absent
let set_present s = s.state <- Present
let set_present_value s v = s.state <- Present; s.value <- v
let setval s v = s.value <- v

let make_signal value =
  { value; state = Absent }

let (!!) s = s.value
let (!?) s = s.state = Present
