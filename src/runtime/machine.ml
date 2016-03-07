


type selection_tree = Runtime_misc.Bitset.t

type machine_state = Pause | Finish
type signal_state = Absent | Present

exception Pause_exc
exception Finish_exc

type setter = Signal_setter : ('a -> 'b -> 'a) -> setter

type ('a, 'b) signal = {
  mutable value : 'a;
  mutable state : signal_state;
  mutable pre : 'a;
  set : 'a -> 'b -> 'a;
}


let set_absent s = s.state <- Absent; s.pre <- s.value
let set_present s = s.state <- Present
let set_present_local s = s.state <- Present

let setval s v = s.value <- s.set s.value v

let set_present_value s v =
  s.state <- Present;
  setval s v;
  s.pre <- s.set s.value v


let set_default acc x = x

let make_signal_gather =
  fun set value -> { value; state = Absent; pre = value; set;}

let make_signal value = {
  value; state = Absent; pre = value;
  set = fun _ x -> x;
}

let pre s = s.pre
let value s = s.value
let (!!) s = s.pre
let (!?) s = s.state = Present
