type signal_state = Absent | Present

type ('a, 'b) signal = {
  mutable value : 'a;
  mutable state : signal_state;
  mutable pre : 'a;
  set : 'a -> 'b -> 'a;
  default : 'a;
}


let set_absent s = s.state <- Absent; s.pre <- s.value
let set_present s = s.state <- Present
let set_present_local s = s.state <- Present

let setval s v = s.value <- s.set s.value v

let set_present_value s v =
  s.state <- Present;
  setval s v;
  s.pre <- s.value

let set_default acc x = x

let make_signal value = {
  value; state = Absent; pre = value;
  set = (fun _ x -> x);
  default = value;
}

let make_event_signal value = {
  value = None; state = Absent; pre = None;
  set = (fun _ x -> Some x);
  default  = value;
}

let make_signal_gather (value, set) = { value; state = Absent; pre = value; set; default = value}

let pre s = s.pre
let value s = s.value
let (!!) s = s.pre
let (!?) s = s.state = Present
