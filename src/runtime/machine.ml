


type selection_tree = Runtime_misc.Bitset.t

type machine_state = Pause | Finish
type signal_state = Absent | Present

type 'a t = {
  instantiate : 'a -> ('a -> machine_state);
}

type 'a signal = {
  mutable value : 'a option;
  mutable state : bool;
}
