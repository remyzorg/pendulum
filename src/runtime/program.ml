
type selection_tree = Runtime_misc.Bitset.t

type machine_state = Pause | Finish

exception Pause_exc
exception Finish_exc
