
type selection_tree = Runtime_misc.Bitset.t

type state = Pause | Finish

exception Pause_exc
exception Finish_exc
