


type selection_tree = Runtime_misc.Bitset.t

type t = {
  react : unit -> unit;
  state : selection_tree
}
