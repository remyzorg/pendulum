


module Bitset = struct

  type t = int array

  let max_value = Sys.word_size - 2

  let make n = Array.make (n / max_value + 1) 0

  let add t e =
    let id = e / max_value in
    t.(id) <- (1 lsl (e mod max_value) lor t.(id))

  let mem t e =
    t.(e / max_value) land (1 lsl (e mod max_value)) != 0

  let remove t e =
    let id = e / max_value in
    t.(id) <- (1 lsl (e mod max_value) lxor t.(id))

end
