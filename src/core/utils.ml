



module MList = struct

  let rec map_filter p f = function
    | [] -> []
    | h :: t -> if p h then f h :: map_filter p f t
      else map_filter p f t


  let rec fold_left_single f1 f2 acc = function
    | [] -> acc
    | h :: [] -> (f1 acc)
    | h :: t -> fold_left_single f1 f2 (f2 acc) t

end
