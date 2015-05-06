



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

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)



module Queue = struct
  type 'a t = 'a list * 'a list
  let empty = ([], [])

  let enqueue (f, b) e = (f, e :: b)
  let rec dequeue q =
    match q with
    | [], [] -> None
    | h :: t, b -> Some (h, (t, b))
    | [], b -> dequeue (List.rev b, [])
end

module Option = struct

  let map f v =
    match v with None -> None | Some v -> Some (f v)

end
