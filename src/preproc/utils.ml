


let fold_n f acc n =
  let rec aux acc n =
    if n < 0 then acc
    else
      aux (f acc n) (n - 1)
  in aux acc n

module MList = struct

  let rec map_filter p f = function
    | [] -> []
    | h :: t -> if p h then f h :: map_filter p f t
      else map_filter p f t


  let rec fold_left_single f1 f2 acc = function
    | [] -> acc
    | h :: [] -> (f1 acc)
    | h :: t -> fold_left_single f1 f2 (f2 acc) t

  let rec pp_iter ?(sep="") pp_element fmt = function
    | [h] -> Format.fprintf fmt "%a" pp_element h
    | h :: t -> Format.fprintf fmt "%a%s@;%a" pp_element h sep (pp_iter ~sep pp_element) t
    | [] -> ()

end

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)

module StringSet = Set.Make(struct
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

  let mapn v f =
    match v with None -> f () | Some v -> Some v

  let print f fmt v =
    match v with
    | None -> Format.fprintf fmt "None"
    | Some v' -> Format.fprintf fmt "Some (%a)" f v'

  let map2or v1 v2 f =
    match v1, v2 with
    | None, None -> None
    | Some v, None | None, Some v -> Some v
    | Some v1, Some v2 -> Some (f v1 v2)

  let map2and v1 v2 f =
    match v1, v2 with
    | None, None -> None
    | _, None | None, _ -> None
    | Some v1, Some v2 -> Some (f v1 v2)

end
