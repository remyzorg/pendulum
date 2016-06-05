


let fold_n f acc n =
  let rec aux acc n =
    if n < 0 then acc
    else
      aux (f acc n) (n - 1)
  in aux acc n

module Tuple = struct

  let tr_of_db ((v1, v2), v3) = (v1, v2, v3)

end

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

  let rec pp_iter_nobreak ?(sep="") pp_element fmt = function
    | [h] -> Format.fprintf fmt "%a" pp_element h
    | h :: t -> Format.fprintf fmt "%a%s%a" pp_element h sep (pp_iter ~sep pp_element) t
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

module Options = struct
  type t = StringSet.t
  let is_option str s = StringSet.mem str s
  let is_debug s = StringSet.mem "debug" s
end


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

  let default d v = match v with None -> d | Some v -> v

  let print s f fmt v =
    match v with
    | None -> Format.fprintf fmt "%s" s
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

  let casefv opt f1 v2 =
    match opt with
    | Some e -> f1 e
    | None -> v2

end

module Bitset = struct

  type t = int array

  let max_value = 32 - 2

  let max_int = 2147483647 (* there is no 64 bits literal integers in js*)

  let make n b = Array.make (n / max_value + 1) @@ if b then max_int else 0

  let add t e =
    let id = e / max_value in
    t.(id) <- (1 lsl (e mod max_value) lor t.(id))

  let mem t e =
    t.(e / max_value) land (1 lsl (e mod max_value)) != 0

  let remove t e =
    let id = e / max_value in
    t.(id) <- (1 lsl (e mod max_value) lxor t.(id))

  let min_length t1 t2 =
    min (Array.length t1) (Array.length t2)

  let simplify (t1, t2) =
    for i = 0 to min_length t1 t2 - 1 do
      let t1i, t2i = t1.(i), t2.(i) in
      t1.(i) <- t1i land t2i;
      t2.(i) <- t1i lor t2i
    done

  let count_set_bits i =
    let rec loop c n =
      if n = 0 then c
      else loop (succ c) (n land (n - 1))
    in loop 0 i

  type content = Empty | Singleton of int | Set

  let is_empty t =
    try
      Array.iter (fun x -> if x <> 0 then raise Exit)  t; true
    with Exit -> false

  let is_full t =
    try
      Array.iter (fun x -> if x <> max_int then raise Exit)  t; true
    with Exit -> false

  let pp fmt t =
    Format.fprintf fmt "[|";
    Array.iteri (fun i x ->
        Format.fprintf fmt "%d" x;
        if i <> Array.length t - 1 then
          Format.fprintf fmt "; "
      ) t;
    Format.fprintf fmt "|]"
end
