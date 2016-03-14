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

  let min_length t1 t2 =
    min (Array.length t1) (Array.length t2)

  let union t1 t2 =
    for i = 0 to min_length t1 t2 - 1 do
      t1.(i) <- t1.(i) lor t2.(i)
    done

  let inter t1 t2 =
    for i = 0 to min_length t1 t2 - 1 do
      t1.(i) <- t1.(i) land t2.(i)
    done

  let inter_union t t1 t2 =
    inter t t1;
    union t t2;

end



module Linked = struct

  type 'a elt = {
    mutable previous : 'a elt;
    value : 'a;
    mutable next : 'a elt;

  }

  type 'a head = Nil | Head of 'a elt

  type 'a t = {
    mutable head : 'a head;
  }


  let create value =
    let rec e = {previous = e; value; next = e} in e

  let remove_elt l a =
    match l.head with
    | Nil -> ()
    | Head h ->
      if a.next == a then l.head <- Nil
      else begin
        if a == h then l.head <- Head a.next;
        a.previous.next <- a.next;
        a.next.previous <- a.previous
      end

  let append l1 l2 =
    match l1.head, l2.head with
    | _, Nil -> () | Nil, Head _ -> l1.head <- l2.head
    | Head h1, Head h2 ->
      h1.previous.next <- h2;
      h2.previous.next <- h1;
      h1.previous <- h2.previous;
      h2.previous <- h1.previous





end
