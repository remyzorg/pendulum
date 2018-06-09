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

  let remove_all t = for i = 0 to Array.length t - 1 do t.(i) <- 0 done

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

  let create_elt value =
    let rec e = {previous = e; value; next = e} in e

  let create value =
    { head = Head (create_elt value) }

  let remove a =
    a.previous.next <- a.next;
    a.next.previous <- a.previous

  let remove_elt l a =
    match l.head with
    | Nil -> ()
    | Head h ->
      if a.next == a then l.head <- Nil
      else begin
        if a == h then l.head <- Head a.next;
        remove a
      end

  let append l1 l2 =
    match l1.head, l2.head with
    | _, Nil -> () | Nil, Head _ -> l1.head <- l2.head
    | Head h1, Head h2 ->
      h1.previous.next <- h2;
      h2.previous.next <- h1;
      h1.previous <- h2.previous;
      h2.previous <- h1.previous

  let add_after v elt =
    let elt' = create_elt v in
    elt'.next <- elt.next;
    elt'.previous <- elt;
    elt.next.previous <- elt';
    elt.next <- elt'

  let add_before elt' elt =
    elt'.previous <- elt.previous;
    elt'.previous <- elt;
    elt.previous.next <- elt';
    elt.previous <- elt'

  let add l e =
    let elt = create_elt e in
    match l.head with
    | Nil -> l.head <- Head elt
    | Head h ->
      add_before elt h;
      l.head <- Head elt

  let push l e =
    let elt = create_elt e in
    match l.head with
    | Nil -> l.head <- Head elt
    | Head h -> add_before elt h



  let iter f l =
    match l with
    | Nil -> ()
    | Head h ->
      let rec aux elt =
        f elt.value;
        if elt.next == h then ()
        else aux elt.next
      in aux h



end
