

(* open Ast *)


(* let p = *)
(*   Loop ( *)
(*     Atom (fun () -> Format.printf "Hello") *)
(*   ) *)

(* let rec instant : Ast.statement -> Ast.statement -> Ast.statement = *)
(*   fun p k -> assert false *)

  (* match p with *)
  (* | Nothing | Seq [] -> End *)

  (* | Atom f -> f (); End *)

  (* | Seq (h :: t) -> *)
  (*   let k = instant h End in *)
  (*   instant (Seq t) k *)

  (* | Loop p -> *)
  (*   let k = instant p Loop in *)
  (*     instant (Loop p) k *)
  (* | Pause -> k *)





let sum s =
  let rec sum' s k =
    match s with
      [] -> k 0
    | x::xs -> sum' xs (fun a -> k (x + a)) in
  sum' s (fun x -> x)
