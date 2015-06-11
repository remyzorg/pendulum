(* generating the ocaml code from ast *)


open Utils

type error =
  | Cyclic_causality of Grc.Flowgraph.t
  | Par_leads_to_finish of Grc.Flowgraph.t
exception Error of Location.t * error
let error ~loc e = raise (Error (loc, e))

let print_error fmt e =
  let open Format in
  fprintf fmt "%s"
    begin match e with
      | Cyclic_causality fg -> "Cyclic causality"
      | Par_leads_to_finish fg -> Format.printf "%a\n" Grc.Flowgraph.pp fg; "Par leads to pause or exit"
    end


let check_causality_cycles grc =
  let open Grc.Flowgraph in
  let st, fg = grc in
  let rec visit m fg =
    match fg with
    | Test (Signal s, t1, t2) ->
      let prev = try StringMap.find s m with
        | Not_found -> []
      in
      let m = StringMap.add s (fg :: prev) m in
      let m1 = visit m t1 in
      let m2 = visit m t2 in
      StringMap.merge (fun k v1 v2 ->
          match v1, v2 with
          | Some v1, Some v2 -> Some (v1 @ v2)
          | Some v, None | None, Some v -> Some v
          | _ -> None
        ) m1 m2

    | Fork (t1, t2, _) | Sync (_, t1, t2) ->
      let m1 = visit m t1 in
      let m2 = visit m t2 in
      StringMap.merge (fun k v1 v2 ->
          match v1, v2 with
          | Some v1, Some v2 -> Some (v1 @ v2)
          | Some v, None | None, Some v -> Some v
          | _ -> None
      ) m1 m2

    | Call(Emit s, t) ->
      begin match StringMap.find s m with
        | h :: fgs -> error ~loc:Ast.dummy_loc @@ Cyclic_causality h
        | [] -> m
        | exception Not_found -> m
      end
    | _ -> m
  in
  visit StringMap.empty fg

module FgEmitsTbl = Hashtbl.Make(struct
    type t = Grc.Flowgraph.t * Grc.Flowgraph.t * string
    let hash = Hashtbl.hash
    let equal (fg, stop, s) (fg', stop', s') =
      fg == fg' && stop = stop' && s = s'
  end)

module Fgtbl2 = Hashtbl.Make(struct
    type t = Grc.Flowgraph.t * Grc.Flowgraph.t
    let hash = Hashtbl.hash
    let equal (a1, b1) (a2, b2) = (a1 == a2) && (b1 == b2)
  end)

module Fgtbl3 = Hashtbl.Make(struct
    type t = Grc.Flowgraph.t * Grc.Flowgraph.t * Grc.Flowgraph.t
    let hash = Hashtbl.hash
    let equal (a1, b1, c1) (a2, b2, c2) = (a1 == a2) && (b1 == b2) && (c1 == c2)
  end)

let memo_rec h f =
  let rec g x =
    try Fgtbl2.find h x with
    | Not_found ->
      let y = f g x in
      Fgtbl2.add h x y; y
  in g

let emits =
  let open Grc.Flowgraph in
  let emittbl = FgEmitsTbl.create 17 in
  let rec aux fg stop s =
    match fg with
    | Call (Emit s', t) when s = s' ->
      FgEmitsTbl.add emittbl (fg, stop, s) true;
      true
    | fg when fg == stop ->
      FgEmitsTbl.add emittbl (fg, stop, s) false;
      false

    | Call (_, t) ->
      let res = aux t stop s in
      FgEmitsTbl.add emittbl (fg, stop, s) res;
      res
    | Test (_, t1, t2) | Fork (t1, t2, _) | Sync (_ , t1, t2) ->
      let res = aux t1 stop s || aux t2 stop s in
      FgEmitsTbl.add emittbl (fg, stop, s) res;
      res
    | Pause | Finish -> false
  in
  fun fg stop s ->
    try FgEmitsTbl.find emittbl (fg, stop, s) with
    | Not_found -> aux fg stop s

let children =
  let open Grc.Flowgraph in
  let tbl = Fgtbl3.create 17 in
  let children fg t1 t2 =
    try Fgtbl3.find tbl (fg, t1, t2) with Not_found ->
      let newfg = match fg with
        | Sync (ids, _, _) -> Sync (ids, t1, t2)
        | Test (tv, _, _) -> Test (tv, t1, t2)
        | Fork (_, _, sync) -> Fork (t1, t2, sync)
        | Call (a, _) -> Call(a, t1)
        | Pause | Finish -> fg
      in Fgtbl3.add tbl (fg, t1, t2) newfg; newfg
  in children


let rec find_and_replace fg elt replf =
  let tbl = Fgtbl2.create 17 in
  let rec aux fg =
    try Fgtbl2.find tbl (fg, elt) with Not_found ->
      let open Grc.Flowgraph in
      let result =
        if fg == elt then true, replf fg
        else match fg with
          | Call (a, t) ->
            let res, t = aux t in
            res, children fg t t
          | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _)->
            let res1, t1 = aux t1 in
            let res2, t2 = aux t2 in
            res1 || res2, children fg t1 t2
          | Pause | Finish -> false, fg
      in Fgtbl2.add tbl (fg, elt) result; result
  in
  aux fg

let rec replace_join fg1 fg2 replf =
  let open Grc.Flowgraph in
  let res, fg2 = find_and_replace fg2 fg1 replf in
  if res then replf fg1, fg2
  else match fg1 with
    | Call(a, t) ->
      let fg1, fg2 = replace_join t fg2 replf in
      Call (a, fg1), fg2
    | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->
      let t1, _ = replace_join t1 fg2 replf in
      let t2, _ = replace_join t2 fg2 replf in
      children fg1 t1 t2, fg2
    | Pause | Finish -> fg1, fg2


let fork_id = function
  | Grc.Flowgraph.Sync(c, _, _) -> c
  | _ -> 0, 0


let rec interleave fg =
  let open Grc.Flowgraph in
  let fork_tbl = Fgtbl2.create 17 in
  let visit_tbl = Grc.Flowgraph.Fgtbl.create 17 in
  let rec sequence_of_fork (stop: Grc.Flowgraph.t) fg1 fg2 =
    try Fgtbl2.find fork_tbl (fg1, fg2) with | Not_found ->
      try Fgtbl2.find fork_tbl (fg2, fg1) with | Not_found ->
        let fg = match fg1, fg2 with
          | fg1, fg2 when fg1 == fg2 -> fg1
          | fg1, fg2 when
              fork_id fg1 = fork_id fg2 &&
              fork_id fg1 = fork_id stop
            -> fg1
          | fg1, fg2 when
              fork_id fg1 = fork_id stop
            -> sequence_of_fork stop fg2 fg1

          | (Pause | Finish), _ ->
            Format.printf "left is wrong@\n";
            error ~loc:Ast.dummy_loc (Par_leads_to_finish fg2)
          | _, (Finish | Pause) ->
            Format.printf "right is wrong@\n";
            error ~loc:Ast.dummy_loc (Par_leads_to_finish fg1)

          | Test (Signal s, t1, t2), fg2 ->
            if emits fg2 stop s then match fg2 with
              | Call (a, t) -> assert false
                (* Call (a, sequence_of_fork stop t fg1) *)
              | Pause | Finish -> assert false (* TODO: Raise exn *)
              | Sync(_, t1, t2) -> assert false
              | Test (_, t1, t2) -> assert false
                (* let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg1) in *)
                (* children fg2 t1 t2 *)
              | Fork (t1, t2, sync) -> assert false
                (* let fg2 = sequence_of_fork stop t1 t2 in *)
                (* sequence_of_fork sync fg1 fg2 *)
            else
              let t1, t2 = replace_join t1 t2 (fun x -> sequence_of_fork stop x fg2) in
              children fg1 t1 t2

          | Call (action, t), fg2 ->
            Call (action, sequence_of_fork stop fg2 t)

          | (Fork (t1, t2, sync)), (_ as fg2)
          | (_ as fg2), (Fork (t1, t2, sync)) -> assert false
            (* let fg1 = sequence_of_fork sync t1 t2 in *)
            (* sequence_of_fork stop fg1 fg2 *)

          | Sync (_, t1, t2), fg2 -> assert false
            (* Format.printf "====@\n"; *)
            (* let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg2) in *)
            (* children fg1 t1 t2 *)

          | Test (_, t1, t2), fg2 ->
            let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg2) in
            children fg1 t1 t2
        in
        Fgtbl2.add fork_tbl (fg1, fg2) fg; fg
  in

  let rec visit (fg: Grc.Flowgraph.t) =
    try
      Fgtbl.find visit_tbl fg
    with Not_found ->
      let fg' = match fg with
        | Call (a, t) -> Call (a, visit t)
        | Test (tv, t1, t2) -> Test (tv, visit t1, visit t2)
        | Fork (t1, t2, sync) -> sequence_of_fork sync (visit t1) (visit t2)
        | Sync ((i1, i2), t1, t2) -> Sync ((i1, i2), visit t1, visit t2)
        | Pause -> Pause
        | Finish -> Finish
      in
      Fgtbl.add visit_tbl fg fg'; fg'
  in
  visit fg



type ml_test_expr =
  | MLsig of string
  | MLselect of int
  | MLfinished

type ml_code =
  | Clist of ml_ast list
  | Capp of ml_code * ml_code
and ml_ast =
  | MLif of ml_test_expr * ml_code * ml_code
  | MLenter of int
  | MLexit of int
  | MLexpr of Parsetree.expression


let nop = Clist []
let ml l = Clist l
let mls e = Clist [e]
let (++) c1 c2 = Capp (c1, c2)

let generate tast =
  let _selection_tree, control_flowgraph as grc = Grc.of_ast tast in
  let open Grc.Flowgraph in
  let _deps = check_causality_cycles grc in
  let _interleaved_grc = interleave control_flowgraph in
  ()
  (* ml_of_grc control_flowgraph selection_tree *)
