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
      | Par_leads_to_finish fg -> "Par leads to pause or exit"
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

let children fg t1 t2 =
  let open Grc.Flowgraph in
  match fg with
  | Sync (ids, _, _) -> Sync (ids, t1, t2)
  | Test (tv, _, _) -> Test (tv, t1, t2)
  | Fork (_, _, sync) -> Fork (t1, t2, sync)
  | Call (a, _) -> Call(a, t1)
  | Pause | Finish -> fg

let rec find_and_replace fg elt replf =

  let open Grc.Flowgraph in
  if fg == elt then true, replf fg
  else match fg with
    | Call (a, t) ->
      let res, t = find_and_replace t elt replf in
      res, children fg t t
    | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _)->
      let res1, t1 = find_and_replace t1 elt replf in
      let res2, t2 = find_and_replace t2 elt replf in
      res1 || res2, children fg t1 t2
    | Pause | Finish -> false, fg

let rec replace_join (fg1 : Grc.Flowgraph.t) fg2 replf =
  let open Grc.Flowgraph in
  let res, fg2 = find_and_replace fg2 fg1 replf in
  if res then replf fg1, fg2
  else match fg1 with
    | Call(a, t) ->
      let fg1, fg2 = replace_join t fg2 replf in
      Call (a, fg1), fg2
    | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->
      let t1, fg = replace_join t1 fg2 replf in
      let t2, fg = replace_join t2 fg2 replf in
      children fg1 t1 t2, fg2
    | Pause | Finish -> fg1, fg2

let rec interleave fg =
  let open Grc.Flowgraph in
  let rec sequence_of_fork (stop: Grc.Flowgraph.t) fg1 fg2 =
    match fg1, fg2 with
    | fg1, fg2 when fg1 == fg2 -> fg1
    | fg1, fg2 when fg1 == stop -> fg2
    | fg1, fg2 when fg2 == stop -> fg1
    (* | (Pause | Finish), (Pause | Finish) -> assert false (\* TODO: Raise exn *\) *)
    | (Pause | Finish), _ | _, (Finish | Pause) -> assert false (* TODO: Raise exn *)
    | Test (Signal s, t1, t2), fg2 ->
      if emits fg2 stop s then match fg2 with
        | Call (a, t) -> Call (a, sequence_of_fork stop t fg1)
        | Pause | Finish -> assert false (* TODO: Raise exn *)
        | Sync(_, t1, t2) | Test (_, t1, t2) ->
          let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg1) in
          children fg2 t1 t2
        | Fork (t1, t2, sync) ->
          let fg2 = sequence_of_fork stop t1 t2 in
          sequence_of_fork sync fg1 fg2
      else
        let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg2) in
        children fg1 t1 t2
    | Call (action, t), fg2 -> Call (action, sequence_of_fork stop t fg2)
    | (Fork (t1, t2, sync) as fg1), (_ as fg2)
    | (_ as fg2), (Fork (t1, t2, sync) as fg1) ->
      let fg1 = sequence_of_fork sync t1 t2 in
      sequence_of_fork stop fg1 fg2
    | Sync (_, t1, t2), fg2 | Test (_, t1, t2), fg2 ->
      let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg2) in
      children fg1 t1 t2
  in
  let visit fg =
    assert false
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
  check_causality_cycles grc;
  ()
  (* ml_of_grc control_flowgraph selection_tree *)
