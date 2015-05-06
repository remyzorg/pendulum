(* generating the ocaml code from ast *)


open Pendulum_misc
open Pendulum_preproc
open Pendulum_preproc.Utils

type error = Cyclic_causality of Grc.Flowgraph.t
exception Error of Location.t * error
let error ~loc e = raise (Error (loc, e))

let print_error fmt e =
  let open Format in
  fprintf fmt "%s"
    begin match e with
      | Cyclic_causality fg -> "Cyclic causality"
    end


let check_causality_cycles grc =
  let open Grc.Flowgraph in
  let st, fg = grc in
  let visit_node m fg nd =
    match nd with
    | Test (Signal s) ->
      let prev = try StringMap.find s m with
        | Not_found -> []
      in
      StringMap.add s (fg :: prev) m
    | Call(Emit s) ->
      begin match StringMap.find s m with
        | h :: fgs -> error ~loc:Ast.dummy_loc @@ Cyclic_causality h
        | [] -> m
        | exception Not_found -> m
      end
    | _ -> m
  in
  let rec visit_fg m fg =
    match fg with
    | Node_bin (n, t1, t2) ->
      let m = visit_node m fg n in
      let m1 = visit_fg m t1 in
      let m2 = visit_fg m t2 in
      StringMap.merge (fun k v1 v2 ->
          match v1, v2 with
          | Some v1, Some v2 -> Some (v1 @ v2)
          | Some v, None | None, Some v -> Some v
          | _ -> None
      ) m1 m2
    | Node (n, t) ->
      visit_fg (visit_node m fg n) t
    | Leaf n -> visit_node m fg n
  in
  visit_fg StringMap.empty fg

module FgEmitsTbl = Hashtbl.Make(struct
    type t = Grc.Flowgraph.t * (int * int) * string
    let hash = Hashtbl.hash
    let equal (fg, stop, s) (fg', stop', s') =
      fg == fg' && stop = stop' && s = s'
  end)


let emits =
  let open Grc.Flowgraph in
  let emittbl = FgEmitsTbl.create 17 in
  let rec aux fg stop s =
    match fg with
    | (Node (Call(Emit s'), _) |
       Leaf (Call(Emit s'))
      ) when s = s' ->
      FgEmitsTbl.add emittbl (fg, stop, s) true;
      true
    | Node_bin (Sync(i1, i2), _, _) when (i1, i2) = stop ->
      FgEmitsTbl.add emittbl (fg, stop, s) false;
      false

    | Node_bin (_, t1, t2) ->
      let res = aux t1 stop s || aux t2 stop s in
      FgEmitsTbl.add emittbl (fg, stop, s) res;
      res
    | Node (_, t) ->
      let res = aux t stop s in
      FgEmitsTbl.add emittbl (fg, stop, s) res;
      res
    | Leaf _ -> false
  in
  fun fg stop s ->
    try FgEmitsTbl.find emittbl (fg, stop, s) with
    | Not_found -> aux fg stop s

let rec find_and_replace fg elt replace =
  let open Grc.Flowgraph in
  if fg == elt then true, replace fg
  else match fg with
    | Node (n, t) ->
      let res, t = find_and_replace t elt replace in
      res, Node(n, t)
    | Node_bin (n, t1, t2) ->
      let (res1, t1), (res2, t2) =
        find_and_replace t1 elt replace, find_and_replace t2 elt replace
      in res1 || res2, Node_bin (n, t1, t2)
    | Leaf _ -> false, fg

let rec replace_join fg1 fg2 repl =
  let open Grc.Flowgraph in
  let res, fg2 = find_and_replace fg2 fg1 repl in
  if res then repl fg1, fg2
  else match fg1 with
    | Node (n, t) ->
      let fg1, fg2 = replace_join t fg2 repl in
      Node(n, fg1), fg2
    | Node_bin (n, t1, t2) ->
      let t1, fg2 = replace_join t1 fg2 repl in
      let t2, fg2 = replace_join t2 fg2 repl in
      Node_bin(n, t1, t2), fg2
    | Leaf _ -> fg1, fg2

let rec sequencing fg =
  let open Grc.Flowgraph in
  let rec sequence_of_fork fg1 fg2 stop =
    match fg1, fg2 with
    | fg1, fg2 when fg1 == fg2 -> fg1

    | (Node_bin (Sync (i1, i2), t1, t2), fg
      | fg, Node_bin (Sync (i1, i2), t1, t2) )
      when (i1, i2) = stop -> fg

    | Node (n, t), fg2 ->
      Node(n, sequence_of_fork t fg2 stop)

    | Node_bin (Test (Signal s), t1, t2) as test, fg ->
      if emits fg stop s then
        match fg with
        | Node (n, t) -> Node (n, sequence_of_fork t test stop)
        | Node_bin (n, t1, t2) ->
          let fg1, fg2 = replace_join t1 t2 (fun replaced ->
              sequence_of_fork t test stop
            )
          in Node_bin (n, t1, t2)
      else
        let fg1, fg2 = replace_join t1 t2 (fun replaced -> sequence_of_fork )
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
  let visit_tv = function
    | Signal s -> MLsig s
    | Selection i -> MLselect i
    | Finished -> MLfinished
  in
  let visit_node = function
    | Call a -> assert false
    | Test tv -> assert false
    | Sync (i1, i2) -> assert false
    | Fork -> assert false
    | Dep -> assert false
  in
  let rec visit_fg = function
    | Node_bin (Test tv, t1, t2) ->
      mls @@ MLif (visit_tv tv, visit_fg t1, visit_fg t2)
    | Node_bin (nd, t1, t2) -> assert false
    | Node (nd, t) -> assert false
    | Leaf nd -> assert false
  in
  ()
  (* ml_of_grc control_flowgraph selection_tree *)
