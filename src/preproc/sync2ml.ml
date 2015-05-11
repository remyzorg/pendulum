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
    | Call (Emit s', t) when s = s' ->
      FgEmitsTbl.add emittbl (fg, stop, s) true;
      true
    | Sync((i1, i2) as stop', _, _) when stop' = stop ->
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


let rec interleave fg =
  let open Grc.Flowgraph in
  let rec sequence_of_fork fg1 fg2 stop =
    match fg1, fg2 with
    | Test (Signal s, t1, t2), fg2 -> assert false
    | Call (action, t), fg2 -> assert false
      (* Call (action, sequence_of_fork f fg2 stop) *)
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
