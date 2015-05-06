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


(* let rec topo fg = *)
(*   let h = Hashtbl.create 17 in *)
(*   let aux accl l = match l with *)
(*     | [] -> accl *)
(*     |  *)


(* let remove_forks grc = *)
(*   let open Grc.Flowgraph in *)
(*   let st, fg = grc in *)
(*   let visit_node m fg nd = *)
(*     match nd with *)
(*     | Test (Signal s) -> *)
(*       let prev = try StringMap.find s m with *)
(*         | Not_found -> [] *)
(*       in *)
(*       StringMap.add s (fg :: prev) m *)
(*     | Call(Emit s) -> *)
(*       begin match StringMap.find s m with *)
(*         | h :: fgs -> error ~loc:Ast.dummy_loc @@ Cyclic_causality h *)
(*         | [] -> m *)
(*         | exception Not_found -> m *)
(*       end *)
(*     | _ -> m *)
(*   in *)
(*   let rec visit_fg m fg = *)
(*     match fg with *)
(*     | Node_bin (Fork, t1, t2) -> assert false *)
(*     | Node_bin (n, t1, t2) -> fg *)
(*     | Node (n, t) -> fg *)
(*     | Leaf n -> fg *)
(*   in *)
(*   visit_fg StringMap.empty fg *)



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
