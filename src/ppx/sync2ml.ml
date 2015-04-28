(* generating the ocaml code from ast *)


open Pendulum_misc
open Pendulum_preproc

type error = Cyclic_causality of Grc.Flowgraph.t
exception Error of Location.t * error
let error ~loc e = raise (Error (loc, e))

let print_error fmt e =
  let open Format in
  fprintf fmt "%s"
    begin match e with
      | Cyclic_causality fg -> "Cyclic causality"
    end


let rec ml_of_grc e =
  let rec visit e =
    assert false
  in visit e


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
      ignore @@ visit_fg m t1;
      ignore @@ visit_fg m t2; ()
    | Node (n, t) ->
      ignore @@ visit_fg (visit_node m fg n) t
    | Leaf n -> ignore @@ visit_node m fg n; ()
  in
  visit_fg StringMap.empty fg



let generate tast =
  let _selection_tree, control_flowgraph as grc = Grc.of_ast tast in
  check_causality_cycles grc;
  ()
  (* ml_of_grc control_flowgraph selection_tree *)
