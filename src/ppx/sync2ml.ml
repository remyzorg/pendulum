(* generating the ocaml code from ast *)





let rec ml_of_grc e =
  let open Pendulum_preproc in
  let rec visit e =
    assert false
  in visit e


let emit_reference grc =
  let open Pendulum_preproc.Grc.Flowgraph in
  let h = Hashtbl.create 17 in
  let st, fg = grc in
  let visit_node fg nd =
    match nd with
    | Test (Signal s) ->
      let prev = try Hashtbl.find h s with
        | Not_found -> []
      in Hashtbl.replace h s (fg :: prev)
    | _ -> ()
  in
  let rec visit_fg fg =
    match fg with
    | Node_bin (n, t1, t2) -> visit_node fg n; visit_fg t1; visit_fg t2
    | Node (n, t) -> visit_node fg n; visit_fg t
    | Leaf n -> visit_node fg n
  in
  visit_fg fg



let generate dast =
  let open Pendulum_preproc in
  let ast = Ast.(Tagged.of_ast dast) in
  let _selection_tree, _control_flowgraph = Grc.of_ast ast in
  assert false
  (* ml_of_grc control_flowgraph selection_tree *)
