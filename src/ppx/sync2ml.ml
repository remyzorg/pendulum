(* generating the ocaml code from ast *)





let rec ml_of_grc e =
  let open Pendulum_preproc in
  let rec visit e =
    assert false
  in visit e





let generate dast =
  let open Pendulum_preproc in
  let ast = Ast.(Tagged.of_ast @@ normalize dast) in
  let _selection_tree, _control_flowgraph = Grc.of_ast ast in
  assert false
  (* ml_of_grc control_flowgraph selection_tree *)
