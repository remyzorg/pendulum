(* generating the ocaml code from ast *)


open Utils

type error = Noerr
exception Error of Location.t * error
let error ~loc e = raise (Error (loc, e))

let print_error fmt e =
  let open Format in
  fprintf fmt "%s"
    begin match e with
      | _ -> assert false
    end

type ml_test_expr =
  | MLsig of string
  | MLselect of int
  | MLfinished

type ml_sequence =
  | Seqlist of ml_ast list
  | Seq of ml_sequence * ml_sequence
and ml_ast =
  | MLemit of string
  | MLif of ml_test_expr * ml_sequence * ml_sequence
  | MLenter of int
  | MLexit of int
  | MLexpr of Parsetree.expression

let nop = Seqlist []
let ml l = Seqlist l
let mls e = Seqlist [e]
let (++) c1 c2 = Seq (c1, c2)
let (++) c1 c2 = Seq (c1, c2)


let construct_ml_action a =
  let open Grc.Flowgraph in
  match a with
  | Emit s -> MLemit s
  | Atom e -> MLexpr e
  | Enter i -> MLenter i
  | Exit i -> MLexit i

let construct_test_expr tv =
  let open Grc.Flowgraph in
  match tv with
  | Signal s -> MLsig s
  | Selection i -> MLselect i
  | Finished -> MLfinished

let grc2ml fg =
  let open Grc.Flowgraph in
  let rec construct stop fg =
    match fg with
    | Call (a, t) -> (mls @@ construct_ml_action a) ++ construct None t
    | Test (tv, t1, t2) ->
      let join = Grc.Schedule.find_join t1 t2 in
      begin
        match join with
        | None -> mls @@ MLif (construct_test_expr tv, construct None t1, construct None t2)
        | Some j ->
          (mls @@ MLif (construct_test_expr tv, construct None t1, construct None t2))
          ++ construct None j
      end
    | Fork (t1, t2, sync) -> assert false
    | Sync ((i1, i2), t1, t2) -> assert false
    | Pause -> assert false
    | Finish -> assert false
  in
  construct fg


let generate tast =
  let _selection_tree, control_flowgraph as grc = Grc.Of_ast.construct tast in
  let open Grc in
  let _deps = Schedule.check_causality_cycles grc in
  let _interleaved_grc = Schedule.interleave control_flowgraph in
  ()
  (* ml_of_grc control_flowgraph selection_tree *)
