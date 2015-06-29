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
  | MLor of ml_test_expr * ml_test_expr
  | MLfinished
    [@@deriving show]

let rec pp_ml_test_expr fmt = Format.(function
  | MLsig s -> fprintf fmt "present %s" s
  | MLselect i -> fprintf fmt "select %d" i
  | MLfinished -> fprintf fmt "finished"
  | MLor (mlt1, mlt2) -> fprintf fmt "%a || %a" pp_ml_test_expr mlt1 pp_ml_test_expr mlt2
  )

type ml_sequence =
  | Seqlist of ml_ast list
  | Seq of ml_sequence * ml_sequence
             [@@deriving show]
and ml_ast =
  | MLemit of string
  | MLif of ml_test_expr * ml_sequence * ml_sequence
  | MLenter of int
  | MLexit of int
  | MLexpr of Parsetree.expression [@printer fun fmt -> Pprintast.expression fmt]
  | MLpause
  | MLfinish
      [@@deriving show]

(* |[h] -> Format.fprintf fmt "%a" pp_element h *)
(*   |h::t -> *)
(*       Format.fprintf fmt "%a%s@,%a" *)
(*       pp_element h sep (pp_list ~sep pp_element) t *)
(*   |[] -> () *)

let pp_deriving_ml_sequence = pp_ml_sequence

let rec pp_list ?(sep="") pp_element fmt = function
  |[h] -> Format.fprintf fmt "%a" pp_element h
  |h::t ->
      Format.fprintf fmt "%a%s@,%a"
      pp_element h sep (pp_list ~sep pp_element) t
  |[] -> ()

let rec pp_ml_sequence fmt =
  (* Format.printf "@."; *)
  (* Format.(fprintf fmt "@[<v 1>@,%a@]@." *)
  (*   (pp_list ~sep:"," (fun fmt s -> fprintf fmt "%s" s)) ["aa";"bb";"cc"]) *)

let rec pp_list ?(sep="") pp_element fmt = function
  |[h] -> Format.fprintf fmt "%a" pp_element h
  |h::t ->
      Format.fprintf fmt "%a%s@,%a"
      pp_element h sep (pp_list ~sep pp_element) t
  |[] -> ()

let pp_cell fmt cell = Format.fprintf fmt "%s" cell


  Format.(function
    | Seqlist [] -> ()
    | Seqlist ml_asts -> MList.pp_iter ~sep:";" pp_ml_ast fmt ml_asts
    | Seq (mlseq1, mlseq2) ->
      begin match mlseq2 with
      | Seqlist [] | Seq (Seqlist [], Seqlist []) -> fprintf fmt "%a" pp_ml_sequence mlseq1
      | mlseq2 -> fprintf fmt "%a;%a" pp_ml_sequence mlseq1 pp_ml_sequence mlseq2
      end)

and pp_ml_ast fmt = Format.(function
    | MLemit s -> fprintf fmt "emit s"
    | MLif (mltest_expr, mlseq1, mlseq2) ->
      fprintf fmt "@[<v 2>if %a then begin@\n" pp_ml_test_expr mltest_expr;
      pp_ml_sequence fmt mlseq1;
      fprintf fmt "@]@\nend";
      begin match mlseq2 with
       | Seqlist [] | Seq (Seqlist [], Seqlist []) -> ()
       | mlseq2 -> Format.fprintf fmt "@[<v 2>else begin@\n %a@]@.end" pp_ml_sequence mlseq2
      end

    | MLenter i -> fprintf fmt "enter %d" i
    | MLexit i -> fprintf fmt "exit %d" i
    | MLexpr e -> fprintf fmt "%a" Pprintast.expression e
    | MLpause -> fprintf fmt "Pause"
    | MLfinish -> fprintf fmt "Finish"
  )

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

let grc2ml (fg : Grc.Flowgraph.t) =
  let open Grc.Flowgraph in
  let rec construct stop fg =
    match stop with
    | Some fg' when fg == fg' -> nop
    | _ ->
      begin match fg with
        | Call (a, t) -> (mls @@ construct_ml_action a) ++ construct None t
        | Test (tv, t1, t2) ->
          begin
            match Grc.Schedule.find_join t1 t2 with
            | None -> mls @@ MLif (construct_test_expr tv, construct None t1, construct None t2)
            | Some j ->
              (mls @@ MLif (construct_test_expr tv, construct (Some j) t1, construct (Some j) t2))
              ++ (match stop with
              | Some fg' when fg' == j -> nop
              | _ -> construct None j)
          end
        | Fork (t1, t2, sync) -> assert false
        | Sync ((i1, i2), t1, t2) ->
          mls @@ MLif (MLor (MLselect i1, MLselect i2), construct None t1, construct None t2)
        | Pause -> mls MLpause
        | Finish -> mls MLfinish
      end
  in
  construct None fg


let generate tast =
  let _selection_tree, control_flowgraph as grc = Grc.Of_ast.construct tast in
  let open Grc in
  let _deps = Schedule.check_causality_cycles grc in
  let interleaved_grc = Schedule.interleave control_flowgraph in
  let _ml_ast = grc2ml interleaved_grc in
  ()
  (* ml_of_grc control_flowgraph selection_tree *)
