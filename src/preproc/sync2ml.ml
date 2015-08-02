(* generating the ocaml code from ast *)


module Expression = struct
  type t = Parsetree.expression
  let print = Pprintast.expression
  module Location = Location
end

module Ast = Ast.Make (Expression)
module Flowgraph = Grc.Flowgraph.Make (Ast)
module Selection_tree = Grc.Selection_tree.Make (Ast)
module Schedule = Grc.Schedule.Make (Flowgraph) (Selection_tree)
module Of_ast = Grc.Of_ast.Make (Flowgraph) (Selection_tree)

open Utils
open Ast

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
  | MLsig of Ast.signal
  | MLselect of int
  | MLor of ml_test_expr * ml_test_expr
  | MLfinished

let rec pp_ml_test_expr fmt = Format.(function
  | MLsig s -> fprintf fmt "present %s" s.ident.content
  | MLselect i -> fprintf fmt "select %d" i
  | MLfinished -> fprintf fmt "finished"
  | MLor (mlt1, mlt2) -> fprintf fmt "%a || %a" pp_ml_test_expr mlt1 pp_ml_test_expr mlt2
  )

type ml_sequence =
  | Seqlist of ml_ast list
  | Seq of ml_sequence * ml_sequence
and ml_ast =
  | MLemit of Ast.valued_signal
  | MLif of ml_test_expr * ml_sequence * ml_sequence
  | MLassign of ident * ml_ast
  | MLenter of int
  | MLexit of int
  | MLexpr of Ast.atom
  | MLunitexpr of Ast.atom
  | MLpause
  | MLfinish
  | MLcall of Ast.ident * Ast.signal list


let rec pp_type_ml_sequence lvl fmt =
  let indent = String.init lvl (fun _ -> ' ') in
  Format.(function
      (* | Seq (Seqlist [], s) | Seq (s, Seqlist[]) -> pp_type_ml_sequence lvl fmt s *)

      | Seqlist ml_asts ->
        fprintf fmt "%sSeqlist [\n%a]"
          indent (MList.pp_iter ~sep:";\n" (pp_type_ml_ast (lvl + 2))) ml_asts

      | Seq (mlseq1, mlseq2) ->
        fprintf fmt "%sSeq (\n%a,\n%a)" indent (pp_type_ml_sequence (lvl + 1))
          mlseq1 (pp_type_ml_sequence (lvl + 2)) mlseq2
    )

and pp_type_ml_ast lvl fmt =
  let indent = String.init lvl (fun _ -> ' ') in
  Format.(function
    | MLemit vs -> fprintf fmt "%sEmit %s" indent vs.signal.ident.content
    | MLif (mltest_expr, mlseq1, mlseq2) ->
      fprintf fmt "%sMLif(%a, \n%a, \n%a)"
        indent pp_ml_test_expr mltest_expr
        (pp_type_ml_sequence (lvl + 2)) mlseq1
        (pp_type_ml_sequence (lvl + 2)) mlseq2
    | MLassign (s, ml) -> fprintf fmt "%sMLassign (%s, %a)" indent s.content (pp_type_ml_ast 0) ml
    | MLenter i -> fprintf fmt "%sEnter %d" indent i
    | MLexit i -> fprintf fmt "%sExit %d" indent i
    | MLexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLunitexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLpause -> fprintf fmt "%sPause" indent
    | MLfinish -> fprintf fmt "%sFinish" indent
    | MLcall (id, sigs) -> assert false
    )

let rec pp_ml_sequence lvl fmt =
  Format.(function
      | Seqlist [] | Seq (Seqlist [], Seqlist []) -> assert false
      | Seq (Seqlist [], s) | Seq (s, Seqlist[]) -> pp_ml_sequence lvl fmt s

      | Seqlist ml_asts -> MList.pp_iter ~sep:";\n" (pp_ml_ast lvl) fmt ml_asts
      | Seq (mlseq1, mlseq2) ->
        fprintf fmt "%a;\n%a" (pp_ml_sequence lvl)
          mlseq1 (pp_ml_sequence lvl) mlseq2
    )

and pp_ml_ast lvl fmt =
  let indent = String.init lvl (fun _ -> ' ') in
  Format.(function
    | MLemit vs -> fprintf fmt "%semit %s" indent vs.signal.ident.content
    | MLassign (id, ml) ->

      fprintf fmt "%s := %a" id.content (pp_ml_ast 0) ml

    | MLif (mltest_expr, mlseq1, mlseq2) ->
      fprintf fmt "%sif %a then (\n%a\n%s)" indent pp_ml_test_expr mltest_expr
        (pp_ml_sequence (lvl + 2)) mlseq1 indent;
      begin match mlseq2 with
       | Seqlist [] | Seq (Seqlist [], Seqlist []) -> ()
       | mlseq2 ->
         Format.fprintf fmt
           "\n%selse (\n%a\n%s)"
           indent
           (pp_ml_sequence (lvl + 2))
           mlseq2
           indent
      end

    | MLenter i -> fprintf fmt "%senter %d" indent i
    | MLexit i -> fprintf fmt "%sexit %d" indent i
    | MLexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLunitexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLpause -> fprintf fmt "%sPause" indent
    | MLfinish -> fprintf fmt "%sFinish" indent
    | MLcall _ -> assert false
    )

let nop = Seqlist []
let ml l = Seqlist l
let mls e = Seqlist [e]
let (++) c1 c2 = Seq (c1, c2)
let (++) c1 c2 = Seq (c1, c2)


let construct_ml_action deps mr a =
  let open Flowgraph in
  match a with
  | Emit vs -> mr := SignalSet.add vs.signal !mr; mls @@ MLemit vs
  | Atom e -> mls @@ MLunitexpr e
  | Enter i -> mls @@ MLenter i
  | Exit i -> ml @@ MLexit i :: List.map (fun x -> MLexit x) deps.(i)
  | Local_signal vs -> mls @@ MLassign (vs.signal.ident, MLexpr vs.svalue)
  | Instantiate_run (id, sigs) -> assert false

let (<::) sel l =
  let open Selection_tree in
  if sel.tested then sel.label :: l else l

let deplist sel =
  let open Selection_tree in
  let env = ref [] in
  let rec visit sel =
    match sel.t with
    | Bottom -> env := (sel.label, []) :: !env; sel <:: []
    | Pause -> env := (sel.label, []) :: !env; sel <:: []
    | Par sels | Excl sels ->
      let l = List.fold_left (fun acc sel -> acc @ (visit sel)) [] sels in
      env := (sel.label, l) :: !env; sel <:: l
    | Ref st ->
      let l = visit st in
      env := (sel.label, l) :: !env; sel <:: l
  in ignore (visit sel); !env

let construct_test_expr mr tv =
  let open Flowgraph in
  match tv with
  | Signal vs -> mr := SignalSet.add vs !mr; MLsig vs
  | Selection i -> MLselect i
  | Finished -> MLfinished
  | Is_paused (id, _) -> assert false

let grc2ml dep_array fg =
  let open Flowgraph in
  let sigs = ref SignalSet.empty in
  let rec construct stop fg =
    match stop with
    | Some fg' when fg == fg' && fg' <> Finish && fg' <> Pause ->
      nop
    | _ ->
      begin match fg with
        | Call (a, t) -> construct_ml_action dep_array sigs a ++ construct stop t
        | Test (tv, t1, t2) ->
          begin
            match Schedule.find_join true t1 t2 with
            | Some j when j <> Finish && j <> Pause ->
              (mls @@ MLif
                 (construct_test_expr sigs tv,
                  construct (Some j) t1,
                  construct (Some j) t2))

              ++ (match stop with
                  | Some fg' when fg' == j -> nop
                  | _ -> construct stop j)
            | _ ->
              mls @@ MLif
                (construct_test_expr sigs tv, construct None t1, construct None t2)
          end
        | Fork (t1, t2, sync) -> assert false
        | Sync ((i1, i2), t1, t2) ->
          mls @@ MLif (MLor (MLselect i1, MLselect i2), construct stop t1, construct stop t2)
        | Pause -> mls MLpause
        | Finish -> mls MLfinish
      end
  in
  construct None fg

module Ocaml_gen = struct

  open Ast_helper
  open Parsetree

  let dumb = Exp.constant (Asttypes.Const_int 0)

  let int_const i = Exp.constant (Asttypes.Const_int i)
  let string_const s = Exp.constant (Asttypes.Const_string(s, None))

  let mk_pat_var s = Pat.(Asttypes.(var @@ Location.mkloc s.content s.loc))

  let mk_ident s = Exp.ident ~loc:s.loc Location.(mkloc (Longident.Lident s.content) s.loc)

  let mk_value_binding ?(pvb_loc=Location.none) ?(pvb_attributes=[]) pvb_pat pvb_expr =
    { pvb_pat; pvb_expr; pvb_attributes; pvb_loc; }

  let deplist sel =
    let open Selection_tree in
    let env = ref [] in
    let rec visit sel =
      match sel.t with
      | Bottom -> env := (sel.label, []) :: !env; [sel.label]
      | Pause -> env := (sel.label, []) :: !env; [sel.label]
      | Par sels | Excl sels ->
        let l = List.fold_left (fun acc sel -> acc @ (visit sel)) [] sels in
        env := (sel.label, l) :: !env; sel.label :: l
      | Ref st ->
        let l = visit st in
        env := (sel.label, l) :: !env; sel.label :: l
    in ignore (visit sel); !env

  let setter_arg = Ast.mk_loc "set~arg"

  let select_env_name = "pendulum~state"
  let select_env_var = Location.(mkloc select_env_name !Ast_helper.default_loc)
  let select_env_ident = mk_ident (Ast.mk_loc select_env_name)

  let remove_signal_renaming s =
    try
      Ast.({s with
            content = String.sub s.content 0 ((String.rindex s.content '~'))})
    with Not_found -> s

  let rebind_locals_let locals e =
    (List.fold_left (fun acc x ->
         [%expr let [%p mk_pat_var @@ remove_signal_renaming x.ident] =
                  ![%e mk_ident x.ident]
                in [%e acc]]
       ) e locals)

  let init nstmts (global_sigs,local_sigs) sel =
    let open Selection_tree in
    fun e ->
      let sigs_step_arg =
        match global_sigs with
        | [] -> [%pat? ()]
        | [s] -> mk_pat_var s.ident
        | l -> Pat.tuple @@ List.rev_map (fun s -> mk_pat_var s.ident) l
      in

      let let_sigs_local e = List.fold_left (fun acc vs ->
          let rebinds = rebind_locals_let vs.svalue.locals
              [%expr ref (make_signal [%e vs.svalue.exp])]
          in
          [%expr let [%p mk_pat_var vs.signal.ident] = [%e rebinds] in [%e acc]]
        ) e (local_sigs)
      in
      let let_sigs_global e = List.fold_left (fun acc s ->
          [%expr let [%p mk_pat_var s.ident] = make_signal [%e mk_ident s.ident] in [%e acc]]
        ) (let_sigs_local e) (global_sigs)
      in
      let let_set_sigs_absent e =
        let set_absent_locals =
          (List.fold_left (fun acc vs ->
               [%expr set_absent ![%e mk_ident vs.signal.ident]; [%e acc]]
             ) [%expr ()] local_sigs)
        in
        let set_absent_globals = (List.fold_left (fun acc s ->
             [%expr set_absent [%e mk_ident s.ident]; [%e acc]]
           ) set_absent_locals global_sigs)
        in
        [%expr let set_absent () = [%e set_absent_globals] in [%e e]]
      in
      let empty_params, return_input_setters =
        match global_sigs with
        | [] -> true, [%expr ()]
        | [s] -> false, [%expr fun [%p mk_pat_var setter_arg] ->
               set_present_value [%e mk_ident s.ident] [%e mk_ident setter_arg]]
        | l -> false, Exp.tuple @@ List.rev_map (fun s ->
            [%expr fun [%p mk_pat_var setter_arg ] ->
                   set_present_value [%e mk_ident s.ident] [%e mk_ident setter_arg]]
          ) global_sigs
      in
      let returns =
        let stepfun =
          [%expr fun () -> try [%e e] with
               | Pause_exc -> set_absent (); Pause
               | Finish_exc -> set_absent (); Bitset.add [%e select_env_ident] 0; Finish]
        in
        if empty_params then [%expr fun () -> [%e stepfun]]
        else [%expr [%e return_input_setters], [%e stepfun]]
      in
      [%expr
        let open Pendulum.Runtime_misc in
        let open Pendulum.Machine in
        fun [%p sigs_step_arg] ->
              let [%p Pat.var select_env_var] =
                Bitset.make [%e int_const (1 + nstmts)]
              in
              [%e let_sigs_global (let_set_sigs_absent returns)]]

  let add_ref_local s =
    match s.origin with
    | Local -> [%expr ![%e mk_ident s.ident]]
    | _ -> [%expr [%e mk_ident s.ident]]

  let rec construct_test test =
    match test with
    | MLsig s -> [%expr !?[%e add_ref_local s]]
    | MLselect i -> [%expr Bitset.mem [%e select_env_ident] [%e int_const i]]
    | MLor (mlte1, mlte2) -> [%expr [%e construct_test mlte1 ] || [%e construct_test mlte2]]
    | MLfinished -> [%expr Bitset.mem [%e select_env_ident] 0]

  let rec construct_sequence depl mlseq =
    match mlseq with
    | Seq (Seqlist [], Seqlist []) | Seqlist [] -> assert false
    | Seq (mlseq, Seqlist []) | Seq (Seqlist [], mlseq) ->
      construct_sequence depl mlseq
    | Seqlist ml_asts ->
      List.fold_left (fun acc x ->
          if acc = dumb then construct_ml_ast depl x
          else Exp.sequence acc (construct_ml_ast depl x)
        ) dumb ml_asts
    | Seq (mlseq1, mlseq2) ->
      Exp.sequence (construct_sequence depl mlseq1)
        (construct_sequence depl mlseq2)

  and construct_ml_ast depl ast =
    match ast with
    | MLemit vs ->
      let rebinded_expr = rebind_locals_let vs.svalue.locals vs.svalue.exp in
      [%expr set_present_value [%e add_ref_local vs.signal] [%e rebinded_expr]]
    | MLif (test, mlseq1, mlseq2) ->
      begin match mlseq1, mlseq2 with
        | mlseq1, (Seqlist [] | Seq (Seqlist [], Seqlist [])) ->
          [%expr if [%e construct_test test]
                 then [%e construct_sequence depl mlseq1]]
        | (Seqlist [] | Seq (Seqlist [], Seqlist [])), mseq2 ->
          [%expr if not [%e construct_test test]
                 then [%e construct_sequence depl mlseq2]]
        | _ ->
          [%expr if [%e construct_test test] then
                   [%e construct_sequence depl mlseq1]
                 else [%e construct_sequence depl mlseq2]]
      end

    | MLassign (ident, mlast) ->
      let ocamlexpr = construct_ml_ast depl mlast in
      [%expr [%e mk_ident ident] := make_signal [%e ocamlexpr]]

    | MLenter i ->
      [%expr Bitset.add
               [%e select_env_ident]
               [%e int_const i]]

    | MLexit i ->
        [%expr Bitset.remove
                 [%e select_env_ident]
                 [%e int_const i]]

    | MLunitexpr pexpr ->
      rebind_locals_let pexpr.locals [%expr let () = [%e pexpr.exp] in ()]
    | MLexpr pexpr -> rebind_locals_let pexpr.locals pexpr.exp

    | MLpause -> [%expr raise Pause_exc]
    | MLfinish -> [%expr raise Finish_exc]
    | MLcall (ident, sigs) ->
      (* Keep the lock from the tupple and put it back back here *)
      let tuple = Ast_helper.Exp.tuple ~loc:Location.none @@
        List.map (fun s -> mk_ident s.ident) sigs
      in [%expr [%e mk_ident ident] [%e tuple]]

  let instantiate dep_array sigs sel ml =
    init (Array.length dep_array) sigs sel (construct_sequence dep_array ml)



end


let generate ?(sigs=[],[]) tast =
  let selection_tree, controlflow_graph as grc = Of_ast.construct tast in
  Schedule.tag_tested_stmts selection_tree controlflow_graph;
  let _deps = Schedule.check_causality_cycles grc in
  let interleaved_cfg = Schedule.interleave controlflow_graph in

  let deps = deplist selection_tree in
  let dep_array = Array.make (List.length deps + 1) [] in
  List.iter (fun (i, l) -> dep_array.(i) <- l) deps;

  let ml_ast = grc2ml dep_array interleaved_cfg in

  let ocaml_ast = Ocaml_gen.instantiate dep_array sigs selection_tree ml_ast in
  ocaml_ast
