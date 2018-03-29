[@@@warning "-9"]

module Expression = struct

  type t = Parsetree.expression
  type core_type = Parsetree.core_type
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


let remove_ident_renaming s =
  try
    let content = String.sub s.content 0 ((String.rindex s.content '~')) in
    let int = int_of_string @@
      String.sub s.content (String.length content + 1) (String.length s.content - String.length content - 1)
    in
    int, Ast.({s with content})
  with Not_found -> 0, s

type ml_test_expr =
  | MLsig of Ast.signal
  | MLselect of int
  | MLor of ml_test_expr * ml_test_expr
  | MLand of ml_test_expr * ml_test_expr
  | MLboolexpr of Ast.atom
  | MLfinished
  | MLis_pause of ml_ast

and ml_sequence =
  | Seqlist of ml_ast list
  | Seq of ml_sequence * ml_sequence
and ml_ast =
  | MLemit of Ast.valued_signal
  | MLif of ml_test_expr * ml_sequence * ml_sequence
  | MLassign_signal of Ast.ident * ml_ast
  | MLassign_machine of int * (Ast.ident * Ast.signal Ast.run_param list * Ast.loc)
  | MLenter of int
  | MLexit of int
  | MLenters_exits of (Bitset.t * Bitset.t)
  | MLexpr of Ast.atom
  | MLunitexpr of Ast.atom
  | MLpause
  | MLfinish
  | MLcall of Ast.ident * Ast.signal Ast.run_param list * Ast.loc

let rec pp_ml_test_expr fmt = Format.(function
  | MLsig s -> fprintf fmt "present %s" s.ident.content
  | MLselect i -> fprintf fmt "select %d" i
  | MLfinished -> fprintf fmt "finished"
  | MLor (mlt1, mlt2) -> fprintf fmt "%a || %a" pp_ml_test_expr mlt1 pp_ml_test_expr mlt2
  | MLand (mlt1, mlt2) -> fprintf fmt "%a && %a" pp_ml_test_expr mlt1 pp_ml_test_expr mlt2
  | MLboolexpr e -> fprintf fmt "%a" Ast.printexp e.exp
  | MLis_pause mle -> fprintf fmt "%a == Pause" (pp_ml_ast 0) mle
  )

and pp_type_ml_sequence lvl fmt =
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
    | MLassign_signal (s, ml) -> fprintf fmt "%sMLassign (%s, %a)" indent s.content (pp_type_ml_ast 0) ml
    | MLassign_machine (_, (id, sigs, loc)) ->
      pp_type_ml_ast lvl fmt (MLassign_signal (id, MLcall (id, sigs, loc)))
    | MLenter i -> fprintf fmt "%sEnter %d" indent i
    | MLexit i -> fprintf fmt "%sExit %d" indent i
    | MLenters_exits (bs_add, bs_rem) ->
      fprintf fmt "%s Entexs (%a, %a)" indent Bitset.pp bs_add Bitset.pp bs_rem
    | MLexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLunitexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLpause -> fprintf fmt "%sPause" indent
    | MLfinish -> fprintf fmt "%sFinish" indent
    | MLcall (id, sigs, _) ->
      fprintf fmt "%sMLcall (%s, [%a])" indent id.content
        (MList.pp_iter ~sep:"; " Ast.(fun fmt -> function
             | Sig_param x -> fprintf fmt "%s" x.ident.content
             | Exp_param e -> fprintf fmt "!(%a)" Ast.printexp e
           )) sigs
    )

and pp_ml_sequence lvl fmt =
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
    | MLassign_signal (id, ml) ->
      fprintf fmt "%s%s := %a" indent id.content (pp_ml_ast 0) ml
    | MLassign_machine (_, (id, sigs, loc)) ->
      fprintf fmt "%s%s := %a" indent id.content (pp_ml_ast 0) (MLcall (id, sigs, loc))

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
    | MLenters_exits (bs_add, bs_rem) ->
      fprintf fmt "%s union %a, inter %a" indent Bitset.pp bs_add Bitset.pp bs_rem
    | MLexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLunitexpr e -> fprintf fmt "%s%s" indent (asprintf "%a" Ast.printexp e.exp)
    | MLpause -> fprintf fmt "%sPause" indent
    | MLfinish -> fprintf fmt "%sFinish" indent
    | MLcall (id, sigs, _) -> fprintf fmt "%s%s (%a)" indent id.content
        (MList.pp_iter ~sep:", "
           Ast.(fun fmt -> function
               | Sig_param x -> fprintf fmt "%s" x.ident.content
               | Exp_param e -> fprintf fmt "%a" Ast.printexp e
             )) sigs
    )

let nop = Seqlist []
let ml l = Seqlist l
let mls e = Seqlist [e]
let (++) c1 c2 = Seq (c1, c2)

let concat_setter ident n = Ast.mk_loc ~loc:ident.loc (* creation of a setter given by a run *)
    (Format.sprintf "set~%s~arg~%d" ident.content n)

let rec mk_ml_action deps mr a =
  let open Flowgraph in
  match a with
  | Emit vs -> mr := SignalSet.add vs.signal !mr; mls @@ MLemit vs
  | Atom e -> mls @@ MLunitexpr e
  | Enter i -> mls @@ MLenter i
  | Exit i -> ml @@ MLexit i :: List.map (fun x -> MLexit x) deps.(i)
  | Local_signal vs -> mls @@ MLassign_signal (vs.signal.ident, MLexpr vs.svalue)
  | Instantiate_run (id, sigs, loc) ->
    let inst_int_id, machine_id = remove_ident_renaming id in
    mls @@ MLassign_machine (inst_int_id, (machine_id, sigs, loc))
  | Compressed (la, ra) ->
    mk_ml_action deps mr la ++ mk_ml_action deps mr ra


let (<::) sel l =
  let open Selection_tree in
  if sel.tested then sel.label :: l else l


let deplist sel =
  let open Selection_tree in
  let env = ref [] in
  let maxid = ref 0 in
  let rec visit sel =
    maxid := max !maxid sel.label;
    match sel.t with
    | Bottom -> env := (sel.label, []) :: !env; sel <:: []
    | Pause -> env := (sel.label, []) :: !env; sel <:: []
    | Par sels | Excl sels ->
      let l = List.fold_left (fun acc sel -> acc @ (visit sel)) [] sels in
      env := (sel.label, l) :: !env; sel <:: l
    | Ref st ->
      let l = visit st in
      env := (sel.label, l) :: !env; sel <:: l
  in ignore(visit sel); !maxid, !env

let mk_test_expr mr tv =
  let open Flowgraph in
  match tv with
  | Signal (vs, None) -> mr := SignalSet.add vs !mr; MLsig vs
  | Signal (vs, Some at) ->
    mr := SignalSet.add vs !mr; MLand (MLsig vs, MLboolexpr at)
  | Selection i -> MLselect i
  | Sync (i1, i2) -> MLor (MLselect i1, MLselect i2)
  | Finished -> MLfinished
  | Is_paused (id, sigs, loc) -> MLis_pause (MLcall (id, sigs, loc))



let grc2ml dep_array fg =
  let open Flowgraph in
  let tbl = Fgtbl.create 19 in
  let sigs = ref SignalSet.empty in
  let rec mk stop fg =
    let aux stop fg =
      let res = begin match fg with
        | Call (a, t) -> mk_ml_action dep_array sigs a ++ mk stop t
        | Test (tv, t1, t2, endt) ->
          let res =
            match endt with
            | None -> Schedule.find_join true t1 t2
            | Some _ -> endt
          in
          begin
            match res with
            | Some j when j <> Finish && j <> Pause ->
              (mls @@ MLif
                 (mk_test_expr sigs tv,
                  mk (Some j) t1,
                  mk (Some j) t2))
              ++ (match stop with
                  | Some fg' when fg' == j -> nop
                  | _ -> mk stop j)
            | _ ->
              mls @@ MLif
                (mk_test_expr sigs tv, mk None t1, mk None t2)
          end
        | Fork (_, _, _) -> assert false
        | Pause -> mls MLpause
        | Finish -> mls MLfinish
      end
      in
      Fgtbl.add tbl fg res;
      res
    in match stop with
    | Some fg' when fg == fg' && fg' <> Finish && fg' <> Pause ->
      nop
    | _ -> aux stop fg
  in
  mk None fg


module ML_optimize = struct

  let mk_enters_exits maxid = (Bitset.make maxid false, Bitset.make maxid true)
  let gather_enter_exits mlseq maxid =
    let rec aux_gather acc mlseq =
      match mlseq with
      | Seq (Seqlist [], Seqlist []) | Seqlist [] -> acc, mlseq
      | Seq (mlseq, Seqlist []) | Seq (Seqlist [], mlseq) ->
        aux_gather acc mlseq
      | Seqlist ml_asts ->
        let entexs, mll = List.fold_left (fun (entexs, mll as acc) ml ->
            match ml with
            | MLenter cur_i ->
              begin match entexs with
                | Some (bs_add, _) ->
                  Bitset.add bs_add cur_i; acc
                | _ ->
                  let bs_add, _ as bs = mk_enters_exits maxid in
                  Bitset.add bs_add cur_i;
                  Some bs, MLenters_exits bs :: mll
              end
            | MLexit cur_i ->
              begin match entexs with
                | Some (_, bs_rem) ->
                  Bitset.remove bs_rem cur_i; acc
                | _ ->
                  let _, bs_rem as bs = mk_enters_exits maxid in
                  Bitset.remove bs_rem cur_i;
                  Some bs, MLenters_exits bs :: mll
              end
            | MLif (mlte, mlseq1, mlseq2) ->
              None, MLif (mlte, snd @@ aux_gather None mlseq1,
                          snd @@ aux_gather None mlseq2) :: mll
            | ml -> None, ml :: mll
          ) (acc, []) ml_asts
        in entexs, Seqlist (List.rev mll)
      | Seq (mlseq1, mlseq2) ->
        let entexs, mll1 = aux_gather acc mlseq1 in
        let entexs', mll2 = aux_gather entexs mlseq2 in
        entexs',
        match mll1, mll2 with
        | Seqlist [], Seqlist [] -> Seqlist []
        | Seqlist l, Seqlist [] | Seqlist [], Seqlist l  -> Seqlist l
        | mll1, mll2 -> Seq (mll1, mll2)
    in
    let _, mlseq = aux_gather None mlseq in
    mlseq

  let rm_atom_deps =
    let open Ast_mapper in
    let open Parsetree in
    let open Longident in
    let mapper lres htbl =
      {default_mapper with
       expr = (fun mapper exp ->
           match exp with
           | {pexp_desc = Pexp_ident {txt = Lident content}}
             when Hashtbl.mem htbl content ->
             let s, r = Hashtbl.find htbl content in
             if r then ()
             else (
               Hashtbl.add htbl content (s, true);
               lres := s :: !lres;
             );
             default_mapper.expr mapper exp
           | x -> default_mapper.expr mapper x
         );
      }
    in fun atom ->
      let htbl = Hashtbl.create 19 in
      let sigs_res = ref [] in
      List.iter (fun x -> Hashtbl.add htbl x.Ast.ident.content (x, false)) atom.locals;
      let mapper = mapper sigs_res htbl in
      let _ = mapper.expr mapper atom.exp in
      {atom with locals = !sigs_res}

  let rm_useless_let_bindings mlseq =
    let rec aux_rm_test texp = match texp with
      | MLboolexpr atom -> MLboolexpr (rm_atom_deps atom)
      | MLor (texp1, texp2) -> MLor (aux_rm_test texp1, aux_rm_test texp2)
      | MLand (texp1, texp2) -> MLand (aux_rm_test texp1, aux_rm_test texp2)
      | texp -> texp
    and aux_rm_ml ml = match ml with
      | MLemit vs -> MLemit {vs with svalue = rm_atom_deps vs.svalue}
      | MLexpr atom -> MLexpr (rm_atom_deps atom)
      | MLunitexpr atom -> MLunitexpr (rm_atom_deps atom)
      | MLif (ml_test_expr, mlseq1, mlseq2) ->
        MLif (aux_rm_test ml_test_expr,
              aux_rm_seq mlseq1, aux_rm_seq mlseq2)
      | MLassign_signal (id, ml_ast) -> MLassign_signal (id, aux_rm_ml ml_ast)
      | mlexp -> mlexp
    and aux_rm_seq mlseq =
      match mlseq with
      | Seq (Seqlist [], Seqlist []) | Seqlist [] -> mlseq
      | Seq (mlseq, Seqlist []) | Seq (Seqlist [], mlseq) ->
        aux_rm_seq mlseq
      | Seqlist l -> Seqlist (List.map aux_rm_ml l)
      | Seq (mlseq1, mlseq2) ->
        Seq(aux_rm_seq mlseq1, aux_rm_seq mlseq2)
    in aux_rm_seq mlseq

end
