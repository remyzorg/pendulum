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
  | MLassign_machine of int * (Ast.ident * Ast.signal list * Ast.loc)
  | MLenter of int
  | MLexit of int
  | MLenters_exits of (Bitset.t * Bitset.t)
  | MLexpr of Ast.atom
  | MLunitexpr of Ast.atom
  | MLpause
  | MLfinish
  | MLcall of Ast.ident * Ast.signal list * Ast.loc

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
    | MLassign_machine (s, (id, sigs, loc)) ->
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
        (MList.pp_iter ~sep:"; " (fun fmt x -> fprintf fmt "%s" x.ident.content)) sigs
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
        (MList.pp_iter ~sep:", " (fun fmt x -> fprintf fmt "%s" x.ident.content)) sigs
    )

let nop = Seqlist []
let ml l = Seqlist l
let mls e = Seqlist [e]
let (++) c1 c2 = Seq (c1, c2)

let concat_setter ident n = Ast.mk_loc ~loc:ident.loc (* creation of a setter given by a run *)
    (Format.sprintf "set~%s~arg~%d" ident.content n)

let construct_ml_action deps mr a =
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

let construct_test_expr mr tv =
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
  let rec construct stop fg =
    let rec aux stop fg =
      let res = begin match fg with
        | Call (a, t) -> construct_ml_action dep_array sigs a ++ construct stop t
        | Test (tv, t1, t2, endt) ->
          let res =
            match endt with
            | None -> Schedule.find_join true t1 t2
            | Some endt' -> endt
          in
          begin
            match res with
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
  construct None fg


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
    let open Asttypes in
    let mapper lres htbl =
      {default_mapper with
       expr = (fun mapper exp ->
           match exp with
           | {pexp_desc = Pexp_ident {txt = Lident content; loc}}
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

module Ocaml_gen = struct

  open Ast_helper
  open Parsetree

  let dumb = Exp.constant (Asttypes.Const_int 0)

  let int_const i = Exp.constant (Asttypes.Const_int i)
  let string_const s = Exp.constant (Asttypes.Const_string(s, None))

  let mk_pat_var s = Pat.(Asttypes.(var @@ Location.mkloc s.content s.loc))

  let mk_ident s = Exp.ident ~loc:s.loc
      Location.(mkloc (Longident.Lident s.content) s.loc)

  let mk_value_binding ?(pvb_loc=Location.none)
      ?(pvb_attributes=[]) pvb_pat pvb_expr =
    { pvb_pat; pvb_expr; pvb_attributes; pvb_loc; }

  let expr_of_bitset bs = bs
  |> Array.to_list
  |> List.map int_const
  |> Exp.array

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

  let setter_arg = Ast.mk_loc "set~arg" (* argument name for the returned setter *)
  let stepfun_name = "p~stepfun"
  let animate_name = "animate"
  let animated_state_var_name = "animated_next_raf"
  let select_env_name = "pendulum~state"
  let select_env_var = Location.(mkloc select_env_name !Ast_helper.default_loc)
  let select_env_ident = mk_ident (Ast.mk_loc select_env_name)

  let mk_mach_inst_step mch =
    {mch with content = Format.sprintf "%s~step" mch.content}

  let mk_mach_inst_ident mch k =
    {mch with content = Format.sprintf "%s%s" mch.content
                  (if k != 0 then Format.sprintf "~%d" k else "")
    }

  let mk_set_mach_arg_n n mch = Ast.mk_loc ~loc:Ast.dummy_loc
      (Format.sprintf "%s~set~arg~%d" mch n)


  let remove_signal_renaming s =
    try
      Ast.({s with
            content = String.sub s.content 0 ((String.rindex s.content '~'))})
    with Not_found -> s

  let add_deref_local s =
    let open Ast in
    match s.origin with
    | Local -> [%expr ![%e mk_ident s.ident]]
    | _ -> mk_ident s.ident

  let rebind_locals_let locals e =
    (List.fold_left (fun acc x ->
         [%expr let [%p mk_pat_var @@ remove_signal_renaming x.ident] =
                  ![%e mk_ident x.ident]
                in [%e acc]]
       ) e locals)

  let mk_machine_instantiation machine_ident inst_int_id args =
    let inst_ident = mk_mach_inst_ident machine_ident inst_int_id in (* usefull to name setters *)
    let step_fun_ident = mk_mach_inst_step inst_ident in (* the actual reaction function *)
    let idents =
      (List.rev @@ step_fun_ident :: (snd @@ List.fold_left (fun (n, acc) arg ->
           let ident = mk_set_mach_arg_n n inst_ident.content in
           n + 1, ident :: acc
         ) (0, []) args))
    in
    let idents_as_pat = Pat.tuple @@ List.map mk_pat_var idents in
    let idents_as_ref = Exp.tuple @@ List.map (fun x -> [%expr ref [%e mk_ident x]]) idents in
    let machine_fun = mk_ident @@ machine_ident in
    let args_init_tuple_exp = match args with
      | [] -> [%expr ()]
      | [arg] -> [%expr [%e add_deref_local arg].value]
      | l -> Exp.tuple @@ List.map (fun arg ->
          [%expr [%e add_deref_local arg].value]
        ) l
    in idents_as_pat, machine_fun, args_init_tuple_exp, idents_as_ref, idents


  let construct_local_signals_definitions env e = List.fold_left (fun acc vs ->
      let atom = ML_optimize.rm_atom_deps vs.svalue in
      let rebinds = rebind_locals_let atom.locals
          [%expr ref (make_signal [%e vs.svalue.exp])]
      in
      [%expr let [%p mk_pat_var vs.signal.ident] = [%e rebinds] in [%e acc]]
    ) e !(env.Tagged.all_local_signals)

  let append_tag s tag =
    { s with ident = {s.ident with
          content = Format.sprintf "%s##%s" s.ident.content tag.content;
        }}

  let construct_global_signals_definitions env e = Tagged.(List.fold_left (fun acc s ->
      let signal_to_definition init_val next_def s =
        [%expr let [%p mk_pat_var s.ident] =
                 make_signal [%e init_val] in [%e next_def]]
      in
      try
        Hashtbl.find env.signals_tags s.ident.content
        |> List.map (append_tag s)
        |> List.fold_left (signal_to_definition [%expr None]) acc
      with Not_found -> signal_to_definition (mk_ident s.ident) acc s
    ) (construct_local_signals_definitions env e) env.global_signals)

  let construct_set_all_absent_definition env e =
    let open Tagged in
    let locals_setters =
      (List.fold_left (fun acc vs ->
           [%expr set_absent ![%e mk_ident vs.signal.ident]; [%e acc]]
         ) [%expr ()] !(env.all_local_signals))
    in
    let globals_setters =
      let cons_setabs s acc tag =
        [%expr set_absent [%e mk_ident @@ (append_tag s tag).ident]; [%e acc]]
      in List.fold_left (
        fun acc s -> try
            Hashtbl.find env.Tagged.signals_tags s.ident.content
            |> List.fold_left (cons_setabs s) acc
          with Not_found -> [%expr set_absent [%e mk_ident s.ident]; [%e acc]]
      ) locals_setters env.global_signals
    in
    [%expr let set_absent () = [%e globals_setters] in [%e e]]

  let construct_input_setters_tuple env stepfun =
    let open Tagged in
    let globals =
      List.filter (fun s -> not @@ Hashtbl.mem env.Tagged.signals_tags s.ident.content)
        env.global_signals
    in
    match globals with
    | [] -> true, [%expr stepfun]
    | [s] -> false, [%expr set_present_value [%e mk_ident s.ident], [%e stepfun]]
    | l -> false, Exp.tuple @@ List.fold_left (fun acc s ->
        [%expr set_present_value [%e mk_ident s.ident]] :: acc
      ) [stepfun] l

  let construct_machine_registers_definitions env e =
    IdentMap.fold (fun k (_, insts) acc ->
        List.fold_left (fun acc (inst_int_id, args) ->
            let idents_as_pat, machine_fun, args_init_tuple_exp, idents_as_ref, _ =
              mk_machine_instantiation k inst_int_id args
            in
            [%expr let [%p idents_as_pat] =
                     let [%p idents_as_pat] = [%e machine_fun] [%e args_init_tuple_exp]
                     in [%e idents_as_ref]
                   in [%e acc]]
          ) acc (List.rev insts)
      ) !(env.Tagged.machine_runs) e

  let construct_callbacks_assigns animate env e =
    let opexpr loc = mk_ident @@ Ast.mk_loc ~loc "##." in
    let construct_lhs s tag =
      [%expr [%e opexpr tag.loc] [%e mk_ident s.ident] [%e mk_ident tag]]
    in
    let step_call =
      if animate then
        [%expr [%e mk_ident @@ Ast.mk_loc animate_name] ()]
      else
        [%expr ignore @@ [%e mk_ident @@ Ast.mk_loc stepfun_name] ()]
    in
    let construct_rhs s tag =
      [%expr Dom_html.handler (
             fun ev ->
               set_present_value [%e mk_ident (append_tag s tag).ident] (Some ev);
               [%e step_call];
               Js._true)]
    in
    let construct_assign s acc tag =
      [%expr [%e construct_lhs s tag] := [%e construct_rhs s tag]; [%e acc]]
    in List.fold_left (
      fun acc s -> try
          Hashtbl.find env.Tagged.signals_tags s.ident.content
          |> List.fold_left (construct_assign s) acc
        with Not_found -> acc
    ) e env.Tagged.global_signals

  let construct_raf_call stepfun_ident =
    [%expr fun () ->
           if ![%e mk_ident @@ Ast.mk_loc animated_state_var_name] then ()
           else
             [%e mk_ident @@ Ast.mk_loc animated_state_var_name] := true;
             let _ = Dom_html.window##requestAnimationFrame
                 (Js.wrap_callback (fun _ ->
                      ignore @@ [%e stepfun_ident] ();
                      [%e mk_ident @@ Ast.mk_loc animated_state_var_name] := false;
                    ))
             in ()
    ]

  let construct_animate_mutex animate body =
    if not animate then body else
      [%expr let [%p mk_pat_var @@ Ast.mk_loc animated_state_var_name] = ref false in [%e body]]


  let construct_instanciation_body animate nstmts env sel stepfun_body =
    let open Selection_tree in
    let sigs_step_arg =
      match env.Tagged.global_signals with
      | [] -> [%pat? ()]
      | [s] -> mk_pat_var s.ident
      | l -> Pat.tuple @@ List.rev_map (fun s -> mk_pat_var s.ident) l
    in
    let stepfun_lambda_expr =
      [%expr fun () -> try [%e stepfun_body] with
             | Pause_exc -> set_absent (); Pause
             | Finish_exc -> set_absent (); Bitset.add [%e select_env_ident] 0; Finish]
    in
    let stepfun_ident_expr = mk_ident @@ Ast.mk_loc stepfun_name in
    let stepfun_pat_var = mk_pat_var @@ Ast.mk_loc stepfun_name in
    let animate_pat_var = mk_pat_var @@ Ast.mk_loc animate_name in
    let animate_ident_expr = mk_ident @@ Ast.mk_loc animate_name in

    let returned_stepfun_ident =
      if animate then animate_ident_expr else stepfun_ident_expr
    in

    let stepfun_definition e =
      if animate then
        Exp.let_ Asttypes.Recursive [
          Vb.mk stepfun_pat_var stepfun_lambda_expr;
          Vb.mk animate_pat_var (construct_raf_call stepfun_ident_expr);
        ] e
      else
        Exp.let_ Asttypes.Nonrecursive [Vb.mk stepfun_pat_var stepfun_lambda_expr] e
    in
    let no_params, input_setters_tuple =
      construct_input_setters_tuple env returned_stepfun_ident
    in
    let return_value_expr =
      if no_params then [%expr [%e returned_stepfun_ident]]
      else [%expr [%e input_setters_tuple]]
    in
    let body_expression =
      construct_global_signals_definitions env
      @@ construct_set_all_absent_definition env
      @@ construct_machine_registers_definitions env
      @@ construct_animate_mutex animate
      @@ stepfun_definition
      @@ construct_callbacks_assigns animate env
      @@ return_value_expr
    in
    [%expr
      let open Pendulum.Runtime_misc in
      let open Pendulum.Machine in
      fun [%p sigs_step_arg] ->
        let [%p Pat.var select_env_var] =
          Bitset.make [%e int_const (1 + nstmts)]
        in [%e body_expression ]]


  let rec construct_test env depl test =
    match test with
    | MLsig s -> [%expr !?[%e add_deref_local s]]
    | MLselect i -> [%expr Bitset.mem [%e select_env_ident] [%e int_const i]]
    | MLor (mlte1, mlte2) ->
      [%expr [%e construct_test env depl mlte1 ] || [%e construct_test env depl mlte2]]
    | MLand (mlte1, mlte2) ->
      [%expr [%e construct_test env depl mlte1 ] && [%e construct_test env depl mlte2]]
    | MLboolexpr pexpr ->
      rebind_locals_let pexpr.locals pexpr.exp
    | MLfinished -> [%expr Bitset.mem [%e select_env_ident] 0]
    | MLis_pause (MLcall (id, args, loc)) ->
      let step_ident = {id with content = Format.sprintf "%s~step" id.content} in
      let step_eq_pause = [%expr ![%e mk_ident step_ident] () == Pause] in
      let _, setters = List.fold_left (fun (nth, acc) arg ->
          let exp_if_set = [%expr
            if !? [%e add_deref_local arg] then
              ![%e mk_ident @@ mk_set_mach_arg_n nth id.content]
                [%e add_deref_local arg].value
          ] in
          nth + 1, [%expr [%e exp_if_set] ; [%e acc]]
        ) (0, step_eq_pause) args
      in
      setters
    | MLis_pause e -> assert false

  and construct_sequence env depl mlseq =
    match mlseq with
    | Seq (Seqlist [], Seqlist []) | Seqlist [] -> assert false
    | Seq (mlseq, Seqlist []) | Seq (Seqlist [], mlseq) ->
      construct_sequence env depl mlseq
    | Seqlist ml_asts ->
      List.fold_left (fun acc x ->
          if acc = dumb then construct_ml_ast env depl x
          else Exp.sequence acc (construct_ml_ast env depl x)
        ) dumb ml_asts
    | Seq (mlseq1, mlseq2) ->
      Exp.sequence (construct_sequence env depl mlseq1)
        (construct_sequence env depl mlseq2)

  and construct_ml_ast env depl ast =
    match ast with
    | MLemit vs ->
      let rebinded_expr = rebind_locals_let vs.svalue.locals vs.svalue.exp in
      [%expr set_present_value [%e add_deref_local vs.signal] [%e rebinded_expr]]
    | MLif (test, mlseq1, mlseq2) ->
      begin match mlseq1, mlseq2 with
        | mlseq1, (Seqlist [] | Seq (Seqlist [], Seqlist [])) ->
          [%expr if [%e construct_test env depl test]
                 then [%e construct_sequence env depl mlseq1]]
        | (Seqlist [] | Seq (Seqlist [], Seqlist [])), mseq2 ->
          [%expr if not [%e construct_test env depl test]
                 then [%e construct_sequence env depl mlseq2]]
        | _ ->
          [%expr if [%e construct_test env depl test] then
                   [%e construct_sequence env depl mlseq1]
                 else [%e construct_sequence env depl mlseq2]]
      end

    | MLassign_machine (inst_int_id, (machine_ident, sigs, loc)) ->
      let _, machine_fun, args_init_tuple_exp, _, idents =
        mk_machine_instantiation machine_ident inst_int_id sigs
      in
      let rename_with' =
        List.map (fun x -> {x with content = x.content ^ "'"}) idents
      in
      let rec mk_seq = function
        | [] -> [%expr ()]
        | [e] -> e
        | h::t -> [%expr [%e h]; [%e mk_seq t]]
      in
      let assigns =
        rename_with'
        |> List.combine idents
        |> List.map (fun (x, y) -> [%expr [%e mk_ident x] := [%e mk_ident y]])
        |> mk_seq
      in
      let renamed_pat = Pat.tuple @@ List.map mk_pat_var rename_with' in
      [%expr let [%p renamed_pat] =
               [%e machine_fun] [%e args_init_tuple_exp]
             in [%e assigns]
      ]

    | MLassign_signal (ident, mlast) ->
      let ocamlexpr = construct_ml_ast env depl mlast in
      [%expr [%e mk_ident ident] := make_signal [%e ocamlexpr]]

    | MLenter i ->
      [%expr Bitset.add
               [%e select_env_ident]
               [%e int_const i]]

    | MLexit i ->
        [%expr Bitset.remove
                 [%e select_env_ident]
                 [%e int_const i]]

    | MLenters_exits (bs_union, bs_inter as bs) ->
      Bitset.simplify bs;
      if Bitset.is_empty bs_union then
        if Bitset.is_full bs_inter then
          [%expr ()]
        else [%expr Bitset.inter [%e select_env_ident]
                      [%e expr_of_bitset bs_inter]]
      else if Bitset.is_full bs_inter then
        [%expr Bitset.union [%e select_env_ident]
                 [%e expr_of_bitset bs_union]]
      else
        [%expr
          Bitset.inter_union [%e select_env_ident] [%e expr_of_bitset bs_inter]
            [%e expr_of_bitset bs_union];
        ]

    | MLunitexpr pexpr ->
      rebind_locals_let pexpr.locals [%expr let () = [%e pexpr.exp] in ()]
    | MLexpr pexpr -> rebind_locals_let pexpr.locals pexpr.exp

    | MLpause -> [%expr raise Pause_exc]
    | MLfinish -> [%expr raise Finish_exc]
    | MLcall (ident, sigs, loc) ->
      let tuple = match sigs with [] -> assert false | [s] -> [%expr [%e add_deref_local s].value]
        | l -> Ast_helper.Exp.tuple ~loc @@ List.map (fun s -> [%expr [%e add_deref_local s].value]) l
      in [%expr [%e mk_ident ident] [%e tuple]]


end


let generate nooptim animate env tast =
  let selection_tree, flowgraph as grc = Of_ast.construct tast in
  Schedule.tag_tested_stmts selection_tree flowgraph;
  let _deps = Schedule.check_causality_cycles grc in
  let interleaved_cfg = Schedule.interleave flowgraph in
  let maxid, deps = deplist selection_tree in
  let dep_array = Array.make (maxid + 1) [] in
  let ml_ast = grc2ml dep_array interleaved_cfg in
  let ml_ast' =
    if not nooptim then
      ML_optimize.gather_enter_exits ml_ast maxid
      |> ML_optimize.rm_useless_let_bindings
    else ml_ast
  in
  Ocaml_gen.(
    construct_instanciation_body animate maxid env selection_tree
    @@ construct_sequence env dep_array ml_ast'
  )
