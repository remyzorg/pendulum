(* generating the ocaml code from ast *)


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

let mk_ml_action deps mr a =
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
    let rec aux stop fg =
      let res = begin match fg with
        | Call (a, t) -> mk_ml_action dep_array sigs a ++ mk stop t
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


  let build_tuple tuple mk init exprs =
    match exprs with
    | [] -> init
    | [s, t] -> mk ?t s
    | l -> tuple @@ List.rev_map (fun (s, t) -> mk ?t s) l

  let dumb = Exp.constant (Ast_helper.Const.int 0)
  let int_const i = Exp.constant (Ast_helper.Const.int i)
  let string_const s = Exp.constant (Ast_helper.Const.string s)

  let mk_pat_var ?t s =
    let pvar = Pat.(Asttypes.(var @@ Location.mkloc s.content s.loc)) in
    match t with None -> pvar | Some t ->
      Pat.(Asttypes.(constraint_ ~loc:s.loc pvar t))

  let signaltype_of_type t =
    [%type: ([%t t], _) Pendulum.Signal.signal]


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
  let reactfun_name = "p~react"
  let animate_name = "animate"
  let gather_str = "gather"
  let api_react_function_name = "react"
  let animated_state_var_name = "animated_next_raf"
  let select_env_name = "pendulum~state"
  let select_env_var = Location.(mkloc select_env_name !Ast_helper.default_loc)
  let select_env_ident = mk_ident (Ast.mk_loc select_env_name)
  let gather_val_arg_name = Ast.mk_loc "arg~~"

  let debug_instant_cnt_name = Ast.mk_loc "instant~cnt"

  let mk_mach_inst_step mch =
    {mch with content = Format.sprintf "%s~step" mch.content}

  let mk_mach_inst_ident mch k =
    {mch with content = Format.sprintf "%s%s" mch.content
                  (if k != 0 then Format.sprintf "~%d" k else "")
    }

  let mk_set_mach_arg_n n mch = Ast.mk_loc ~loc:Ast.dummy_loc
      (Format.sprintf "%s~set~arg~%d" mch n)

  let ident_app_str ident sep str =
    Ast.mk_loc ~loc:ident.loc
      (Format.sprintf "%s%s%s" ident.content sep str)

  let remove_signal_renaming s =
    try Ast.({s with content = String.sub s.content 0
                         ((String.rindex s.content '~'))})
    with Not_found -> s


  module Debug = struct
    let str = string_const

    let print expr =
      [%expr Firebug.console##debug (Js.string [%e expr])]

    let letin ~debug patvar value expr =
      if debug then [%expr let [%p patvar] = [%e value] in
                           [%e expr]]
      else expr

    let (++) a b = [%expr [%e a] ^ [%e b]]

    let seqif ~debug debug_expr expr =
      if debug then [%expr [%e debug_expr ]; [%e expr]] else expr
  end

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

  let handle_param = function
    | Sig_param s -> add_deref_local s
    | Exp_param e -> e

  let mk_machine_instantiation machine_ident inst_int_id args =
    (* used to name setters *)
    let inst_ident = mk_mach_inst_ident machine_ident inst_int_id in
    let prog_ident = mk_mach_inst_step inst_ident in (* the actual reaction function *)
    let handle_param = function
      | Sig_param s -> add_deref_local s
      | Exp_param e -> [%expr make_signal [%e e]] in
    let args_init_tuple_exp = match args with
      | [] -> [%expr ()]
      | [arg] -> handle_param arg
      | l -> Exp.tuple @@ List.map handle_param l in
    let prog_create_call =
      let id = mk_ident machine_ident in
      if args = [] then [%expr [%e id]#create_run]
      else [%expr [%e id]#create_run [%e args_init_tuple_exp]] in
    prog_ident, prog_create_call


  let mk_local_signals_definitions env e = List.fold_left (fun acc vs ->
      let atom = ML_optimize.rm_atom_deps vs.svalue in
      let rebinds = rebind_locals_let atom.locals
          [%expr ref (make_signal [%e vs.svalue.exp])]
      in
      [%expr let [%p mk_pat_var vs.signal.ident] = [%e rebinds] in [%e acc]]
    ) e !(env.Tagged.local_only_env)

  let append_tag s tag =
    {s with ident =
              {s.ident with content = Format.sprintf "%s##%s" s.ident.content tag.content;
                     }}

  let has_tobe_defined s = match s with
    | No_binding | Event _ -> true
    | Access _ -> false

  let signal_to_creation_expr init_val s =
    match s.bind with
    | Event (e, gopt) ->
      Option.casefv gopt (fun g ->
          [%expr make_signal_gather ([%e g] :> _ * ( _ -> #Dom_html.event Js.t -> _ ))]
        ) [%expr make_event_signal [%e init_val]]
    | _ ->
      Option.casefv s.gatherer (fun g ->
          [%expr make_signal_gather ([%e init_val],[%e g])])
        [%expr make_signal [%e init_val]]

  let signal_to_definition rhs next_def s =
    [%expr let [%p mk_pat_var s.ident] = [%e rhs] in [%e next_def]]


  let mk_args_signals_definitions env e =
    let open Tagged in
    List.fold_left (fun acc (s, _) ->
        try
          Hashtbl.find env.binders_env s.ident.content
          |> MList.map_filter has_tobe_defined (function
              | Event (e, gatherer) as bind -> { (append_tag s e) with gatherer; bind}
              | _ -> s)
          |> List.fold_left (fun acc s ->
              signal_to_definition (signal_to_creation_expr [%expr None] s) acc s
            ) acc
        with Not_found ->
          if s.origin = Output then
            [%expr let [%p mk_pat_var s.ident],
                       [%p mk_pat_var @@ ident_app_str s.ident "~" "out" ] =
                     fst [%e (mk_ident s.ident)], snd [%e (mk_ident s.ident)]
                   in [%e acc]]
          else acc

      ) (mk_local_signals_definitions env e) env.args_signals

  let mk_set_all_absent_definition env e =
    let open Tagged in
    (* let set_absent_expr ident = *)
    let locals_setters =
      (List.fold_left (fun acc vs ->
           [%expr set_absent ![%e mk_ident vs.signal.ident]; [%e acc]]
         ) [%expr ()] !(env.local_only_env))
    in
    let absent_setters =
      let events_setabs s acc bind =
        let mk s e = mk_ident @@ (append_tag s e).ident in
        match bind with
        | Event (e, ev) ->
          let acc = if ev = None then acc else
              [%expr [%e mk s e].value <- [%e mk s e].default; [%e acc]]
          in [%expr set_absent [%e mk s e]; [%e acc]]
        | _ -> acc in
      List.fold_left (fun acc (s, _) ->
          try
            if s.origin = Element then acc
            else Hashtbl.find env.Tagged.binders_env s.ident.content
                 |> List.fold_left (events_setabs s) acc
          with Not_found ->
            let exp = Option.casefv s.gatherer (fun _ ->
                [%expr [%e mk_ident s.ident].value <- [%e mk_ident s.ident].default; [%e acc]]
              ) acc
            in [%expr set_absent [%e mk_ident s.ident]; [%e exp]]
        ) locals_setters env.args_signals
    in
    [%expr let set_absent () = [%e absent_setters] in [%e e]]

  let is_tagged env s =
    Tagged.(s.origin = Element || Hashtbl.mem env.binders_env s.ident.content)

  let mk_method loc str expr =
    Asttypes.(Cf.method_ {txt = str; loc}
                Public (Cfk_concrete (Fresh, expr)))

  let mk_program_object env reactfun =
    let open Tagged in
    let mk_field_setter s = mk_method s.ident.loc s.ident.content
        [%expr set_present_value [%e mk_ident s.ident]] in
    let pcstr_fields =
      List.fold_left
        (fun acc (s, _) ->
           if not @@ is_tagged env s && s.origin <> Output then
             mk_field_setter s :: acc else acc)
        [(mk_method env.pname.loc api_react_function_name
            [%expr [%e reactfun] ()])] env.args_signals
    in Exp.object_ {pcstr_self = Pat.any (); pcstr_fields}

  let mk_machine_registers_definitions env e =
    IdentMap.fold (fun k (_, insts) acc ->
        List.fold_left (fun acc (inst_int_id, args) ->
            let prog_ident, program_call = mk_machine_instantiation k inst_int_id args in
            [%expr let [%p mk_pat_var prog_ident] = ref ([%e program_call])
                   in [%e acc]]
          ) acc (List.rev insts)
      ) !(env.Tagged.machine_runs) e

  let mk_callbacks_assigns animate env e =
    let opexpr loc = mk_ident @@ Ast.mk_loc ~loc "##." in
    let mk_lhs s tag =
      [%expr [%e opexpr tag.loc] [%e mk_ident s.ident] [%e mk_ident tag]]
    in
    let step_call =
      if animate then
        [%expr [%e mk_ident @@ Ast.mk_loc animate_name] ()]
      else
        [%expr ignore @@ [%e mk_ident @@ Ast.mk_loc reactfun_name] ()]
    in
    let mk_rhs s tag =
      [%expr Dom_html.handler (
             fun ev ->
               set_present_value [%e mk_ident (append_tag s tag).ident] ev;
               [%e step_call];
               Js._true)]
    in
    let mk_assign s acc typ =
      match typ with | No_binding -> acc | Access _ -> acc
      | Event (tag, _) ->
      [%expr [%e mk_lhs s tag] := [%e mk_rhs s tag]; [%e acc]]
    in List.fold_left (
      fun acc (s, _) -> try
          Hashtbl.find env.Tagged.binders_env s.ident.content
          |> List.fold_left (mk_assign s) acc
        with Not_found -> acc
    ) e env.Tagged.args_signals

  let mk_raf_call debug reactfun_ident =
    let debug_cnt = mk_ident debug_instant_cnt_name in
    let debug_cnt_str = [%expr string_of_int ![%e debug_cnt]] in
    [%expr
      fun () ->
        if not ![%e mk_ident @@ Ast.mk_loc animated_state_var_name] then (
          [%e Debug.(
                seqif ~debug ([%expr incr [%e debug_cnt]]) (
                  seqif ~debug (print @@ str "Add to next frame..." ++ debug_cnt_str)
                  [%expr [%e mk_ident @@ Ast.mk_loc animated_state_var_name] := true]))];
          let _ = Dom_html.window##requestAnimationFrame
              (Js.wrap_callback (
                  fun _ -> [%e Debug.(
                         seqif ~debug
                           (print (str "Frame start..." ++ debug_cnt_str))
                           [%expr ignore @@ [%e reactfun_ident] ()])
                  ]; [%e Debug.(seqif ~debug
                           (print @@ str "Frame done..." ++ debug_cnt_str)
                           [%expr [%e mk_ident @@ Ast.mk_loc animated_state_var_name] := false])]
                ))
          in ()) else
          [%e Debug.(seqif ~debug (print (str "Already in next frame...")) [%expr ()])]
    ]

  let mk_animate_mutex animate body =
    if not animate then body else
      [%expr let [%p mk_pat_var @@ Ast.mk_loc animated_state_var_name] = ref false in [%e body]]

  let mk_running_env debug nstmts body =
    let body = [%expr
      let [%p Pat.var select_env_var] =
        Bitset.make [%e int_const (1 + nstmts)]
      in [%e body ]]
    in
    if debug then
      [%expr let [%p mk_pat_var debug_instant_cnt_name] =
               ref 0 in [%e body]]
    else body

  let mk_constructor_reactfun env animate d body =
    let reactfun = [%expr
      fun () -> try [%e body] with
        | Pause_exc -> set_absent (); Pause
        | Finish_exc -> set_absent ();
          Bitset.add [%e select_env_ident] 0; Finish] in
    let reactfun_ident = mk_loc reactfun_name in
    let reactfun_expr = mk_ident reactfun_ident in
    let recparam, anim, id =
      if animate then
        let animate_ident = mk_loc animate_name in
        Asttypes.Recursive
      , [Vb.mk (mk_pat_var animate_ident) (mk_raf_call d reactfun_expr)]
      , mk_ident animate_ident
      else Asttypes.Nonrecursive, [], reactfun_expr
    in
    Exp.let_ recparam (Vb.mk (mk_pat_var reactfun_ident) reactfun :: anim), id


  let mk_constructor_create_fun env =
    let open Tagged in
    let inputs, outputs =
      List.partition (fun (x, _) -> x.origin <> Output) env.args_signals in
    let mk_notbind mk mknb s = if is_tagged env s then mk s.ident else mknb s.ident in
    let mk_createfun_args_pat =
      build_tuple Pat.tuple (fun ?t s -> mk_pat_var ?t s.ident) [%pat? ()] in
    let createfun_inputs_pat = mk_createfun_args_pat inputs in
    let createfun_outputs_pat = mk_createfun_args_pat outputs in
    let mk_createfun_run_args_pat =
      build_tuple Pat.tuple
        (fun ?t -> mk_notbind mk_pat_var
            (mk_pat_var ?t:(Option.map signaltype_of_type t))) [%pat? ()] in
    let createfun_run_inputs_pat = mk_createfun_run_args_pat inputs in
    let createfun_run_outputs_pat = mk_createfun_run_args_pat outputs in
    let mk_createfun_outputs_expr =
      build_tuple Exp.tuple (
        fun ?t s -> mk_notbind mk_ident (fun _ ->
            [%expr [%e signal_to_creation_expr
                         [%expr fst [%e (mk_ident s.ident)]] s],
                   snd [%e (mk_ident s.ident)]]) s
      ) [%expr ()]
    in
    let mk_createfun_inputs_expr =
      build_tuple Exp.tuple (
        fun ?t s -> mk_notbind mk_ident (fun _ ->
            signal_to_creation_expr (mk_ident s.ident) s) s
      ) [%expr ()]
    in
    let createfun_expr, createfun_run_expr =
      if inputs <> [] then
        let ins = mk_createfun_inputs_expr inputs in
        if outputs <> [] then
          let outs = mk_createfun_outputs_expr outputs in
          [%expr fun [%p createfun_inputs_pat] [%p createfun_outputs_pat]
                   -> create_local [%e ins] [%e outs]],
          [%expr create_local]
        else
          [%expr fun [%p createfun_inputs_pat] -> create_local [%e ins] ()],
          [%expr fun ins -> create_local ins ()]
      else if outputs <> [] then
        let outs = mk_createfun_outputs_expr outputs in
        [%expr fun [%p createfun_outputs_pat] -> create_local () [%e outs]],
        [%expr fun outs -> create_local]
      else
        [%expr create_local () ()], [%expr create_local () ()] in

    createfun_run_inputs_pat, createfun_run_outputs_pat,
    createfun_expr, createfun_run_expr


  let mk_constructor options nstmts env sel reactfun_body =
    let open Tagged in
    let animate = StringSet.mem "animate" options in
    let d = StringSet.mem "debug" options in
    let mk_reactfun_let, reactfun_ident =
      mk_constructor_reactfun env animate d reactfun_body
    in
    let createfun_run_inputs_pat,
        createfun_run_outputs_pat,
        createfun_expr,
        createfun_run_expr
      = mk_constructor_create_fun env
    in
    [%expr
      let open Pendulum.Runtime_misc in
      let open Pendulum.Program in
      let open Pendulum.Signal in
      let create_local
          [%p createfun_run_inputs_pat]
          [%p createfun_run_outputs_pat] = [%e
        mk_running_env d nstmts
        @@ mk_args_signals_definitions env
        @@ mk_set_all_absent_definition env
        @@ mk_machine_registers_definitions env
        @@ mk_animate_mutex animate
        @@ mk_reactfun_let
        @@ mk_callbacks_assigns animate env
        @@ mk_program_object env reactfun_ident
      ] in
      object
        method create = [%e createfun_expr]
        method create_run = [%e createfun_run_expr]
      end

    ]


  let rec mk_test env depl test =
    match test with
    | MLsig s -> [%expr !?[%e add_deref_local s]]
    | MLselect i -> [%expr Bitset.mem [%e select_env_ident] [%e int_const i]]
    | MLor (mlte1, mlte2) ->
      [%expr [%e mk_test env depl mlte1 ] || [%e mk_test env depl mlte2]]
    | MLand (mlte1, mlte2) ->
      [%expr [%e mk_test env depl mlte1 ] && [%e mk_test env depl mlte2]]
    | MLboolexpr pexpr ->
      rebind_locals_let pexpr.locals pexpr.exp
    | MLfinished -> [%expr Bitset.mem [%e select_env_ident] 0]
    | MLis_pause (MLcall (id, args, loc)) ->
      let step_ident = {id with content = Format.sprintf "%s~step" id.content} in
      let step_eq_pause = [%expr ![%e mk_ident step_ident]#react == Pause] in
      step_eq_pause
    | MLis_pause e -> assert false

  and mk_sequence env depl mlseq =
    match mlseq with
    | Seq (Seqlist [], Seqlist []) | Seqlist [] -> assert false
    | Seq (mlseq, Seqlist []) | Seq (Seqlist [], mlseq) ->
      mk_sequence env depl mlseq
    | Seqlist ml_asts ->
      List.fold_left (fun acc x ->
          if acc = dumb then mk_ml_ast env depl x
          else Exp.sequence acc (mk_ml_ast env depl x)
        ) dumb ml_asts
    | Seq (mlseq1, mlseq2) ->
      Exp.sequence (mk_sequence env depl mlseq1)
        (mk_sequence env depl mlseq2)

  and mk_ml_ast env depl ast =
    match ast with
    | MLemit vs ->
      let rebinded_expr = rebind_locals_let vs.svalue.locals vs.svalue.exp in
      begin match vs.signal.bind with
        | No_binding | Event _ ->
          [%expr set_present_value [%e add_deref_local vs.signal]
                   [%e rebinded_expr]]
        | Access (elt, fields) ->
          let lhs = List.fold_left (fun acc field ->
              [%expr [%e acc] ##. [%e mk_ident field]]
            ) (mk_ident elt) fields
          in
          [%expr [%e lhs] := [%e rebinded_expr]]
      end
    | MLif (test, mlseq1, mlseq2) ->
      begin match mlseq1, mlseq2 with
        | mlseq1, (Seqlist [] | Seq (Seqlist [], Seqlist [])) ->
          [%expr if [%e mk_test env depl test]
                 then [%e mk_sequence env depl mlseq1]]
        | (Seqlist [] | Seq (Seqlist [], Seqlist [])), mseq2 ->
          [%expr if not [%e mk_test env depl test]
                 then [%e mk_sequence env depl mlseq2]]
        | _ ->
          [%expr if [%e mk_test env depl test] then
                   [%e mk_sequence env depl mlseq1]
                 else [%e mk_sequence env depl mlseq2]]
      end

    | MLassign_machine (inst_int_id, (machine_ident, sigs, loc)) ->
      let prog_ident, machine_call = mk_machine_instantiation machine_ident inst_int_id sigs in
      [%expr [%e mk_ident prog_ident] := [%e machine_call]]

    | MLassign_signal (ident, mlast) ->
      let ocamlexpr = mk_ml_ast env depl mlast in
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
      let tuple = match sigs with [] -> assert false
        | [s] -> [%expr [%e handle_param s].value]
        | l -> Ast_helper.Exp.tuple ~loc @@
          List.map (fun s -> [%expr [%e handle_param s].value]) l
      in [%expr [%e mk_ident ident] [%e tuple]]


end


let generate pname options env tast =
  let t0 = Sys.time () in

  let selection_tree, flowgraph as grc = Of_ast.construct env options tast in
  let t_cons = Sys.time () -. t0 in

  Schedule.tag_tested_stmts selection_tree flowgraph;

  let _deps = Schedule.check_causality_cycles grc in
  let t_check = Sys.time () -. t_cons in

  let interleaved_cfg = Schedule.interleave env flowgraph in
  let t_inter = Sys.time () -. t_check in
  let maxid, deps = deplist selection_tree in
  let dep_array = Array.make (maxid + 1) [] in
  let ml_ast = grc2ml dep_array interleaved_cfg in
  let t_ml = Sys.time () -. t_inter in

  if StringSet.mem "stats" options then Schedule.Stats.(
      Format.printf "======> %s\nfg:\t%a\nfg_sched:\t%a\n"
        pname pp flowgraph pp interleaved_cfg
      ; Format.printf "time: cons(%f); check (%f); inter(%f); ml(%f)\n"
          t_cons t_check t_inter t_ml
      ; Format.printf "<======\n"

    );

  let ml_ast' =
    if not @@ StringSet.mem "nooptim" options then
      ML_optimize.gather_enter_exits ml_ast maxid
      |> ML_optimize.rm_useless_let_bindings
    else ml_ast
  in
  Ocaml_gen.(
    mk_constructor options maxid env selection_tree
    @@ mk_sequence env dep_array ml_ast'
  )
