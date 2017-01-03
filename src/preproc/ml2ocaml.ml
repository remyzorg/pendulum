(* generating the ocaml code from ast *)

module Expression = Grc2ml.Expression
module Ast = Grc2ml.Ast
module Flowgraph = Grc2ml.Flowgraph
module Selection_tree = Grc2ml.Selection_tree
module Schedule = Grc2ml.Schedule
module Of_ast = Grc2ml.Of_ast

open Ast_helper
open Parsetree
open Utils
open Ast

open Gen_names
open Gen_utils

let expr_of_bitset bs =
  bs |> Array.to_list
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

let mk_mach_inst_step mch =
  {mch with content = Format.sprintf "%s~step" mch.content}

let mk_mach_inst_ident mch k =
  {mch with content = Format.sprintf "%s%s" mch.content
                (if k != 0 then Format.sprintf "~%d" k else "")
  }


let mk_set_mach_arg_n n mch = Ast.mk_loc ~loc:Ast.dummy_loc
    (Format.sprintf "%s~set~arg~%d" mch n)

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
    let atom = Grc2ml.ML_optimize.rm_atom_deps vs.svalue in
    let rebinds = rebind_locals_let atom.locals
        [%expr ref (make_signal [%e vs.svalue.exp])]
    in
    [%expr let [%p mk_pat_var vs.signal.ident] = [%e rebinds] in [%e acc]]
  ) e !(env.Tagged.local_only_env)

let signal_to_creation_expr init_val s =
  match s.bind with
  | Event (e, gopt) ->
    Option.casefv gopt (fun g ->
        [%expr make_signal_gather ([%e g] :> _ * ( _ -> #Dom_html.event Js.t -> _ ))]
      ) [%expr make_event_signal [%e init_val]]
  | _ ->
    Option.casefv s.gatherer (fun g ->
        [%expr make_signal_gather ([%e init_val],[%e g])]
          [@metaloc s.ident.loc]
      ) [%expr make_signal [%e init_val]]

let signal_to_definition rhs next_def s =
  [%expr let [%p mk_pat_var s.ident] = [%e rhs] in [%e next_def]]


let mk_args_signals_definitions env e =
  let open Tagged in
  List.fold_left (fun acc (s, _) ->
      try
        Hashtbl.find env.binders_env s.ident.content
        |> MList.map_filter has_to_be_defined (function
            | Event (e, gatherer) as bind -> { (append_tag s e) with gatherer; bind}
            | _ -> s)
        |> List.fold_left (fun acc s ->
            signal_to_definition (
              signal_to_creation_expr [%expr None] s
            ) acc s
          ) acc
      with Not_found ->
        begin match s.origin with
          | Output ->
            [%expr let [%p mk_pat_var s.ident],
                       [%p mk_pat_var @@ ident_app_str
                           s.ident api_output_signal_callback ] =
                     fst [%e (mk_ident s.ident)], snd [%e (mk_ident s.ident)]
              in [%e acc]]
          | React ->
            [%expr let [%p mk_pat_var s.ident],
                       [%p mk_pat_var @@ ident_app_str s.ident api_React_output_signal],
                       [%p mk_pat_var @@ ident_app_str
                           s.ident api_output_signal_callback ] =
                     fst [%e (mk_ident s.ident)],
                     fst (snd [%e (mk_ident s.ident)]),
                     snd (snd [%e (mk_ident s.ident)]) ?step:None
              in [%e acc]]
          | Local | Input | Element -> acc
        end
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


let mk_method loc str expr =
  Asttypes.(Cf.method_ {txt = str; loc}
              Public (Cfk_concrete (Fresh, expr)))

let pcstr_fields mk_field env reactfun =
  let open Tagged in
  List.fold_left
    (fun acc (s, _) ->
       match s.origin with
       | Input when not @@ is_tagged env s -> mk_field s :: acc
       | React ->
         (mk_method s.ident.loc s.ident.content
          @@ (mk_ident @@ ident_app_str s.ident api_React_output_signal)
         ) :: acc
       | Input | Local | Output | Element -> acc
    ) [(mk_method env.pname.loc api_react_function_name
          [%expr [%e reactfun] ()])] env.args_signals

let mk_program_object env reactfun =
  let mk_field_setter s = mk_method s.ident.loc s.ident.content
      [%expr set_present_value [%e mk_ident s.ident]] in
  let pcstr_fields = pcstr_fields mk_field_setter env reactfun
  in Exp.object_ {pcstr_self = Pat.any (); pcstr_fields}


let mk_machine_registers_definitions env e =
  IdentMap.fold (fun k (_, insts) acc ->
      List.fold_left (fun acc (inst_int_id, args) ->
          let prog_ident, program_call = mk_machine_instantiation k inst_int_id args in
          [%expr let [%p mk_pat_var prog_ident] = ref ([%e program_call])
            in [%e acc]]
        ) acc (List.rev insts)
    ) !(env.Tagged.machine_runs) e

let mk_callbacks_assigns_rhs step_call s tag =
  [%expr Dom_html.handler (
      fun ev ->
        set_present_value [%e mk_ident (append_tag s tag).ident] ev;
        [%e step_call];
        Js._true)]

let mk_callbacks_assigns mk_rhs animate env e =
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
  let mk_rhs = mk_rhs step_call in
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
  let debug_cnt = mk_ident @@ Ast.mk_loc debug_instant_cnt_name in
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

let mk_callbacks_assigns = mk_callbacks_assigns mk_callbacks_assigns_rhs

let mk_animate_mutex animate body =
  if not animate then body else
    [%expr let [%p mk_pat_var @@ Ast.mk_loc animated_state_var_name] = ref false in [%e body]]

let select_env_var = Location.(mkloc select_env_name !Ast_helper.default_loc)
let select_env_ident = mk_ident (Ast.mk_loc select_env_name)

let mk_running_env debug nstmts body =
  let body = [%expr
    let [%p Pat.var select_env_var] =
      Bitset.make [%e int_const (1 + nstmts)]
    in [%e body ]]
  in
  if debug then
    [%expr let [%p mk_pat_var @@ Ast.mk_loc debug_instant_cnt_name] =
             ref 0 in [%e body]]
  else body

let mk_constructor_reactfun env animate d body =
  let reactfun = [%expr
    fun () -> try [%e body] with
      | Pause_exc -> set_absent (); Pause
      | Finish_exc -> set_absent ();
        Bitset.add [%e select_env_ident] 0; Finish] in
  let reactfun_ident = mk_loc reactfun_name in
  let reactfun_var_expr = mk_ident reactfun_ident in
  let recparam, anim, reactfun_var =
    if animate then
      let animate_ident = mk_loc animate_name in
      Asttypes.Recursive
    , [Vb.mk (mk_pat_var animate_ident) (mk_raf_call d reactfun_var_expr)]
    , mk_ident animate_ident
    else Asttypes.Nonrecursive, [], reactfun_var_expr
  in
  Exp.let_ recparam (Vb.mk (mk_pat_var reactfun_ident) reactfun :: anim), reactfun_var

let mk_createfun_outputs_expr env =
  build_tuple Exp.tuple (
    fun ?t s ->
      let initexpr, output =
        match s.origin with
        | Output ->
          [%expr fst [%e (mk_ident s.ident)]],
          [%expr snd [%e (mk_ident s.ident)]]
        | React ->
          (mk_ident s.ident),
          [%expr React.S.create [%e (mk_ident s.ident)]][@metaloc s.ident.loc]
        | _ -> assert false
      in
      mk_notbind env mk_ident (fun _ ->
          [%expr [%e signal_to_creation_expr initexpr s], [%e output ]]) s
  ) [%expr ()]

let mk_createfun_inputs_expr env =
  build_tuple Exp.tuple (
    fun ?t s -> mk_notbind env mk_ident (fun _ ->
        signal_to_creation_expr (mk_ident s.ident) s) s
  ) [%expr ()]

let mk_constructor_create_fun env =
  let open Tagged in
  let inputs, outputs =
    List.partition (fun (x, _) -> is_input x) env.args_signals in
  let mk_notbind mk mknb s = mk_notbind env mk mknb s in
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
  let createfun_expr, createfun_run_expr =
    let ins = mk_createfun_inputs_expr env inputs in
    let outs = mk_createfun_outputs_expr env outputs in
    match inputs, outputs with
    | [], [] ->
      [%expr create_local () ()], [%expr create_local () ()]
    | inputs, [] ->
      [%expr fun [%p createfun_inputs_pat] -> create_local [%e ins] ()],
      [%expr fun ins -> create_local ins ()]
    | [], outputs ->
      [%expr fun [%p createfun_outputs_pat] -> create_local () [%e outs]],
      [%expr fun outs -> create_local]
    | inputs, outputs ->
      [%expr fun [%p createfun_inputs_pat] [%p createfun_outputs_pat]
        -> create_local [%e ins] [%e outs]], [%expr create_local]
  in createfun_run_inputs_pat, createfun_run_outputs_pat,
     createfun_expr, createfun_run_expr


let mk_constructor options nstmts env reactfun_body =
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
    let create_local [%p createfun_run_inputs_pat]
        [%p createfun_run_outputs_pat] =
      [%e
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
  let open Grc2ml in
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
  let open Grc2ml in
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
  let open Grc2ml in
  match ast with
  | MLemit vs ->
    let rebinded_expr = rebind_locals_let vs.svalue.locals vs.svalue.exp in
    begin match vs.signal.bind with
      | No_binding | Event _ ->
        let setval_expr = [%expr set_present_value [%e add_deref_local vs.signal]
            [%e rebinded_expr]]
        in begin match vs.signal.origin with
          | Output | React ->
            Exp.sequence setval_expr
              ([%expr [%e mk_ident @@ ident_app_str vs.signal.ident
                  api_output_signal_callback]
                  [%e mk_ident vs.signal.ident].value])
          | Local | Input | Element -> setval_expr
        end

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


let generate pname options env tast =
  let t0 = Sys.time () in

  let selection_tree, flowgraph as grc = Of_ast.construct env options tast in
  let t_cons = Sys.time () -. t0 in

  Schedule.tag_tested_stmts selection_tree flowgraph;

  let _deps = Schedule.check_causality_cycles grc in
  let t_check = Sys.time () -. t_cons in

  let interleaved_cfg = Schedule.interleave env flowgraph in
  let t_inter = Sys.time () -. t_check in
  let maxid, deps = Grc2ml.deplist selection_tree in
  let dep_array = Array.make (maxid + 1) [] in
  let ml_ast = Grc2ml.grc2ml dep_array interleaved_cfg in
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
      Grc2ml.ML_optimize.gather_enter_exits ml_ast maxid
      |> Grc2ml.ML_optimize.rm_useless_let_bindings
    else ml_ast
  in
  mk_constructor options maxid env @@ mk_sequence env dep_array ml_ast'

