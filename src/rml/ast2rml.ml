open Pendulum_compiler

module Ast = Ml2ocaml.Ast
open Ast

open Utils

open Ast_helper
open Parsetree

type error = Rml_undefined
exception Error of error

let print_error fmt = function
  | Rml_undefined -> Format.fprintf fmt "undefined in RML backend"

open Gen_utils
open Gen_names

let rec compile ast =
  let open Ast.Tagged in
  let open Ml2ocaml in
  match ast.st.content with
  | Loop t -> [%expr rml_loop [%e compile t]]
  | Seq (t1, t2) -> [%expr rml_seq [%e compile t1] [%e compile t2]]
  | Par (t1, t2) -> [%expr rml_par [%e compile t1] [%e compile t2]]

  | Emit vs ->
    let signal = mk_ident vs.signal.ident in
    let value = vs.svalue.exp in
    [%expr rml_emit_val [%e signal]
        (fun () -> [%e value])]

  | Nothing -> [%expr rml_nothing]
  | Pause -> [%expr rml_nothing]

  | Suspend (t, (s, _)) ->
    let signal = mk_ident s.ident in
    [%expr rml_control' [%e signal] [%e compile t]]

  | Trap (Label label, t) ->
    [%expr
      rml_signal (fun [%p mk_pat_var label] ->
          rml_until [%e mk_ident label]
            [%e compile t]
        )]

  | Exit (Label label) ->
    let signal = mk_ident label in
    [%expr rml_emit [%e signal]]

  | Present ((s, _), t1, t2) ->
    let signal = mk_ident s.ident in
    [%expr rml_present [%e signal]
        [%expr compile t1]
        [%expr compile t2]]

  | Atom atom -> [%expr rml_compute (fun () -> [%e atom.exp]; ())]

  | Signal (vs, t) ->
    [%expr rml_signal (fun [%p mk_pat_var vs.signal.ident] -> [%e compile t])]

  | Await (s, _) ->
    let signal = mk_ident s.ident in
    [%expr rml_await [%e signal]]

  | Run (ident, params, loc) -> raise (Error (Rml_undefined))

let signal_to_creation_expr init_val s =
  match s.bind with
  | Event (e, gopt) -> assert false
  | _ ->
    Option.casefv s.gatherer (fun g -> assert false)
      [%expr Lco_ctrl_tree_record.rml_global_signal_combine
          create [%e init_val] (fun acc x -> x)]


let mk_args_signals_definitions env e =
  let open Tagged in
  let open Ml2ocaml in
  List.fold_left (fun acc (s, _) ->
      try
        Hashtbl.find env.binders_env s.ident.content
        |> MList.map_filter has_tobe_defined (function
            | Event (e, gatherer) as bind -> { (append_tag s e) with gatherer; bind}
            | _ -> s)
        |> List.fold_left (fun acc s ->
            signal_to_definition (
              signal_to_creation_expr [%expr None] s
            ) acc s
          ) acc
      with Not_found -> acc
    ) e env.args_signals


let mk_constructor_reactfun env animate d body =
  let open Ml2ocaml in
  let reactfun = [%expr
    Lco_ctrl_tree_record.rml_make [%e body]
  ] in
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

let mk_program_object env reactfun =
  let open Tagged in
  let open Ml2ocaml in
  let mk_field_setter s = mk_method s.ident.loc s.ident.content
      [%expr Sig_env.Record.emit [%e mk_ident s.ident] ] in
  let pcstr_fields = pcstr_fields mk_field_setter env reactfun in
  Exp.object_ {pcstr_self = Pat.any (); pcstr_fields}

let mk_callbacks_assigns animate env e =
  let open Ml2ocaml in
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

let mk_createfun_inputs_expr env =
  let open Ml2ocaml in
  build_tuple Exp.tuple (
    fun ?t s -> mk_notbind env mk_ident (fun _ ->
        signal_to_creation_expr (mk_ident s.ident) s) s
  ) [%expr ()]

let mk_constructor_create_fun env =
  let open Tagged in
  let open Ml2ocaml in
  let inputs, outputs =
    List.partition (fun (x, _) -> is_input x) env.args_signals in
  let mk_notbind mk mknb s = mk_notbind env mk mknb s in
  let mk_createfun_args_pat =
    build_tuple Pat.tuple (fun ?t s -> mk_pat_var ?t s.ident) [%pat? ()] in
  let createfun_inputs_pat = mk_createfun_args_pat inputs in
  let mk_createfun_run_args_pat =
    build_tuple Pat.tuple
      (fun ?t -> mk_notbind mk_pat_var
          (mk_pat_var ?t:(Option.map signaltype_of_type t))) [%pat? ()] in
  let createfun_run_inputs_pat = mk_createfun_run_args_pat inputs in
  let createfun_expr =
    if inputs <> [] then
      let ins = mk_createfun_inputs_expr env inputs in
      [%expr fun [%p createfun_inputs_pat] -> create_local [%e ins] ()]
    else
      [%expr create_local ()] in
  createfun_run_inputs_pat, createfun_expr


let mk_constructor options _ env reactfun_body =
  let open Tagged in
  let animate = StringSet.mem "animate" options in
  let d = StringSet.mem "debug" options in
  let mk_reactfun_let, reactfun_ident =
    mk_constructor_reactfun env animate d reactfun_body
  in
  let createfun_run_inputs_pat,
      createfun_expr
    = mk_constructor_create_fun env
  in
  [%expr
    let create_local [%p createfun_run_inputs_pat] =
      [%e
        mk_args_signals_definitions env
        @@ Ml2ocaml.mk_animate_mutex animate
        @@ mk_reactfun_let
        @@ mk_callbacks_assigns animate env
        @@ mk_program_object env reactfun_ident
      ] in
    object
      method create = [%e createfun_expr]
    end

  ]

let generate _ options env tast =
  let reactfun_ocaml = compile tast in
  mk_constructor options 0 env reactfun_ocaml





