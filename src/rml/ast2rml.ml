

module Ast = Ml2ocaml.Ast
open Ast


open Utils

open Ast_helper
open Parsetree

type error = Rml_undefined
exception Error of error

let print_error fmt = function
  | Rml_undefined -> Format.fprintf fmt "undefined in RML backend"


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


let rml_mk_args_signals_definitions env = assert false
let rml_mk_constructor_reactfun env animate d reactfun_body = assert false
let rml_mk_callbacks_assigns animate env = assert false

let rml_mk_program_object env reactfun =
  let open Tagged in
  let open Ml2ocaml in
  let mk_field_setter s = mk_method s.ident.loc s.ident.content
      [%expr set_present_value [%e mk_ident s.ident]] in
  let pcstr_fields =
    List.fold_left
      (fun acc (s, _) ->
         match s.origin with
         | Input when not @@ is_tagged env s -> mk_field_setter s :: acc
         | React ->
           (mk_method s.ident.loc s.ident.content
            @@ (mk_ident @@ ident_app_str s.ident api_React_output_signal)
           ) :: acc
         | Input | Local | Output | Element -> acc
      ) [(mk_method env.pname.loc api_react_function_name
          [%expr [%e reactfun] ()])] env.args_signals
  in Exp.object_ {pcstr_self = Pat.any (); pcstr_fields}




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


let mk_constructor options nstmts env reactfun_body =
  let open Tagged in
  let animate = StringSet.mem "animate" options in
  let d = StringSet.mem "debug" options in
  let rml_mk_reactfun_let, reactfun_ident =
    rml_mk_constructor_reactfun env animate d reactfun_body
  in
  let createfun_run_inputs_pat,
      createfun_expr
    = mk_constructor_create_fun env
  in
  [%expr
    let open Pendulum.Runtime_misc in
    let open Pendulum.Program in
    let open Pendulum.Signal in
    let create_local [%p createfun_run_inputs_pat] =
      [%e
        rml_mk_args_signals_definitions env
        @@ Ml2ocaml.mk_animate_mutex animate
        @@ rml_mk_reactfun_let
        @@ rml_mk_callbacks_assigns animate env
        @@ rml_mk_program_object env reactfun_ident
      ] in
    object
      method create = [%e createfun_expr]
    end

  ]





