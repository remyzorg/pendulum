open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Pendulum_compiler

open Utils

module Ast = Sync2ml.Ast


let check_expr_ident e =
  let open Error in
  match e.pexp_desc with
  | Pexp_ident {txt = Lident content; loc} -> Ast.{loc; content}
  | _ -> syntax_error ~loc:e.pexp_loc Signal_name

let check_pat_ident e =
  let open Error in
  match e.ppat_desc with
  | Ppat_var {txt = content; loc} -> Ast.{loc; content}
  | _ -> syntax_error ~loc:e.ppat_loc Signal_name

let check_run_param e =
  let open Ast in
  let open Error in
  match e with
  | {pexp_desc = Pexp_ident {txt = Lident content; loc}} -> Ast.(Sig_param {loc; content})
  | [%expr ![%e? e]] -> Exp_param e
  | [%expr !![%e? _]] as e -> Exp_param e
  | _ -> syntax_error ~loc:e.pexp_loc Signal_name

let signal_tuple_to_list e =
  let open Ast in
  match e with
  | [%expr ![%e? e]] | ([%expr !![%e? _]] as e) -> [Exp_param e]
  | { pexp_desc = Pexp_ident {txt = Lident content; loc }} -> [Sig_param {loc; content}]
  | { pexp_desc = Pexp_tuple exprs } -> List.map check_run_param exprs
  | _ -> Error.syntax_error ~loc:e.pexp_loc Error.Signal_tuple

let rec check_signal_presence_expr atom_mapper e =
  let open Ast in
  match e with
  | { pexp_desc = Pexp_ident {txt = Lident content; loc} } -> {loc; content}, None, None
  | [%expr [%e? sigexpr] & ([%e? boolexpr])] ->
    let ident, tag, _ = check_signal_presence_expr atom_mapper sigexpr in
    ident, tag, Some (atom_mapper boolexpr)


  | [%expr [%e? elt] ##
             [%e? {pexp_desc = Pexp_ident {txt = Lident content; loc}} ]]
  | {pexp_desc = Pexp_field (elt, {txt = Lident content; loc})} ->
    check_expr_ident elt, Some {loc; content}, None

  | [%expr [%e? elt] ## [%e? err]] ->
    Error.(syntax_error ~loc:err.pexp_loc Event_name)
  | _ -> Error.(syntax_error ~loc:e.pexp_loc Signal_test)

let rec check_signal_emit_expr acc atom_mapper e =
  let open Ast in
  let rec aux acc e =
    match e with
    | { pexp_desc = Pexp_ident {txt = Lident content; loc} } -> [{loc; content}]
    | [%expr [%e? elt] ##. [%e? field]] ->
      let elt_list = aux acc elt in
      let event_ident =
        match field with
        | {pexp_desc = Pexp_ident {txt = Lident content; loc}} -> {loc; content}
        | _ -> Error.(syntax_error ~loc:field.pexp_loc Event_name)
      in
      event_ident :: elt_list
    | _ -> Error.(syntax_error ~loc:e.pexp_loc Signal_name)
  in
  List.rev @@ aux [] e


let pop_signals_decl e =
  let open Ast in
  let mk ?gatherer origin var = mk_signal ~origin ?gatherer (check_expr_ident var) in
  let get_orig = function
    | [%expr output] -> Output
    | [%expr input] -> Input
    | [%expr element] -> Element
    | _ -> Input
  in
  let sig_of_expr ?gatherer orig = function
    | [%expr ([%e? e_var] : [%t? t])] -> mk (get_orig orig) e_var, Some t
    | e_var -> mk ?gatherer (get_orig orig) e_var, None
  in
  let rec aux p binders sigs =
    match p with
    | [%expr [%e? {pexp_desc = Pexp_tuple (
                    [%expr [%e? ([%expr input] | [%expr output]) as orig]
                             [%e? e_var]] :: e_vars)}]; [%e? e2]] ->
      aux e2 binders @@ List.fold_left (fun acc e_var' ->
          sig_of_expr orig e_var' :: acc
        ) (sig_of_expr orig e_var :: sigs) e_vars

    | [%expr [%e? ([%expr input] | [%expr output] | [%expr element]) as orig]
               [%e? e_var]; [%e? e2] ] ->
      aux e2 binders @@ sig_of_expr orig e_var :: sigs

    | [%expr [%e? ([%expr input] | [%expr output] | [%expr element]) as orig]
               [%e? e_var] [%e? {pexp_desc = Pexp_record (gatherers, None)}];
             [%e? e2] ] ->
      let binder = List.map (fun ({txt; loc}, expr) ->
          match txt with
          | Lident s -> Event (Ast.mk_loc ~loc s, Some expr)
          | _ -> Error.(syntax_error ~loc:e.pexp_loc Event_name)
        ) gatherers
      in
      let s, _ as st = sig_of_expr orig e_var in
      aux e2 ((s.ident.content, binder) :: binders) @@ st :: sigs

    | [%expr [%e? ([%expr input] | [%expr output] | [%expr element]) as orig]
               [%e? e_var] [%e? gatherer]; [%e? e2] ] ->
      let st = sig_of_expr ~gatherer orig e_var in
      aux e2 binders @@ st :: sigs
    | e -> e, binders, sigs
  in aux e [] []

let ast_of_expr atom_mapper e =
  let rec visit e =
    let open Ast in
    let open Ast.Derived in
    mk_loc ~loc:e.pexp_loc @@ match e with
    | [%expr output [%e? _ ] ; [%e? _]]
    | [%expr input [%e? _ ] ; [%e? _]] ->
      Error.(syntax_error ~loc:e.pexp_loc Signal_decl_at_start)

    | [%expr nothing] -> Nothing
    | [%expr pause] -> Pause

    | [%expr emit [%e? signal] [%e? e_value]] ->
      let signal, fields =
        match check_signal_emit_expr [] atom_mapper signal
        with | [] -> assert false | hd :: tl -> hd, tl
      in
      Emit (Ast.mk_vid ~fields signal @@ atom_mapper e_value)

    | [%expr emit [%e? signal]] ->
      Emit (Ast.mk_vid (check_expr_ident signal) [%expr ()])

    | [%expr exit [%e? label]] ->
      Exit (Label(check_expr_ident label))

    | [%expr atom [%e? e]] ->
      Atom (atom_mapper e)

    | [%expr !([%e? e])] ->
      Atom (atom_mapper e)

    | [%expr loop [%e? e]] ->
      Loop (visit e)

    | [%expr [%e? e1]; [%e? e2]] ->
      let e1' = visit e1 in
      let e2' = visit e2 in
      Seq (e1', e2')

    | [%expr [%e? e1] || [%e? e2]] ->
      let e1' = visit e1 in
      let e2' = visit e2 in
      Par (e1', e2')

    | [%expr present [%e? signal] [%e? e1] [%e? e2]] ->
      let e1' = visit e1 in
      let e2' = visit e2 in
      Present (check_signal_presence_expr atom_mapper signal, e1', e2')

    | [%expr let [%p? signal] = [%e? e_value] in [%e? e]] ->
      let vid = Ast.mk_vid (check_pat_ident signal) @@ atom_mapper e_value in
      let e' = visit e in
      Signal (vid, e')

    | [%expr signal [%e? signal] [%e? e_value] [%e? e]] ->
      let vid = Ast.mk_vid (check_expr_ident signal) @@ atom_mapper e_value in
      let e' = visit e in
      Signal (vid, e')

    | [%expr signal [%e? signal] [%e? e]] ->
      let vid = Ast.mk_vid (check_expr_ident signal) @@ atom_mapper [%expr ()] in
      let e' = visit e in
      Signal (vid, e')

    | [%expr suspend [%e? e] [%e? signal]] ->
      Suspend (visit e, check_signal_presence_expr atom_mapper signal)

    | [%expr trap [%e? label] [%e? e]] ->
      Trap (Label (check_expr_ident label), visit e)

    | [%expr run [%e? ident] [%e? tupl]] ->
      Run (check_expr_ident ident, signal_tuple_to_list tupl, tupl.pexp_loc)

    | [%expr halt ] ->
      Halt

    | [%expr sustain [%e? signal] [%e? e_value]] ->
      Sustain (Ast.mk_vid (check_expr_ident signal) e_value)

    | [%expr sustain [%e? signal]] ->
      Error.signal_value_missing e (check_expr_ident signal).content

    | [%expr present [%e? signal] [%e? e]] ->
      Present_then
        (check_signal_presence_expr atom_mapper signal, visit e)

    | [%expr await [%e? signal]] ->
      Await (check_signal_presence_expr atom_mapper signal)

    | [%expr abort [%e? e] [%e? signal]] ->
      Abort (visit e, check_signal_presence_expr atom_mapper signal)

    | [%expr loopeach [%e? e] [%e? signal]] ->
      Loop_each (visit e, check_signal_presence_expr atom_mapper signal)

    | [%expr every [%e? e] [%e? signal]] ->
      Every (check_signal_presence_expr atom_mapper signal, visit e)

    | [%expr nothing [%e? e_err]]
    | [%expr present [%e? _] [%e? _] [%e? e_err]]
    | [%expr await [%e? _][%e? e_err]]
    | [%expr abort [%e? _] [%e? _] [%e? e_err]]
    | [%expr loopeach [%e? _] [%e? _][%e? e_err]]
    | [%expr every [%e? _] [%e? _] [%e? e_err]]
    | [%expr input [%e? _ ] [%e? _] [%e? e_err]]
    | [%expr output [%e? _ ] [%e? _] [%e? e_err]] ->
      Error.(syntax_error ~loc:e_err.pexp_loc Forgot_sem)

    | e -> Error.(syntax_error ~loc:e.pexp_loc Keyword)
  in visit e



let compiler_options =
  [ "animate"
  ; "dsource"
  ; "ast"
  ; "print"
  ; "debug"
  ; "nooptim"
  ; "obj"
  ; "stats"
  ]

let rec parse_args options inputs exp =
  let addopt opt = StringSet.add opt options in
  match exp with

  | {pexp_desc = Pexp_fun (Labelled s, None, _, exp')}
    when s <> "" && List.mem s compiler_options ->
    parse_args (addopt s) inputs exp'

  | [%expr fun ~print:[%p? pp_params] -> [%e? exp']] as prt_param ->
    let check_param opts = function
      | {ppat_desc = Ppat_var {txt = content; loc}} ->
        StringSet.add (match content with
            | "pdf" | "png" | "dot" -> content
            | _ -> Error.(error ~loc:prt_param.pexp_loc Wrong_argument_values)
          ) opts
      | _ -> Error.(error ~loc:prt_param.pexp_loc Wrong_argument_values)
    in
    let opts = match pp_params with
      | {ppat_desc = Ppat_var _} -> check_param options pp_params
      | {ppat_desc = Ppat_tuple l} -> List.fold_left check_param options l
      | _ -> Error.(error ~loc:prt_param.pexp_loc Wrong_argument_values)
    in
    parse_args opts inputs exp'

  | [%expr fun [%p? {ppat_desc = Ppat_var ident}] -> [%e? exp']] ->
    parse_args options (Ast.({content = ident.txt; loc = ident.loc}, None) :: inputs) exp'

  | [%expr fun ([%p? {ppat_desc = Ppat_var ident}] : [%t? typ]) -> [%e? exp']] ->
    parse_args options (Ast.({content = ident.txt; loc = ident.loc}, Some typ) :: inputs) exp'

  | { pexp_desc = Pexp_fun (Labelled s, _, _, _) } when s <> "" ->
    Error.(syntax_error ~loc:exp.pexp_loc (Unknown_arg_option s))

  | { pexp_desc = Pexp_fun _ } ->
    Error.(syntax_error ~loc:exp.pexp_loc Argument_syntax_error)

  | e -> e, options,inputs
