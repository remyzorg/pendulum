
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Preproc

open Utils

module Ast = Sync2ml.Ast

module Error = struct

  type syntax =
    | Forgot_sem
    | Keyword
    | Signal_name
    | Signal_test
    | Signal_test_expression
    | Signal_tuple
    | Signal_decl_at_start
    | Event_name
    | Unknown_arg_option of string
    | Argument_syntax_error

  type 'a t =
    | Value_missing of string
    | Syntax of syntax
    | Non_recursive_let
    | Only_on_let
    | Wrong_argument_values
    | Other_err of 'a * (Format.formatter -> 'a -> unit)


  let print_syntax_rsn fmt rsn =
    let open Format in
    match rsn with
    | Forgot_sem -> fprintf fmt "maybe you forgot a `;`"
    | Keyword -> fprintf fmt "keyword expected"
    | Signal_name -> fprintf fmt "signal name expected"
    | Signal_test -> fprintf fmt "signal test expected"
    | Event_name -> fprintf fmt "event name expected"
    | Signal_test_expression -> fprintf fmt "signal test expression expected"
    | Signal_tuple -> fprintf fmt "signal tuple expected"
    | Signal_decl_at_start -> fprintf fmt "signal declarations must be on the top"
    | Unknown_arg_option s -> fprintf fmt "unknown option %s" s
    | Argument_syntax_error -> fprintf fmt "argument expected"


  let print_error fmt e =
    let open Format in
    fprintf fmt "[pendulum] ";
    match e with
    | Value_missing s -> fprintf fmt "Signal value of %s is missing" s
    | Syntax rsn -> fprintf fmt "Syntax error : %a" print_syntax_rsn rsn
    | Non_recursive_let -> fprintf fmt "recursive `let` not allowed"
    | Only_on_let -> fprintf fmt "only allowed on let"
    | Other_err (e, f) -> fprintf fmt "%a" f e
    | Wrong_argument_values -> fprintf fmt "unexpected value for this argument"

  let error ~loc rsn =
    raise (Location.Error (
        Location.error ~loc (Format.asprintf "%a" print_error rsn)))

  let syntax_error ~loc r = error ~loc (Syntax r)

  let signal_value_missing e s =
    error ~loc:e.pexp_loc (Value_missing s)

end


let check_expr_ident e =
  let open Ast in
  let open Error in
  match e.pexp_desc with
  | Pexp_ident {txt = Lident content; loc} -> {loc; content}
  | _ -> syntax_error ~loc:e.pexp_loc Signal_name

let check_pat_ident e =
  let open Ast in
  let open Error in
  match e.ppat_desc with
  | Ppat_var {txt = content; loc} ->
     {loc; content}
  | _ -> syntax_error ~loc:e.ppat_loc Signal_name

let signal_tuple_to_list e =
  let open Ast in
  match e.pexp_desc with
  | Pexp_ident {txt = Lident content; loc } -> [{loc; content}]
  | Pexp_tuple exprs -> List.map check_expr_ident exprs
  | _ -> Error.syntax_error ~loc:e.pexp_loc Error.Signal_tuple

let rec check_signal_presence_expr atom_mapper e =
  let open Ast in
  match e with
  | { pexp_desc = Pexp_ident {txt = Lident content; loc} } -> {loc; content}, None, None
  | [%expr [%e? sigexpr] & ([%e? boolexpr])] ->
    let ident, tag, _ = check_signal_presence_expr atom_mapper sigexpr in
    ident, tag, Some (atom_mapper boolexpr)

  | [%expr [%e? elt] ## [%e? event]] ->
     let elt_ident = check_expr_ident elt in
     let event_ident =
       begin match event with
       | {pexp_desc = Pexp_ident {txt = Lident content; loc}} -> {loc; content}
       | _ -> Error.(syntax_error ~loc:event.pexp_loc Event_name)
       end
     in
     elt_ident, Some event_ident, None
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
  let mk orig var = mk_signal ~origin:orig (check_expr_ident var) in
  let get_orig = function
    | [%expr output] -> Output | [%expr input] -> Input | _ -> Input
  in
  let get_sig orig = function
    | [%expr ([%e? e_var] : [%t? t])] -> mk (get_orig orig) e_var, Some t
    | e_var -> mk (get_orig orig) e_var, None
  in
  let rec aux p sigs =
    match p with
    | [%expr [%e? params], [%e? e2]] ->
      Error.(syntax_error ~loc:e2.pexp_loc Forgot_sem)

    | [%expr [%e? {pexp_desc = Pexp_tuple (
                    [%expr [%e? ([%expr input] | [%expr output]) as orig]
                             [%e? e_var]] :: e_vars)}]; [%e? e2]] ->
      aux e2 @@ List.fold_left (fun acc e_var' ->
          get_sig orig e_var' :: acc
        ) (get_sig orig e_var :: sigs) e_vars

    | [%expr [%e? ([%expr input] | [%expr output]) as orig] [%e? e_var]; [%e? e2] ] ->
      aux e2 @@ get_sig orig e_var :: sigs

    | [%expr input [%e? e_var] [%e? _]; [%e? e2] ]
    | [%expr output [%e? e_var] [%e? _]; [%e? e2] ] ->
      Error.(syntax_error ~loc:e_var.pexp_loc Forgot_sem)

    | e -> e, sigs
  in aux e []

let ast_of_expr atom_mapper e =
  let rec visit e =
    let open Ast in
    let open Ast.Derived in
    mk_loc ~loc:e.pexp_loc @@ match e with
    | [%expr output [%e? _ ] ; [%e? _]]
    | [%expr input [%e? _ ] ; [%e? _]] ->
      Error.(syntax_error ~loc:e.pexp_loc Signal_decl_at_start)

    | [%expr nothing] ->
      Nothing

    | [%expr pause] ->
      Pause

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

    | [%expr signal [%e? signal] [%e? _]] ->
      Error.signal_value_missing e (check_expr_ident signal).content

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

let parse_ast atom_mapper vb =
  let dsource, dot, genast = ref false, ref false, ref false in
  let animate, pdf, png = ref false, ref false, ref false in
  let nooptim = ref false in
  let rec parse_args inputs exp =
    match exp with
    | [%expr fun ~animate -> [%e? exp']] ->
      animate := true; parse_args inputs exp'
    | [%expr fun ~dsource -> [%e? exp']] ->
      dsource := true;
      parse_args inputs exp'
    | [%expr fun ~ast -> [%e? exp']] ->
      genast := true; parse_args inputs exp'

    | [%expr fun ~print -> [%e? exp']] ->
      pdf := true; parse_args inputs exp'

    | [%expr fun ~print:[%p? pp_params] -> [%e? exp']] as prt_param ->
      let check_param p = match p with
        | {ppat_desc = Ppat_var {txt = content; loc}} ->
          begin match content with
            | "pdf" -> pdf := true
            | "png" -> png := true
            | "dot" -> dot := true
            | _ -> Error.(error ~loc:prt_param.pexp_loc Wrong_argument_values)
          end
        | _ -> Error.(error ~loc:prt_param.pexp_loc Wrong_argument_values)
      in
      begin match pp_params with
      | {ppat_desc = Ppat_var _} -> check_param pp_params
      | {ppat_desc = Ppat_tuple l} -> List.iter check_param l
      | _ -> Error.(error ~loc:prt_param.pexp_loc Wrong_argument_values)
      end;
      parse_args inputs exp'

    | [%expr fun ~nooptim -> [%e? exp']] ->
      nooptim := true; parse_args inputs exp'

    | [%expr fun [%p? {ppat_desc = Ppat_var ident}] -> [%e? exp']] ->
      parse_args (Ast.({content = ident.txt; loc = ident.loc}, None) :: inputs) exp'

    | [%expr fun ([%p? {ppat_desc = Ppat_var ident}] : [%t? typ]) -> [%e? exp']] ->
      parse_args (Ast.({content = ident.txt; loc = ident.loc}, Some typ) :: inputs) exp'

    | { pexp_desc = Pexp_fun (s, _, _, _) } when s <> "" ->
      Error.(syntax_error ~loc:exp.pexp_loc (Unknown_arg_option s))

    | { pexp_desc = Pexp_fun _ } ->
      Error.(syntax_error ~loc:exp.pexp_loc Argument_syntax_error)

    | e -> e, inputs
  in
  let e, args = parse_args [] vb.pvb_expr in
  let e, inputs = pop_signals_decl e in
  let sigs = List.(
      inputs @ map Ast.(fun (s, t) -> mk_signal ~origin:Input s, t) args
    )
  in
  if !genast then
    [%expr ([%e Pendulum_misc.expr_of_ast @@ ast_of_expr atom_mapper e])]
  else
    let loc = vb.pvb_loc in
    let pat =
      match vb.pvb_pat.ppat_desc with
      | Ppat_var id -> id.txt
      | _ -> "unknown"
    in
    let ast = ast_of_expr atom_mapper e in
    let tast, env = Ast.Tagged.of_ast ~sigs ast in
    let tast = Ast.Analysis.filter_dead_trees tast in
    if !dot || !pdf || !png then Pendulum_misc.print_to_dot !dot !pdf !png loc pat tast;
    let ocaml_expr = Sync2ml.generate !nooptim !animate env tast in
    if !dsource then Format.printf "%a@." Pprintast.expression ocaml_expr;
    [%expr [%e ocaml_expr]]



let gen_bindings atom_mapper vbl =
  List.map (fun vb ->
      {vb with pvb_expr = parse_ast atom_mapper vb}
    ) vbl


let try_compile_error f mapper str =
  let open Sync2ml in
  try f mapper str with
  | Ast.Error (loc, e) ->
    Error.(error ~loc (Other_err (e, Ast.print_error)))
  | Sync2ml.Error (loc, e) ->
    Error.(error ~loc (Other_err (e, Sync2ml.print_error)))
  | Flowgraph.Error (loc, e) ->
    Error.(error ~loc (Other_err (e, Flowgraph.print_error)))
  | Location.Error _ as e -> raise e
  | e ->
    Error.(error ~loc:Ast.dummy_loc
             (Other_err (e, fun fmt e ->
                  Format.fprintf fmt "%s" (Printexc.to_string e))))

let tagged_signals_mapper =
  {default_mapper with
   expr = (fun mapper exp ->
       match exp with
       | [%expr !!([%e? {pexp_desc = Pexp_ident {txt = Lident content; loc}}]
                   ## [%e? {pexp_desc = Pexp_ident {txt = Lident tag_content}}])] ->
         let ident =
           {Ast.content = Format.sprintf "%s##%s" content tag_content; loc}
         in
         let e' =
           [%expr !![%e Sync2ml.Ocaml_gen.mk_ident ident]][@metaloc exp.pexp_loc]
         in
         mapper.expr mapper e'
       | x ->
         default_mapper.expr mapper x
     );
  }

let pendulum_mapper argv =
  let open Sync2ml in
  {default_mapper with
   structure_item = try_compile_error (fun mapper stri ->
       match stri with
       | { pstr_desc = Pstr_extension (({ txt = "sync" }, PStr [
           { pstr_desc = Pstr_value (Nonrecursive, vbs) }]), attrs); pstr_loc } ->
         Str.value Nonrecursive
         @@ gen_bindings (tagged_signals_mapper.expr tagged_signals_mapper) vbs

       | { pstr_desc = Pstr_extension (({ txt = "sync" }, PStr [
           { pstr_desc = Pstr_value (Recursive, _) }]), _); pstr_loc } ->
         Error.(error ~loc:pstr_loc Non_recursive_let)

       | x -> default_mapper.structure_item mapper x
     ) ;

   expr = try_compile_error (fun mapper exp ->
       match exp with
       | { pexp_desc = Pexp_extension ({ txt = "sync"; loc }, e)} ->
         begin match e with
           | PStr [{ pstr_desc = Pstr_eval (e, _)}] ->
             begin match e.pexp_desc with
               | Pexp_let (Nonrecursive, vbl, e) ->
                 Exp.let_ Nonrecursive
                   (gen_bindings (tagged_signals_mapper.expr tagged_signals_mapper) vbl)
                 @@ mapper.expr mapper e
               | Pexp_let (Recursive, vbl, e) ->
                 Error.(error ~loc Non_recursive_let)
               | _ ->
                 Error.(error ~loc Only_on_let)
             end
           | _ -> Error.(error ~loc Only_on_let)
         end
       | x -> default_mapper.expr mapper x;
     )
  }

let () =
  register "pendulum" pendulum_mapper
