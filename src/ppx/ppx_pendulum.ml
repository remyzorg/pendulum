
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Preproc

open Utils

module Ast = Sync2ml.Ast

module Error = struct

  type syntax = Forgot_sem | Keyword | Signal_name | Signal_tuple | Signal_decl_at_start

  type 'a t =
    | Value_missing of string
    | Syntax of syntax
    | Non_recursive_let
    | Only_on_let
    | Other_err of 'a * (Format.formatter -> 'a -> unit)


  let print_syntax_rsn fmt rsn =
    let open Format in
    match rsn with
    | Forgot_sem -> fprintf fmt "maybe you forgot a `;`"
    | Keyword -> fprintf fmt "keyword expected"
    | Signal_name -> fprintf fmt "signal name expected"
    | Signal_tuple -> fprintf fmt "signal tuple expected"
    | Signal_decl_at_start -> fprintf fmt "signal declarations must be on the top"


  let print_error fmt e =
    let open Format in
    fprintf fmt "[pendulum] ";
    match e with
    | Value_missing s -> fprintf fmt "Signal value of %s is missing" s
    | Syntax rsn -> fprintf fmt "Syntax error : %a" print_syntax_rsn rsn
    | Non_recursive_let -> fprintf fmt "recursive `let` not allowed"
    | Only_on_let -> fprintf fmt "only allowed on let"
    | Other_err (e, f) -> fprintf fmt "%a" f e

  let error ~loc rsn =
    raise (Location.Error (
        Location.error ~loc (Format.asprintf "%a" print_error rsn)))

  let syntax_error ~loc r = error ~loc (Syntax r)

  let check_expr_ident e =
    let open Ast in
    match e.pexp_desc with
    | Pexp_ident {txt = Lident content; loc} -> {loc; content}
    | _ -> syntax_error ~loc:e.pexp_loc Signal_name

  let check_pat_ident e =
    let open Ast in
    match e.ppat_desc with
    | Ppat_var {txt = content; loc} ->
      {loc; content}
    | _ -> syntax_error ~loc:e.ppat_loc Signal_name

  let signal_value_missing e s =
    error ~loc:e.pexp_loc (Value_missing (check_expr_ident s).Ast.content)

end

let signal_tuple_to_list e =
  let open Ast in
  match e.pexp_desc with
  | Pexp_ident {txt = Lident content; loc } -> [{loc; content}]
  | Pexp_tuple exprs -> List.map Error.check_expr_ident exprs
  | _ -> Error.syntax_error ~loc:e.pexp_loc Error.Signal_tuple

let pop_signals_decl e =
  let cont origin e =
    match e with
    | [%expr [%e? e_var] [%e? _]] ->
      Error.(syntax_error ~loc:e_var.pexp_loc Forgot_sem)
    | [%expr [%e? e_var]] -> Ast.(mk_signal ~origin @@ Error.check_expr_ident e_var)
  in
  let rec aux sigs p =
    match p with
    | [%expr [%e? params], [%e? e2]] ->
      Error.(syntax_error ~loc:e2.pexp_loc Forgot_sem)
    | [%expr [%e? params]; [%e? e2] ] ->
      begin match params.pexp_desc with
        | Pexp_tuple ([%expr input [%e? e_var]] :: ids) ->
          aux
            Ast.(List.rev @@ (mk_signal ~origin:Input @@ Error.check_expr_ident e_var)
             :: ((List.map (cont Input) ids) @ sigs)) e2
        | Pexp_tuple ([%expr output [%e? e_var]] :: ids)->
          aux
            Ast.(List.rev @@ (mk_signal ~origin:Output @@ Error.check_expr_ident e_var)
             :: ((List.map (cont Output) ids) @ sigs)) e2
        | _ ->
          begin match params with
            | [%expr input [%e? e_var]] ->
              aux Ast.(mk_signal ~origin:Input (Error.check_expr_ident e_var) :: sigs) e2
            | [%expr output [%e? e_var]] ->
              aux Ast.(mk_signal ~origin:Output (Error.check_expr_ident e_var) :: sigs) e2
            | [%expr input [%e? e_var] [%e? _]]
            | [%expr output [%e? e_var] [%e? _]] ->
              Error.(syntax_error ~loc:e_var.pexp_loc Forgot_sem)
            | _ -> p, sigs
          end
      end
    | e -> e, sigs
  in aux [] e

let ast_of_expr e =
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
      Emit (Ast.mk_vid (Error.check_expr_ident signal) e_value)

    | [%expr emit [%e? signal]] ->
      Error.signal_value_missing e signal

    | [%expr exit [%e? label]] ->
      Exit (Label(Error.check_expr_ident label))

    | [%expr atom [%e? e]] ->
      Atom e

    | [%expr loop [%e? e]] ->
      Loop (visit e)

    | [%expr [%e? e1]; [%e? e2]] ->
      Seq (visit e1, visit e2)

    | [%expr [%e? e1] || [%e? e2]] ->
      Par (visit e1, visit e2)

    | [%expr present [%e? signal] [%e? e1] [%e? e2]] ->
      Present (Error.check_expr_ident signal, visit e1, visit e2)

    | [%expr let [%p? signal] = [%e? e_value] in [%e? e]] ->
      Signal (Ast.mk_vid (Error.check_pat_ident signal) e_value, visit e)

    | [%expr signal [%e? signal] [%e? e_value] [%e? e]] ->
      Signal (Ast.mk_vid (Error.check_expr_ident signal) e_value, visit e)

    | [%expr signal [%e? signal] [%e? _]] ->
      Error.signal_value_missing e signal

    | [%expr suspend [%e? e] [%e? signal]] ->
      Suspend (visit e, Error.check_expr_ident signal)

    | [%expr trap [%e? label] [%e? e]] ->
      Trap (Label (Error.check_expr_ident label), visit e)

    | [%expr run [%e? ident] [%e? tupl]] ->
      Run (Error.check_expr_ident ident, signal_tuple_to_list tupl, tupl.pexp_loc)

    | [%expr halt ] ->
      Halt

    | [%expr sustain [%e? signal] [%e? e_value]] ->
      Sustain (Ast.mk_vid (Error.check_expr_ident signal) e_value)

    | [%expr sustain [%e? signal]] ->
      Error.signal_value_missing e signal

    | [%expr present [%e? signal] [%e? e]] ->
      Present_then
        (Error.check_expr_ident signal, visit e)

    | [%expr await [%e? signal]] ->
      Await (Error.check_expr_ident signal)

    | [%expr abort [%e? e] [%e? signal]] ->
      Abort (visit e, Error.check_expr_ident signal)

    | [%expr loopeach [%e? e] [%e? signal]] ->
      Loop_each (visit e, Error.check_expr_ident signal)

    | [%expr every [%e? e] [%e? signal]] ->
      Every (Error.check_expr_ident signal, visit e)

    | [%expr nothing [%e? e_err]]
    (* | [%expr pause [%e? e_err]] *)
    (* | [%expr emit [%e? _] [%e? _] [%e? e_err]] *)
    (* | [%expr exit [%e? _] [%e? e_err]] *)
    (* | [%expr atom [%e? _] [%e? e_err]] *)
    (* | [%expr loop [%e? _] [%e? e_err]] *)
    (* | [%expr present [%e? _] [%e? _] [%e? _] [%e? e_err]] *)
    (* | [%expr signal [%e? _] [%e? _] [%e? e_err]] *)
    (* | [%expr suspend [%e? _] [%e? _][%e? e_err]] *)
    (* | [%expr trap [%e? _] [%e? _][%e? e_err]] *)
    (* | [%expr halt [%e? e_err]] *)
    (* | [%expr sustain [%e? _] [%e? _] [%e? e_err]] *)
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

let parse_ast ext vb =
  let e, sigs = pop_signals_decl vb.pvb_expr in
  let loc = vb.pvb_loc in
  begin match ext with
    | "sync_ast" ->
      [%expr ([%e Pendulum_misc.expr_of_ast @@ ast_of_expr e])]

    | "to_dot_grc" ->
      let pat =
        match vb.pvb_pat.ppat_desc with
        | Ppat_var id -> id.txt
        | _ -> "unknown"
      in
      let ast = ast_of_expr e in
      let tast, env = Ast.Tagged.of_ast ~sigs ast in
      let tast = Ast.Analysis.filter_dead_trees tast in

      Pendulum_misc.print_to_dot loc pat tast;
      let _ocaml_expr =
        Sync2ml.generate sigs env tast
      in
      Format.printf "%a@." Pprintast.expression _ocaml_expr;
      [%expr [%e Pendulum_misc.expr_of_ast ast]]

    | "sync" ->
      let ast = ast_of_expr e in
      let tast, env = Ast.Tagged.of_ast ~sigs ast in
      let tast = Ast.Analysis.filter_dead_trees tast in
      let ocaml_expr =
        Sync2ml.generate sigs env tast in

      [%expr [%e ocaml_expr]]

    | _ -> assert false
  end

let gen_bindings ext vbl =
  List.map (fun vb ->
      {vb with pvb_expr = parse_ast ext vb}
    ) vbl


let expected_ext = Utils.StringSet.(
    add "to_dot_grc" (
      add "sync_ast" (
        singleton "sync" )))

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


let extend_mapper argv =
  let open Sync2ml in
  {default_mapper with
   structure_item = try_compile_error (fun mapper stri ->
       match stri with
       | { pstr_desc = Pstr_extension (({ txt = ext }, PStr [
           { pstr_desc = Pstr_value (Nonrecursive, vbs) }]), attrs); pstr_loc }
         when StringSet.mem ext expected_ext ->
         (Str.value Nonrecursive (gen_bindings ext vbs))

       | { pstr_desc = Pstr_extension (({ txt = ext }, PStr [
           { pstr_desc = Pstr_value (Recursive, _) }]), _); pstr_loc }
         when StringSet.mem ext expected_ext ->
         Error.(error ~loc:pstr_loc Non_recursive_let)
       | x -> default_mapper.structure_item mapper x
     ) ;

   expr = try_compile_error (fun mapper exp ->
       match exp with
       | { pexp_desc = Pexp_extension ({ txt = ext; loc }, e)}
         when StringSet.mem ext expected_ext ->
         begin match e with
           | PStr [{ pstr_desc = Pstr_eval (e, _)}] ->
             begin match e.pexp_desc with
               | Pexp_let (Nonrecursive, vbl, e) ->
                 Exp.let_ Nonrecursive (gen_bindings ext vbl)
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
  register "pendulum" extend_mapper
