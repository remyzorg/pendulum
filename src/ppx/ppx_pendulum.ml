
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Pendulum_preproc

let syntax_error ~loc s = raise (Location.Error (
    Location.error ~loc ("[%sync] " ^ s)))


let check_ident_string e =
  let open Ast in
  match e.pexp_desc with
  | Pexp_ident {txt = Lident s; loc}
  | Pexp_construct ({txt = Lident s; loc}, None) -> {loc; content = s}
  | _ -> syntax_error ~loc:e.pexp_loc "identifier expected"

let pop_signals_decl e =
  let rec aux sigs e =
    match e with
    | [%expr input [%e ?e]; [%e ?e2] ]
    | [%expr output [%e ?e]; [%e ?e2]] ->
      let ident = check_ident_string e in
      aux (ident :: sigs) e2
    | e -> e, sigs
  in
  aux [] e



let ast_of_expr e =
  let rec visit e =
    let open Ast in
    let open Ast.Derived in
    mk_loc ~loc:e.pexp_loc @@ match e with
    | [%expr nothing] ->
      Nothing

    | [%expr pause] ->
      Pause

    | [%expr emit [%e? signal]] ->
      Emit (check_ident_string signal)

    | [%expr exit [%e? label]] ->
      Exit (Label(check_ident_string label))

    | [%expr atom [%e? e]] ->
      Atom e

    | [%expr loop [%e? e]] ->
      Loop (visit e)

    | [%expr [%e? e1]; [%e? e2]] ->
      Seq (visit e1, visit e2)

    | [%expr [%e? e1] || [%e? e2]] ->
      Par (visit e1, visit e2)

    | [%expr present [%e? signal] [%e? e1] [%e? e2]] ->
      Present (check_ident_string signal, visit e1, visit e2)

    | [%expr signal [%e? signal] [%e? e]] ->
      Signal (check_ident_string signal, visit e)

    | [%expr suspend [%e? e] [%e? signal]] ->
      Suspend (visit e, check_ident_string signal)

    | [%expr trap [%e? label] [%e? e]] ->
      Trap (Label (check_ident_string label), visit e)

    | [%expr halt ] ->
      Halt

    | [%expr sustain [%e? signal]] ->
      Sustain (check_ident_string signal)

    | [%expr present [%e? signal] [%e? e]] ->
      Present_then
        (check_ident_string signal, visit e)

    | [%expr await [%e? signal]] ->
      Await (check_ident_string signal)

    | [%expr abort [%e? e] [%e? signal]] ->
      Abort (visit e, check_ident_string signal)

    | [%expr loopeach [%e? e] [%e? signal]] ->
      Loop_each (visit e, check_ident_string signal)

    | [%expr every [%e? e] [%e? signal]] ->
      Every (check_ident_string signal, visit e)

    | e -> syntax_error ~loc:e.pexp_loc "Syntax error : pendulum keyword expected"
  in visit e


let string_const ident =
  let open Ast in
  Exp.constant ~loc:ident.loc (Const_string (ident.content, None))

let rec expr_of_ast e =
  let open Ast in
  let open Ast.Derived in
  let rec visit e =
    begin match e.content with
    | Nothing ->
      [%expr Nothing]

    | Pause ->
      [%expr Pause]

    | Emit signal ->
      [%expr Emit [%e string_const signal]]

    | Exit (Label(label)) ->
      [%expr Exit (Label([%e string_const label]))]

    | Atom f ->
      [%expr Atom [%e f]]

    | Loop e ->
      [%expr Loop ([%e visit e])]

    | Seq (e1, e2) ->
      [%expr Seq ([%e visit e1], [%e visit e2])]

    | Par (e1, e2) ->
      [%expr Par ([%e visit e1], [%e visit e2])]

    | Present (signal, e1, e2) ->
      [%expr Present ([%e string_const signal], [%e visit e1], [%e visit e2])]

    | Signal (signal, e) ->
      [%expr Signal ([%e string_const signal], [%e visit e])]

    | Suspend (e, signal) ->
      [%expr Suspend ([%e visit e], [%e string_const signal])]

    | Trap (Label label, e) ->
      [%expr Trap (Label [%e string_const label], [%e visit e])]

    | Halt ->
      [%expr Halt]

    | Sustain signal ->
      [%expr Sustain ([%e string_const signal])]

    | Present_then (signal, e) ->
      [%expr Present_then ([%e string_const signal], [%e visit e])]

    | Await signal ->
      [%expr (Await [%e string_const signal])]

    | Abort (e, signal) ->
      [%expr Abort ([%e visit e], [%e string_const signal])]

    | Loop_each (e, signal) ->
      [%expr Loop_each ([%e visit e], [%e string_const signal])]

    | Every (signal, e) ->
      [%expr Every ([%e string_const signal], [%e visit e])]

    | _ -> syntax_error ~loc:e.loc "Syntax error : pendulum keyword expected"
    end  [@metaloc e.loc]
  in visit e


let print_to_dot loc =
  let open Location in
  let n = ref 0 in
  fun e ->
    let name = Filename.(basename @@ chop_extension @@
                         loc.loc_start.Lexing.pos_fname) in
    let fgname = (name ^ "_fg") in
    let tagname = (name ^ "_tast") in
    Format.printf "FILE HERE : |%s| |%n|" name (incr n; !n);
    let c_tagged = open_out (tagname ^ ".dot") in
    let c_flowgraph = open_out (fgname ^ ".dot")in
    let fmt_tagged = Format.formatter_of_out_channel c_tagged in
    let fmt_flowgraph = Format.formatter_of_out_channel c_flowgraph in
    Ast.Tagged.print_to_dot fmt_tagged e;
    Grc.Flowgraph.print_to_dot fmt_flowgraph (Grc.flowgraph e);
    close_out c_flowgraph; close_out c_tagged;
    ignore @@ Sys.command (Format.sprintf "dot -Tpdf %s.dot -o %s.pdf" tagname tagname);
    ignore @@ Sys.command (Format.sprintf "dot -Tpdf %s.dot -o %s.pdf" fgname fgname);
    Unix.unlink (tagname ^ ".dot");
    Unix.unlink (fgname ^ ".dot")


let extend_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = ext; loc }, pe)}
        when ext = "sync" || ext = "sync_ast" || ext = "to_dot_grc" ->
        begin match pe with
          | PStr [{ pstr_desc = Pstr_eval (e, _)}] ->
            let decls, e = pop_signals_decl e in
            begin match ext with
              | "sync_ast" -> [%expr ([%e expr_of_ast @@ ast_of_expr e])]
              | "to_dot_grc" ->
                let e = ast_of_expr e in
                print_to_dot loc Ast.(Tagged.of_ast decls @@ Ast.normalize @@ e);
                [%expr ([%e expr_of_ast e])]
              | "sync" -> [%expr ([%e Sync2ml.generate @@ ast_of_expr e])]
              | _ -> assert false
            end
          | _ ->
            raise (Location.Error (
                Location.error ~loc "[%sync] is only on expressions"))
        end
      | x -> default_mapper.expr mapper x;
  }



let () = register "pendulum" extend_mapper
