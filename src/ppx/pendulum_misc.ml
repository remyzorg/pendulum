
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Pendulum_preproc



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

    | Exit (Label label) ->
      [%expr Exit (Label [%e string_const label])]

    | Loop e ->
      [%expr Loop ([%e visit e])]

    | Atom _ ->
      [%expr Atom ]

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
    let name = Filename.(
        loc.loc_start.Lexing.pos_fname
        |> chop_extension
        |> basename
      ) ^ ("_" ^ (string_of_int !n))
    in
    let fgname = (name ^ "_fg") in
    let interfgname = (name ^ "_interfg") in
    let tagname = (name ^ "_tast") in

    let c_tagged = open_out (tagname ^ ".dot") in
    let c_flowgraph = open_out (fgname ^ ".dot") in
    let c_inter_flowgraph = open_out (interfgname ^ ".dot") in

    let fmt_tagged = Format.formatter_of_out_channel c_tagged in
    let fmt_flowgraph = Format.formatter_of_out_channel c_flowgraph in
    let fmt_interleaved_flowgraph =
      Format.formatter_of_out_channel c_inter_flowgraph
    in

    Ast.Tagged.print_to_dot fmt_tagged e;
    let fg = Grc.flowgraph e in
    Grc.Flowgraph.print_to_dot fmt_flowgraph fg;
    Grc.Flowgraph.print_to_dot fmt_interleaved_flowgraph (Sync2ml.interleave fg);

    close_out c_flowgraph; close_out c_tagged; close_out c_inter_flowgraph;
    ignore @@ Sys.command (Format.sprintf "dot -Tpdf %s.dot -o %s.pdf" tagname tagname);
    ignore @@ Sys.command (Format.sprintf "dot -Tpdf %s.dot -o %s.pdf" fgname fgname);
    ignore @@ Sys.command (Format.sprintf "dot -Tpdf %s.dot -o %s.pdf" interfgname interfgname);
    Unix.unlink (tagname ^ ".dot");
    Unix.unlink (fgname ^ ".dot");
    Unix.unlink (interfgname ^ ".dot")

