
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Preproc


module Ast = Sync2ml.Ast

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

    | Emit vs ->
      [%expr Emit [%e string_const vs.sname]]

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

    | Present ((signal,_ , tag), e1, e2) ->
      [%expr Present ([%e string_const signal], [%e visit e1], [%e visit e2])]

    | Signal (vid, e) ->
      [%expr Signal ([%e string_const vid.sname], [%e visit e])]

    | Run (id, args, loc) ->
      [%expr Run ([%e string_const id])]

    | Suspend (e, (signal, _, tag)) ->
      [%expr Suspend ([%e visit e], [%e string_const signal])]

    | Trap (Label label, e) ->
      [%expr Trap (Label [%e string_const label], [%e visit e])]

    | Halt ->
      [%expr Halt]

    | Sustain vid ->
      [%expr Sustain ([%e string_const vid.sname])]

    | Present_then ((signal, _, tag), e) ->
      [%expr Present_then ([%e string_const signal], [%e visit e])]

    | Await (signal, _, tag) ->
      [%expr (Await [%e string_const signal])]

    | Abort (e, (signal, _, tag)) ->
      [%expr Abort ([%e visit e], [%e string_const signal])]

    | Loop_each (e, (signal, _, tag)) ->
      [%expr Loop_each ([%e visit e], [%e string_const signal])]

    | Every ((signal, _, tag), e) ->
      [%expr Every ([%e string_const signal], [%e visit e])]

    | _ -> assert false
    end  [@metaloc e.loc]
  in visit e

let print_to_dot_one todot topdf topng name ext f e =
  let full_name = (name ^ ext) in
  let c = open_out (full_name ^ ".dot") in
  let fmt = Format.formatter_of_out_channel c in
  f fmt e;
  close_out c;
  if topdf then
    ignore @@ Sys.command (Format.sprintf "dot -Tpdf %s.dot -o %s.pdf" full_name full_name);
  if topng then
    ignore @@ Sys.command (Format.sprintf "dot -Tpng %s.dot -o %s.png" full_name full_name);
  if not todot then (Unix.unlink (full_name ^ ".dot"))


let print_to_dot todot topdf topng loc pat =
  let open Location in
  fun e ->
    let name = Filename.(
        loc.loc_start.Lexing.pos_fname
        |> chop_extension
        |> basename
      ) ^ ("_" ^ pat)
    in
    print_to_dot_one todot topdf topng name "_tagged" Ast.Tagged.print_to_dot e;
    let sel, fg = Sync2ml.Of_ast.construct e in
    print_to_dot_one todot topdf topng name "_sel" Sync2ml.Selection_tree.print_to_dot sel;
    print_to_dot_one todot topdf topng name "_fg" Sync2ml.Flowgraph.print_to_dot fg;
    let fg = Sync2ml.Schedule.interleave fg in
    print_to_dot_one todot topdf topng name "_interfg"
      Sync2ml.Flowgraph.print_to_dot fg
    (* Format.printf "=============================@."; *)
    (* Sync2ml.(pp_ml_sequence 0 Format.std_formatter (grc2ml fg)) *)
