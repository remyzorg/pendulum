
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Preproc



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
      [%expr Emit [%e string_const signal.ident]]

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
      [%expr Signal ([%e string_const signal.ident], [%e visit e])]

    | Suspend (e, signal) ->
      [%expr Suspend ([%e visit e], [%e string_const signal])]

    | Trap (Label label, e) ->
      [%expr Trap (Label [%e string_const label], [%e visit e])]

    | Halt ->
      [%expr Halt]

    | Sustain signal ->
      [%expr Sustain ([%e string_const signal.ident])]

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

    | _ -> assert false
    end  [@metaloc e.loc]
  in visit e

let print_to_dot_one name ext f e =
  let full_name = (name ^ ext) in
  let c = open_out (full_name ^ ".dot") in
  let fmt = Format.formatter_of_out_channel c in
  f fmt e;
  close_out c;
  ignore @@ Sys.command (Format.sprintf "dot -Tpdf %s.dot -o %s.pdf" full_name full_name);
  (Unix.unlink (full_name ^ ".dot"))


let print_to_dot loc pat =
  let open Location in
  fun e ->
    let name = Filename.(
        loc.loc_start.Lexing.pos_fname
        |> chop_extension
        |> basename
      ) ^ ("_" ^ pat)
    in
    print_to_dot_one name "_tagged" Ast.Tagged.print_to_dot e;
    let sel, fg = Grc.Of_ast.construct e in
    print_to_dot_one name "_sel" Grc.Selection_tree.print_to_dot sel;
    print_to_dot_one name "_fg" Grc.Flowgraph.print_to_dot fg;
    let fg = Grc.Schedule.interleave fg in
    print_to_dot_one name "_interfg"
      Grc.Flowgraph.print_to_dot fg
    (* Format.printf "=============================@."; *)
    (* Sync2ml.(pp_ml_sequence 0 Format.std_formatter (grc2ml fg)) *)
