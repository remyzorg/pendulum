
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

let extend_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc =
          (* Should have name "getenv". *)
          Pexp_extension ({ txt = "sync"; loc }, pe)} ->
        begin match pe with
        | PStr [{ pstr_desc = Pstr_eval (e, _)}] ->
          [%expr ([%e expr_of_ast @@ ast_of_expr e])]
        | _ ->
          raise (Location.Error (
            Location.error ~loc "[%sync] is only on expressions"))

        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }



let () = register "pendulum" extend_mapper
