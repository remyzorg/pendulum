
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Pendulum

let syntax_error ~loc s = raise (Location.Error (
    Location.error ~loc ("[%sync] " ^ s)))

let check_ident_string e =
  match e.pexp_desc with
  | Pexp_ident {txt = Lident s; loc} -> s
  | Pexp_construct ({txt = Lident s; loc}, None) -> s
  | _ -> syntax_error ~loc:e.pexp_loc "identifier expected"

let check_ident e =
  match e.pexp_desc with
  | Pexp_ident {txt = Lident s; loc} ->
    {e with pexp_desc = Pexp_constant (Const_string (s, None))}
  | Pexp_construct ({txt = Lident s; loc}, None) ->
    {e with pexp_desc = Pexp_constant (Const_string (s, None))}
  | _ -> syntax_error ~loc:e.pexp_loc "identifier expected"


let ast_of_syntax r e =
  let rec visit e =
    let open Ast in
    let open Ast.Derived in
    match e with
    | [%expr nothing] ->
      Nothing

    | [%expr pause] ->
      Pause

    | [%expr emit [%e? signal]] ->
      Emit (check_ident_string signal)

    | [%expr exit [%e? label]] ->
      Exit (Label(check_ident_string label))

    | [%expr atom [%e? e]] ->
      Atom (fun () -> r := e; ())

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

let rec ast_expr_of_ast r e =
  let open Ast in
  let open Ast.Derived in
  let rec visit e =
    match e with
    | Nothing ->
      [%expr Nothing]

    | Pause ->
      [%expr Pause]

    | Emit signal ->
      [%expr Emit [%e signal]]

    | Exit (Label(label)) ->
      [%expr Exit (Label([%e label]))]

    | Atom f ->
      let e = f (); !r in
      [%expr Atom (fun () -> [%e e]; ())]

    | Loop e ->
      [%expr Loop ([%e visit e])]

    | Seq (e1, e2) ->
      [%expr Seq ([%e visit e1], [%e visit e2])]

    | Par (e1, e2) ->
      [%expr Par ([%e visit e1], [%e visit e2])]

    | Present (signal, e1, e2) ->
      [%expr Present ([%e signal], [%e visit e1], [%e visit e2])]

    | Signal (signal, e) ->
      [%expr Signal ([%e signal], [%e visit e])]

    | Suspend (e, signal) ->
      [%expr Suspend ([%e visit e], [%e visit signal])]

    | Trap (Label label, e) ->
      [%expr Trap (Label [%e label], [%e visit e])]

    | Halt ->
      [%expr Halt]

    | Sustain signal ->
      [%expr Sustain ([%e signal])]

    | Present_then (signal, e) ->
      [%expr Present_then ([%e signal], [%e visit e])]

    | Await signal ->
      [%expr Await [%e signal]]

    | Abort (e, signal) ->
      [%expr Abort ([%e visit e], [%e signal])]

    | Loop_each (e, signal) ->
      [%expr Loop_each ([%e visit e], [%e signal])]

    | Every (signal, e) ->
      [%expr Every ([%e signal], [%e visit e])]

    | e -> syntax_error ~loc:e.pexp_loc "Syntax error : pendulum keyword expected"
  in visit e

let rec handle_expr e =
  match e with
  | [%expr nothing] ->
    [%expr Nothing]

  | [%expr pause] ->
    [%expr Pause]

  | [%expr emit [%e? signal]] ->
    [%expr Emit [%e check_ident signal]]

  | [%expr exit [%e? label]] ->
    [%expr Exit (Label([%e check_ident label]))]

  | [%expr atom [%e? e]] ->
    [%expr Atom (fun () -> [%e e]; ())]

  | [%expr loop [%e? e]] ->
    [%expr Loop ([%e handle_expr e])]

  | [%expr [%e? e1]; [%e? e2]] ->
    [%expr Seq ([%e handle_expr e1], [%e handle_expr e2])]

  | [%expr [%e? e1] || [%e? e2]] ->
    [%expr Par ([%e handle_expr e1], [%e handle_expr e2])]

  | [%expr present [%e? signal] [%e? e1] [%e? e2]] ->
    [%expr Present ([%e check_ident signal], [%e handle_expr e1], [%e handle_expr e2])]

  | [%expr signal [%e? signal] [%e? e]] ->
    [%expr Signal ([%e check_ident signal], [%e handle_expr e])]

  | [%expr suspend [%e? e] [%e? signal]] ->
    [%expr Suspend ([%e handle_expr e], [%e check_ident signal])]

  | [%expr trap [%e? label] [%e? e]] ->
    [%expr Trap (Label [%e check_ident label], [%e handle_expr e])]

  | [%expr halt ] ->
    [%expr Halt]

  | [%expr sustain [%e? signal]] ->
    [%expr Sustain ([%e check_ident signal])]

  | [%expr present [%e? signal] [%e? e]] ->
    [%expr Present_then
        ([%e check_ident signal], [%e handle_expr e])]

  | [%expr await [%e? signal]] ->
    [%expr Await [%e check_ident signal]]

  | [%expr abort [%e? e] [%e? signal]] ->
    [%expr Abort ([%e handle_expr e], [%e check_ident signal])]

  | [%expr loopeach [%e? e] [%e? signal]] ->
    [%expr Loop_each ([%e handle_expr e], [%e check_ident signal])]

  | [%expr every [%e? e] [%e? signal]] ->
    [%expr Every ([%e check_ident signal], [%e handle_expr e])]

  | e -> syntax_error ~loc:e.pexp_loc "Syntax error : pendulum keyword expected"



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
          [%expr Derived.([%e handle_expr e])]
        | _ ->
          raise (Location.Error (
            Location.error ~loc "[%sync] is only on expressions"))

        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }



let () = register "sync" extend_mapper
