
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident



let syntax_error ~loc s = raise (Location.Error (
    Location.error ~loc ("[%sync] " ^ s)))


let check_ident e =
  match e.pexp_desc with
  | Pexp_ident {txt = Lident s; loc} ->
    {e with pexp_desc = Pexp_constant (Const_string (s, None))}
  | _ -> syntax_error ~loc:e.pexp_loc "identifier expected"

let rec handle_expr e =
  match e with
  | [%expr nothing] ->
    [%expr Nothing]

  | [%expr pause] ->
    [%expr Pause]

  | [%expr emit [%e? signal]] ->
    [%expr Emit [%e check_ident signal]]

  | [%expr exit [%e? label]] ->
    [%expr Exit [%e check_ident label]]

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

  | _ -> syntax_error ~loc:e.pexp_loc "Syntax error"



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
