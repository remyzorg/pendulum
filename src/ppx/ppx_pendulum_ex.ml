
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident



let syntax_error ~loc s = raise (Location.Error (
    Location.error ~loc ("[%sync] " ^ s)))


let check_ident e =
  match e.pexp_desc with
  | Pexp_ident (Lident s) -> s
  | _ -> syntax_error ~loc:e.pexp_loc "identifier expected"


let rec handle_expr ~loc e =
  match e with
  (*Nothing*)
  | [%expr nothing] ->
    [%expr Nothing]
  (*Pause*)
  | [%expr pause] ->
    [%expr Pause]

  (*Emit*)
  | [%expr emit [%e? signal]] ->
    [%expr Emit [%e check_signal signal]]

  (*Exit*)
  | [%expr exit [%e? label]] -> ()
    [%expr Emit [%e check_ident label]]

  (*Atom*)
  | [%expr atom [%e? e]] ->
    [%expr Atom (fun () -> [%e check_ident label]; ())]

  (*Loop*)
  | [%expr loop [%e? e]] ->
    [%expr Loop ([%e handle_expr ~loc e])]

  (*Seq*)
  | [%expr [%e? e1]; [%e? e2]] ->
    handle_expr ~loc e1; handle_expr ~loc e2

  (*Par*)
  | [%expr [%e? e1] || [%e? e2]] ->
    handle_expr ~loc e1; handle_expr ~loc e2

  (*Present*)
  | [%expr present [%e? signal] [%e? e1] [%e? e2]] ->
    handle_expr ~loc e1; handle_expr ~loc e2

  (*Suspend*)
  | [%expr suspend [%e? e] [%e? signal]] ->
    handle_expr ~loc e

  (*Trap*)
  | [%expr trap [%e? label] [%e? e]] ->
    handle_expr ~loc e

  (*Halt*)
  | [%expr halt ] -> ()

  (*Sustain*)
  | [%expr sustain [%e? signal]] -> ()

  (*Present*)
  | [%expr present [%e? signal] [%e? e]] -> ()

  (*Await*)
  | [%expr await [%e? signal]] -> ()

  (*Abort*)
  | [%expr abort [%e? e][%e? signal]] -> ()

  (*Loopeach*)
  | [%expr loopeach [%e? e] [%e? signal]] -> ()

  (*Every*)
  | [%expr every [%e? e] [%e? signal]] -> ()

  | _ -> syntax_error ~loc "Syntax error"



let extend_mapper argv =
  (* Our getenv_mapper only overrides the handling of expressions in the default mapper. *)
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      (* Is this an extension node? *)
      | { pexp_desc =
          (* Should have name "getenv". *)
          Pexp_extension ({ txt = "sync"; loc }, pe)} ->
        begin match pe with
        | PStr [{ pstr_desc = Pstr_eval (e, _)}] -> handle_expr ~loc e; [%expr ()]
        | _ ->
          raise (Location.Error (
            Location.error ~loc "[%sync] is only on expressions"))

        end
      (* Delegate to the default mapper. *)
      | x -> default_mapper.expr mapper x;
  }



let () = register "sync" extend_mapper
