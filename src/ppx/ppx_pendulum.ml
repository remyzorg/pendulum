open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident



let syntax_error ~loc s = raise (Location.Error (
    Location.error ~loc ("[%sync] " ^ s)))


let rec handle_expr ~loc e =
  let open Ast in
  match e with
  | [%expr nothing] -> ()
  | [%expr pause] -> ()
  | [%expr emit [%e? signal]] -> ()
  | [%expr exit [%e? label]] -> ()
  | [%expr atom [%e? e]] -> ()
  | [%expr loop [%e? e]] ->
    handle_expr ~loc e
  | [%expr [%e? e1]; [%e? e2]] ->
    handle_expr ~loc e1; handle_expr ~loc e2
  | [%expr [%e? e1] || [%e? e2]] ->
    handle_expr ~loc e1; handle_expr ~loc e2
  | [%expr present [%e? signal] [%e? e1] [%e? e2]] ->
    handle_expr ~loc e1; handle_expr ~loc e2
  | [%expr suspend [%e? e] [%e? signal]] ->
    handle_expr ~loc e
  | [%expr trap [%e? label] [%e? e]] ->
    handle_expr ~loc e
  (* | [%expr ] *)


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



let () = register "pendulum" extend_mapper
