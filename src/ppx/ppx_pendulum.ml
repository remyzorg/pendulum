
open Ast_mapper
open Ast_helper
open Asttypes
open Parsetree
open Longident

open Pendulum_preproc



let check_ident_string e =
  let open Ast in
  match e.pexp_desc with
  | Pexp_ident {txt = Lident s; loc}
  | Pexp_construct ({txt = Lident s; loc}, None) -> {loc; content = s}
  | _ -> Ast.syntax_error ~loc:e.pexp_loc "identifier expected"

let pop_signals_decl e =
  let rec aux sigs e =
    match e with
    | [%expr input [%e ?e]; [%e ?e2] ]
    | [%expr output [%e ?e]; [%e ?e2]] ->
      let ident = check_ident_string e in
      aux (ident.Ast.content :: sigs) e2
    | e -> e, sigs
  in
  aux [] e

let pop_signals_decl e =
  let cont e = (check_ident_string e).Ast.content in
  let rec aux sigs p =
    match p with
    | [%expr [%e ?params]; [%e ?e2] ] ->
      begin match params.pexp_desc with
        | Pexp_tuple ([%expr input [%e ?e]] :: ids)
        | Pexp_tuple ([%expr output [%e ?e]] :: ids)->
          aux (cont e :: ((List.map cont ids) @ sigs)) e2
        | _ ->
          begin match params with
            | [%expr input [%e ?e]] | [%expr output [%e ?e]] -> aux (cont e :: sigs) e2
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

    | e -> Ast.(error ~loc:e.pexp_loc Syntax)
  in visit e


let extend_mapper argv =
  { default_mapper with
    expr = fun mapper expr ->
      match expr with
      | { pexp_desc = Pexp_extension ({ txt = ext; loc }, e)}
        when ext = "sync" || ext = "sync_ast" || ext = "to_dot_grc" ->
        begin try
            begin match e with
              | PStr [{ pstr_desc = Pstr_eval (e, _)}] ->
                let e, env = pop_signals_decl e in
                begin match ext with
                  | "sync_ast" -> [%expr ([%e Pendulum_misc.expr_of_ast @@ ast_of_expr e])]
                  | "to_dot_grc" ->
                    let e = ast_of_expr e in
                    Pendulum_misc.print_to_dot loc Ast.(Tagged.of_ast ~env e);
                    [%expr ([%e Pendulum_misc.expr_of_ast e])]
                  | "sync" -> [%expr ([%e Sync2ml.generate @@ ast_of_expr e])]
                  | _ -> assert false
                end
              | _ ->
                raise (Location.Error (
                    Location.error ~loc "[%sync] is only on expressions"))
            end
          with Ast.Error (loc, e) ->
            raise (Location.Error (
                Location.error ~loc (Format.asprintf "[%%sync] : %a" Ast.print_error e)))
        end
      | x -> default_mapper.expr mapper x;
  }

let () = register "pendulum" extend_mapper
