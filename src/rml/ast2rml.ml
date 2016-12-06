

module Ast = Ml2ocaml.Ast
open Ast


type error = Rml_undefined
exception Error of error

let print_error fmt = function
  | Rml_undefined -> Format.fprintf fmt "undefined in RML backend"


let rec compile ast =
  let open Ast.Tagged in
  let open Ml2ocaml in
  match ast.st.content with
  | Loop t -> [%expr rml_loop [%e compile t]]
  | Seq (t1, t2) -> [%expr rml_seq [%e compile t1] [%e compile t2]]
  | Par (t1, t2) -> [%expr rml_par [%e compile t1] [%e compile t2]]

  | Emit vs ->
    let signal = mk_ident vs.signal.ident in
    let value = vs.svalue.exp in
    [%expr rml_emit_val [%e signal]
        (fun () -> [%e value])]

  | Nothing -> [%expr rml_nothing]
  | Pause -> [%expr rml_nothing]

  | Suspend (t, (s, _)) ->
    let signal = mk_ident s.ident in
    [%expr rml_control' [%e signal] [%e compile t]]

  | Trap (Label label, t) ->
    [%expr
      rml_signal (fun [%p mk_pat_var label] ->
          rml_until [%e mk_ident label]
            [%e compile t]
        )]

  | Exit (Label label) ->
    let signal = mk_ident label in
    [%expr rml_emit [%e signal]]

  | Present ((s, _), t1, t2) ->
    let signal = mk_ident s.ident in
    [%expr rml_present [%e signal]
        [%expr compile t1]
        [%expr compile t2]]

  | Atom atom -> [%expr rml_compute (fun () -> [%e atom.exp]; ())]

  | Signal (vs, t) ->
    [%expr rml_signal (fun [%p mk_pat_var vs.signal.ident] -> [%e compile t])]

  | Await (s, _) ->
    let signal = mk_ident s.ident in
    [%expr rml_await [%e signal]]

  | Run (ident, params, loc) -> raise (Error (Rml_undefined))



