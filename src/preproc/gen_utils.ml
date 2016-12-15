
module Ast = Grc2ml.Ast

open Ast_helper
open Parsetree

open Utils
open Ast


let build_tuple tuple mk init exprs =
  match exprs with
  | [] -> init
  | [s, t] -> mk ?t s
  | l -> tuple @@ List.rev_map (fun (s, t) -> mk ?t s) l

let dumb = Exp.constant (Ast_helper.Const.int 0)
let int_const i = Exp.constant (Ast_helper.Const.int i)
let string_const s = Exp.constant (Ast_helper.Const.string s)

let mk_pat_var ?t s =
  let pvar = Pat.(Asttypes.(var @@ Location.mkloc s.content s.loc)) in
  match t with None -> pvar | Some t ->
    Pat.(Asttypes.(constraint_ ~loc:s.loc pvar t))

let signaltype_of_type t =
  [%type: ([%t t], _) Pendulum.Signal.signal]


let mk_ident s = Exp.ident ~loc:s.loc
    Location.(mkloc (Longident.Lident s.content) s.loc)

let mk_value_binding ?(pvb_loc=Location.none)
    ?(pvb_attributes=[]) pvb_pat pvb_expr =
  { pvb_pat; pvb_expr; pvb_attributes; pvb_loc; }

let ident_app_str ident str =
  Ast.mk_loc ~loc:ident.loc
    (Format.sprintf "%s%s" ident.content str)


let remove_signal_renaming s =
  try Ast.({s with content = String.sub s.content 0
                       ((String.rindex s.content '~'))})
  with Not_found -> s


let add_deref_local s =
  let open Ast in
  match s.origin with
  | Local -> [%expr ![%e mk_ident s.ident]]
  | _ -> mk_ident s.ident

let rebind_locals_let locals e =
  (List.fold_left (fun acc x ->
       [%expr let [%p mk_pat_var @@ remove_signal_renaming x.ident] =
                ![%e mk_ident x.ident]
         in [%e acc]]
     ) e locals)

let handle_param = function
  | Sig_param s -> add_deref_local s
  | Exp_param e -> e


module Debug = struct
  let str = string_const

  let print expr =
    [%expr Firebug.console##debug (Js.string [%e expr])]

  let letin ~debug patvar value expr =
    if debug then [%expr let [%p patvar] = [%e value] in
      [%e expr]]
    else expr

  let (++) a b = [%expr [%e a] ^ [%e b]]

  let seqif ~debug debug_expr expr =
    if debug then [%expr [%e debug_expr ]; [%e expr]] else expr
end
