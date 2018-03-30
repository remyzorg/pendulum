

module Ast = Grc2ml.Ast


open Ast


open Migrate_parsetree
open Ast_405
open Ast_helper
open Parsetree

let build_tuple tuple mk init exprs =
  match exprs with
  | [] -> init
  | [s, t] -> mk ?t s
  | l -> tuple @@ List.rev_map (fun (s, t) -> mk ?t s) l

let dumb = Exp.constant (Ast_helper.Const.int 0)
let int_const i = Exp.constant (Ast_helper.Const.int i)
let string_const s = Exp.constant (Ast_helper.Const.string s)

let mk_pat_var ?t s =
  let pvar = Pat.((var @@ Location.mkloc s.content s.loc)) in
  match t with None -> pvar | Some t ->
    Pat.((constraint_ ~loc:s.loc pvar t))

let signaltype_of_type t =
  [%type: ([%t t], [%t t]) Pendulum.Signal.signal]


let mk_ident s = Exp.ident ~loc:s.loc
    Location.(mkloc (Longident.Lident s.content) s.loc)

let mk_value_binding ?(pvb_loc=Ast.dummy_loc ())
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

let has_to_be_defined s = match s with
  | No_binding | Event _ -> true
  | Access _ -> false

let append_tag s tag =
  {s with ident =
            {s.ident with content = Format.sprintf "%s##%s" s.ident.content tag.content;
            }}

let is_input s = match s.origin with
  | Local | Input | Element -> true
  | Output | React -> false

let is_tagged env s =
  Tagged.(s.origin = Element || Hashtbl.mem env.binders_env s.ident.content)

let mk_notbind env mk mknb s =
  if is_tagged env s then mk s.ident
  else mknb s.ident

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
