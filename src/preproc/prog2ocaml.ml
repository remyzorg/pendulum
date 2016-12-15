
module Expression = Grc2ml.Expression
module Ast = Grc2ml.Ast
module Flowgraph = Grc2ml.Flowgraph
module Selection_tree = Grc2ml.Selection_tree
module Schedule = Grc2ml.Schedule
module Of_ast = Grc2ml.Of_ast

open Utils

open Gen_utils
open Gen_names


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


module type S = sig

  val signal_to_creation_expr :
    Expression.t -> Ast.signal -> Expression.t

  val mk_args_signals_definitions :
    Ast.Tagged.env -> Expression.t -> Expression.t

  val mk_constructor_reactfun :
    Ast.Tagged.env -> bool -> bool -> Expression.t ->
    (Expression.t -> Expression.t) * Expression.t

  val mk_program_object :
    Ast.Tagged.env -> Expression.t -> Expression.t

  val mk_callbacks_assigns :
    bool -> Ast.Tagged.env -> Expression.t -> Expression.t

  val mk_createfun_inputs_expr :
    Ast.Tagged.env -> (Ast.signal * Parsetree.core_type option) list -> Expression.t

  val mk_constructor :
    StringSet.t -> int -> Ast.Tagged.env -> Expression.t -> Expression.t

  val generate : string -> StringSet.t -> Ast.Tagged.env -> Ast.Tagged.t -> Expression.t

end


module Make (Schema : S) = struct

  open Ast_helper
  open Parsetree
  open Utils
  open Ast



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



  let mk_local_signals_definitions env e = List.fold_left (fun acc vs ->
      let atom = Grc2ml.ML_optimize.rm_atom_deps vs.svalue in
      let rebinds = rebind_locals_let atom.locals
          [%expr ref (make_signal [%e vs.svalue.exp])]
      in
      [%expr let [%p mk_pat_var vs.signal.ident] = [%e rebinds] in [%e acc]]
    ) e !(env.Tagged.local_only_env)

  let append_tag s tag =
    {s with ident =
              {s.ident with content = Format.sprintf "%s##%s" s.ident.content tag.content;
              }}

  let has_tobe_defined s = match s with
    | No_binding | Event _ -> true
    | Access _ -> false

  let is_input s = match s.origin with
    | Local | Input | Element -> true
    | Output | React -> false

  let signal_to_definition rhs next_def s =
    [%expr let [%p mk_pat_var s.ident] = [%e rhs] in [%e next_def]]



  let is_tagged env s =
    Tagged.(s.origin = Element || Hashtbl.mem env.binders_env s.ident.content)

  let mk_set_all_absent_definition env e =
    let open Tagged in
    (* let set_absent_expr ident = *)
    let locals_setters =
      (List.fold_left (fun acc vs ->
           [%expr set_absent ![%e mk_ident vs.signal.ident]; [%e acc]]
         ) [%expr ()] !(env.local_only_env))
    in
    let absent_setters =
      let events_setabs s acc bind =
        let mk s e = mk_ident @@ (append_tag s e).ident in
        match bind with
        | Event (e, ev) ->
          let acc = if ev = None then acc else
              [%expr [%e mk s e].value <- [%e mk s e].default; [%e acc]]
          in [%expr set_absent [%e mk s e]; [%e acc]]
        | _ -> acc in
      List.fold_left (fun acc (s, _) ->
          try
            if s.origin = Element then acc
            else Hashtbl.find env.Tagged.binders_env s.ident.content
                 |> List.fold_left (events_setabs s) acc
          with Not_found ->
            let exp = Option.casefv s.gatherer (fun _ ->
                [%expr [%e mk_ident s.ident].value <- [%e mk_ident s.ident].default; [%e acc]]
              ) acc
            in [%expr set_absent [%e mk_ident s.ident]; [%e exp]]
        ) locals_setters env.args_signals
    in
    [%expr let set_absent () = [%e absent_setters] in [%e e]]


  let mk_method loc str expr =
    Asttypes.(Cf.method_ {txt = str; loc}
                Public (Cfk_concrete (Fresh, expr)))

  let pcstr_fields mk_field env reactfun =
    let open Tagged in
    List.fold_left
      (fun acc (s, _) ->
         match s.origin with
         | Input when not @@ is_tagged env s -> mk_field s :: acc
         | React ->
           (mk_method s.ident.loc s.ident.content
            @@ (mk_ident @@ ident_app_str s.ident api_React_output_signal)
           ) :: acc
         | Input | Local | Output | Element -> acc
      ) [(mk_method env.pname.loc api_react_function_name
            [%expr [%e reactfun] ()])] env.args_signals

  let mk_callbacks_assigns_rhs step_call s tag =
    [%expr Dom_html.handler (
        fun ev ->
          set_present_value [%e mk_ident (append_tag s tag).ident] ev;
          [%e step_call];
          Js._true)]

  let mk_callbacks_assigns mk_rhs animate env e =
    let opexpr loc = mk_ident @@ Ast.mk_loc ~loc "##." in
    let mk_lhs s tag =
      [%expr [%e opexpr tag.loc] [%e mk_ident s.ident] [%e mk_ident tag]]
    in
    let step_call =
      if animate then
        [%expr [%e mk_ident @@ Ast.mk_loc animate_name] ()]
      else
        [%expr ignore @@ [%e mk_ident @@ Ast.mk_loc reactfun_name] ()]
    in
    let mk_rhs = mk_rhs step_call in
    let mk_assign s acc typ =
      match typ with | No_binding -> acc | Access _ -> acc
                     | Event (tag, _) ->
                       [%expr [%e mk_lhs s tag] := [%e mk_rhs s tag]; [%e acc]]
    in List.fold_left (
      fun acc (s, _) -> try
          Hashtbl.find env.Tagged.binders_env s.ident.content
          |> List.fold_left (mk_assign s) acc
        with Not_found -> acc
    ) e env.Tagged.args_signals

  let mk_raf_call debug reactfun_ident =
    let debug_cnt = mk_ident @@ Ast.mk_loc debug_instant_cnt_name in
    let debug_cnt_str = [%expr string_of_int ![%e debug_cnt]] in
    [%expr
      fun () ->
        if not ![%e mk_ident @@ Ast.mk_loc animated_state_var_name] then (
          [%e Debug.(
              seqif ~debug ([%expr incr [%e debug_cnt]]) (
                seqif ~debug (print @@ str "Add to next frame..." ++ debug_cnt_str)
                  [%expr [%e mk_ident @@ Ast.mk_loc animated_state_var_name] := true]))];
          let _ = Dom_html.window##requestAnimationFrame
              (Js.wrap_callback (
                  fun _ -> [%e Debug.(
                      seqif ~debug
                        (print (str "Frame start..." ++ debug_cnt_str))
                        [%expr ignore @@ [%e reactfun_ident] ()])
                  ]; [%e Debug.(seqif ~debug
                                  (print @@ str "Frame done..." ++ debug_cnt_str)
                                  [%expr [%e mk_ident @@ Ast.mk_loc animated_state_var_name] := false])]
                ))
          in ()) else
          [%e Debug.(seqif ~debug (print (str "Already in next frame...")) [%expr ()])]
    ]

  let mk_callbacks = mk_callbacks_assigns mk_callbacks_assigns_rhs

  let mk_animate_mutex animate body =
    if not animate then body else
      [%expr let [%p mk_pat_var @@ Ast.mk_loc animated_state_var_name] = ref false in [%e body]]

  let mk_notbind env mk mknb s =
    if is_tagged env s then mk s.ident
    else mknb s.ident

  let mk_createfun_outputs_expr env =
    build_tuple Exp.tuple (
      fun ?t s ->
        let initexpr, output =
          match s.origin with
          | Output ->
            [%expr fst [%e (mk_ident s.ident)]],
            [%expr snd [%e (mk_ident s.ident)]]
          | React ->
            (mk_ident s.ident),
            [%expr React.S.create [%e (mk_ident s.ident)]][@metaloc s.ident.loc]
          | _ -> assert false
        in
        mk_notbind env mk_ident (fun _ ->
            [%expr [%e Schema.signal_to_creation_expr initexpr s], [%e output ]]) s
    ) [%expr ()]

  let mk_createfun_inputs_expr env =
    build_tuple Exp.tuple (
      fun ?t s -> mk_notbind env mk_ident (fun _ ->
          Schema.signal_to_creation_expr (mk_ident s.ident) s) s
    ) [%expr ()]

  let generate = Schema.generate


end


