[@@@warning "-9"]
open Utils

module type Location = sig
  type t
  val none : t
end

module type Exp = sig
  type t
  type core_type
  val print : Format.formatter -> t -> unit
  module Location : Location
end

module type S = sig

  type loc
  type exp
  type core_type

  val printexp : Format.formatter -> exp -> unit

  type 'a location = {
    loc : loc;
    content : 'a;
  }

  type ident = string location

  type signal_origin = Local | Input | Output | Element | React


  type gatherer = exp option

  type signal_binder =
    | Access of ident * ident list
    | Event of ident * gatherer
    | No_binding

  type signal = { ident : ident; origin : signal_origin; bind : signal_binder; gatherer : gatherer}
  type label = Label of ident
  type atom = { locals : signal list; exp : exp}

  type test = ident * ident option * exp option

  type valued_signal = {signal : signal ; svalue : atom}
  type valued_ident = {sname : ident ; fields : ident list; ivalue : exp}

  type 'a run_param = Sig_param of 'a | Exp_param of exp
  val filter_param : (signal -> 'b) -> signal run_param list -> 'b list
  val mk_signal : ?origin:signal_origin -> ?bind:signal_binder -> ?gatherer:exp -> ident -> signal

  val mk_vsig : signal -> signal list -> exp -> valued_signal
  val mk_vid : ?fields:ident list -> ident -> exp -> valued_ident

  val mk_atom : ?locals:signal list -> exp -> atom

  module IdentMap : Map.S with type key = ident
  module IdentSet : Set.S with type elt = ident
  module SignalMap : Map.S with type key = signal
  module SignalSet : Set.S with type elt = signal

  val set_dummy_loc : loc -> unit
  val dummy_loc : unit -> loc
  val mk_loc : ?loc:loc -> 'a -> 'a location

  module Derived : sig
    type statement = (statement_tree) location
    and statement_tree =
      | Loop of statement
      | Seq of statement * statement
      | Par of statement * statement
      | Emit of valued_ident
      | Nothing
      | Pause
      | Suspend of statement * test
      | Trap of label * statement
      | Exit of label
      | Present of test * statement * statement
      | Atom of exp
      | Signal of valued_ident * statement
      | Run of ident * (ident * ident option) run_param list * loc

      | Halt
      | Sustain of valued_ident
      | Present_then of test * statement
      | Await of test
      | Await_imm of test
      | Suspend_imm of statement * test
      | Abort of statement * test
      | Weak_abort of statement * test
      | Loop_each of statement * test
      | Every of test * statement
  end

  type error =
    | Unbound_identifier of string
  exception Error of loc * error
  val error : loc:loc -> error -> 'a
  val print_error : Format.formatter -> error -> unit

  module Tagged : sig

    type tlabel = TLabel of ident * int

    type t = {id : int; st : tagged}
    and test = signal * atom option
    and tagged_ast =
      | Loop of t
      | Seq of t * t
      | Par of t list
      | Emit of valued_signal
      | Nothing
      | Pause
      | Suspend of t * test
      | Trap of tlabel * t
      | Exit of tlabel
      | Present of test * t * t
      | Atom of atom
      | Signal of valued_signal * t
      | Await of test
      | Run of ident * signal run_param list * loc
    and tagged = (tagged_ast) location

    type env = {
      pname : ident;
      args_signals : (signal * core_type option) list;
      nlabels : int ref;
      labels : int IdentMap.t;
      global_occurences : int IdentMap.t ref;
      scope : (int * signal_origin * signal_binder * gatherer) SignalMap.t;
      binders_env : (string, signal_binder list) Hashtbl.t;
      local_only_env : (valued_signal) list ref;
      local_only_scope : valued_signal list;
      machine_runs : (int * (int * signal run_param list) list) IdentMap.t ref;
    }

    val print_env : Format.formatter -> env -> unit

    val of_ast :
      ?sigs:((signal * core_type option) list) ->
      ?binders:((string * signal_binder list) list) ->
      ident -> Derived.statement -> t * env

    val pp_st : Format.formatter -> t -> unit
    val pp_dot : Format.formatter -> t -> unit
  end

  module Analysis : sig
    val blocking : Tagged.t -> bool
    val non_blocking : Tagged.t -> bool
    val filter_dead_trees : Tagged.t -> Tagged.t
  end

end

module Make (E : Exp) = struct

  module Location = E.Location

  type loc = Location.t
  let printexp = E.print
  type exp = E.t
  type core_type = E.core_type

  type 'a location = {
    loc : loc;
    content : 'a;
  }

  let dummy_loc_ref = ref Location.none
  let set_dummy_loc loc = dummy_loc_ref := loc
  let dummy_loc () = ! dummy_loc_ref

  type ident = string location

  type gatherer = exp option

  type signal_binder =
    | Access of ident * ident list
    | Event of ident * gatherer
    | No_binding

  type signal_origin = Local | Input | Output | Element | React


  type signal = { ident : ident; origin : signal_origin; bind : signal_binder; gatherer : gatherer}

  type atom = { locals : signal list; exp : exp}
  type test = ident * ident option * exp option

  type valued_signal = {signal : signal ; svalue : atom}
  type valued_ident = {sname : ident ; fields : ident list; ivalue : exp}

  type label = Label of ident

  type 'a run_param = Sig_param of 'a | Exp_param of exp

  let filter_param f l =
    l |> List.fold_left (fun acc ->
        function Sig_param ({bind = No_binding} as x) -> (f x) :: acc | _ -> acc) []
    |> List.rev

  let mk_loc ?(loc=dummy_loc()) content = {loc; content}
  let mk_signal ?(origin=Local) ?(bind=No_binding) ?gatherer ident = {ident; origin; bind; gatherer}

  let mk_vid ?(fields=[]) sname ivalue = {sname; fields; ivalue}
  let mk_vsig signal locals exp = {signal; svalue = {locals; exp}}

  let mk_atom ?(locals = []) exp = {locals; exp}

  module Derived = struct
    type statement = (statement_tree) location
    and statement_tree =
      | Loop of statement
      | Seq of statement * statement
      | Par of statement * statement
      | Emit of valued_ident
      | Nothing
      | Pause
      | Suspend of statement * test
      | Trap of label * statement
      | Exit of label
      | Present of test * statement * statement
      | Atom of exp
      | Signal of valued_ident * statement
      | Run of ident * (ident * ident option) run_param list * loc

      | Halt
      | Sustain of valued_ident
      | Present_then of test * statement
      | Await of test
      | Await_imm of test
      | Suspend_imm of statement * test
      | Abort of statement * test
      | Weak_abort of statement * test
      | Loop_each of statement * test
      | Every of test * statement
  end


  type error = Unbound_identifier of string
  exception Error of Location.t * error
  let error ~loc e = raise (Error (loc, e))

  let print_error fmt e =
    let open Format in
    fprintf fmt "%s"
      begin match e with
        | Unbound_identifier s -> sprintf "Unbound signal %s" s
      end

  module IdentMap = Map.Make(struct
      type t = ident
      let compare a b = compare a.content b.content
    end)

  module SignalMap = Map.Make(struct
      type t = signal
      let compare a b = compare a.ident.content b.ident.content
    end)

  module SignalSet = Set.Make(struct
      type t = signal
      let compare a b = compare a.ident.content b.ident.content
    end)

  module IdentSet = Set.Make(struct
      type t = ident
      let compare a b = compare a.content b.content
    end)

  let trap_signal = Label (mk_loc "Trap")
  let (!+) a = incr a; !a


  module Tagged = struct

    type tlabel = TLabel of ident * int

    type t = {id : int; st : tagged}
    and test = signal * atom option
    and tagged_ast =
      | Loop of t
      | Seq of t * t
      | Par of t list
      | Emit of valued_signal
      | Nothing
      | Pause
      | Suspend of t * test
      | Trap of tlabel * t
      | Exit of tlabel
      | Present of test * t * t
      | Atom of atom
      | Signal of valued_signal * t
      | Await of test
      | Run of ident * signal run_param list * loc
    and tagged = (tagged_ast) location

    let mk_tagged ?(loc = dummy_loc()) content id =
      {id; st = {loc ; content}}

    type env = {
      pname : ident;
      args_signals : (signal * core_type option) list;
      nlabels : int ref;
      labels : int IdentMap.t;
      global_occurences : int IdentMap.t ref;
      scope : (int * signal_origin * signal_binder * gatherer) SignalMap.t;
      binders_env : (string, signal_binder list) Hashtbl.t;
      local_only_env : (valued_signal) list ref;
      local_only_scope : valued_signal list;
      machine_runs : (int * (int * signal run_param list) list) IdentMap.t ref;
    }

  let print_env fmt e =
    let open Format in
    fprintf fmt "====================\nglobal_occurences: ";
    IdentMap.iter (fun k v -> fprintf fmt "%s (%d), " k.content v) (!(e.global_occurences));
    fprintf fmt "\nmachine_runs: \n";
    IdentMap.iter (fun k (cntinst, insts) ->
        fprintf fmt "  mach(%s) (%d) : " k.content cntinst;
        List.iter (fun (int_id, _) -> fprintf fmt "%d, " int_id) insts;
        fprintf fmt "\n"
      ) (!(e.machine_runs));
    fprintf fmt "\n====================\n"

    let add_signal env locals vi =
      let scope, s' = match IdentMap.find vi.sname !(env.global_occurences) with
        | exception Not_found ->
          let s = mk_signal vi.sname in
          env.global_occurences := IdentMap.add vi.sname 0 !(env.global_occurences);
          SignalMap.add (mk_signal vi.sname) (0, Local, s.bind, s.gatherer) env.scope, s
        | i ->
          let s = mk_signal vi.sname in
          env.global_occurences := IdentMap.add vi.sname (succ i) !(env.global_occurences);
          SignalMap.add (mk_signal vi.sname) (succ i, Local, s.bind, s.gatherer) env.scope,
          {s with ident = {s.ident with content = Format.sprintf "%s~%d" s.ident.content (succ i)}}
      in
      let vs = {signal = s'; svalue = mk_atom ~locals vi.ivalue} in
      env.local_only_env := vs :: !(env.local_only_env);
      {env with scope; local_only_scope = vs :: env.local_only_scope}, vs


    let add_label env (Label s) = IdentMap.(
        let n = !(env.nlabels) in
        incr env.nlabels;
        { env with labels = add s n env.labels},
        TLabel ({s with content = Format.sprintf "%s%d" s.content n}, n)
      )

    let rename_ident env (Label s) p =
      IdentMap.(match find s env with
          | exception Not_found -> error ~loc:p.loc @@ Unbound_identifier s.content
          | n -> TLabel ({s with content = Format.sprintf "%s~%d" s.content n}, n)
        )

    let add_rename_machine env s args =
      IdentMap.(match find s !env with
          | exception Not_found -> env := add s (1, [0, args]) !env; s
          | i, insts ->
            env := add s (succ i, (i, args) :: insts) !env ;
            {s with content = Format.sprintf "%s~%d" s.content i})


    (** rename signals in tests or emits depending prevous signals (unique
        names) if the signal is tagged, it concats the tag to the name *)
    let rename ~loc env bind s =
      (* if the signal is known in the current scope *)
      SignalMap.(match find (mk_signal s) env.scope with
      (* fail, it should have been defined *)
       | exception Not_found -> error ~loc @@ Unbound_identifier s.content
       (* else, if its the first, it's probably a tagged signal *)
       | (0, origin, _, gatherer) ->
         begin match bind with
           (* if it's not tagged just create the signal*)
           | No_binding -> {ident = s; origin; bind=No_binding; gatherer}
           (* if it's tagged with an event, concat the event name with ## *)
           | Event (eident, gatherer) ->
             let tags = try Hashtbl.find env.binders_env s.content with Not_found -> [] in
             let is_binded = List.exists (function
                 | Event (id, _) -> id.content = eident.content
                 | _ -> false) tags
             in
             if not is_binded then
               Hashtbl.replace env.binders_env s.content (bind :: tags);
             let ident = mk_loc ~loc:s.loc (Format.sprintf "%s##%s" s.content eident.content) in
             {ident; origin = Input; bind; gatherer}
           (* if it's an access, concat with a##.b##.c *)
           | Access (elt, fields) ->
             let fields_str = List.fold_left (fun acc field ->
                 Format.sprintf "%s##.%s" acc field.content) elt.content fields
             in
             let ident = mk_loc ~loc:elt.loc (fields_str) in
             let tags = try Hashtbl.find env.binders_env s.content with Not_found -> [] in
             Hashtbl.replace env.binders_env s.content (bind :: tags);
             {ident; origin; bind; gatherer}
         end
       | (i, origin, _, gatherer) ->
         {ident = mk_loc ~loc:s.loc (Format.sprintf "%s~%d" s.content i);
          origin; bind = No_binding; gatherer})


    let create_env pname sigs binders = {
      nlabels = ref 0;
      pname;
      args_signals = sigs;
      labels = IdentMap.empty;
      global_occurences = ref IdentMap.(List.fold_left (fun accmap (s, _) ->
          add s.ident 0 accmap )
          empty sigs);

      scope = SignalMap.(List.fold_left (fun accmap (s, _) ->
          add s (0, s.origin, s.bind, s.gatherer) accmap )
          empty sigs);

      binders_env = begin
        let h = Hashtbl.create 19 in
        List.iter (fun (k, v) -> Hashtbl.add h k v) binders;
        h
      end;
      local_only_env = ref [];
      local_only_scope = [];
      machine_runs = ref IdentMap.empty
    }

    let rec extract_parallel l st1 st2 =
      let open Derived in
      match st1.content, st2.content with
      | Par (st11, st12), Par (st21, st22) ->
        extract_parallel (extract_parallel l st21 st22) st11 st12
      | Par (st11, st12), _ ->
        extract_parallel [st2] st11 st12
      | _, Par (st21, st22)  ->
        st1 :: extract_parallel l st21 st22
      | _, _ -> st1 :: st2 :: l


    let of_ast ?(sigs=[]) ?(binders=[]) pname ast =
      let id = ref 0 in
      let start_env = create_env pname sigs binders in
      let rec visit : env -> Derived.statement -> t = fun env ast ->
        let loc = ast.loc in
        let mk_tagged tagged = mk_tagged ~loc tagged in
        let mkl stmt = mk_loc ~loc stmt in
        match ast.content with

        | Derived.Loop t -> mk_tagged (Loop (visit env t)) !+id

        | Derived.Seq (st1, st2) ->
          let st1 = visit env st1 in
          let st2 = visit env st2 in
          mk_tagged (Seq (st1, st2)) !+id

        | Derived.Par (st1, st2) ->
          let sts = extract_parallel [] st1 st2 in
          mk_tagged (Par (List.map (visit env) sts)) !+id

        | Derived.Emit s ->
          let locals = List.map (fun x -> x.signal) env.local_only_scope in
          let typ = match s.fields with
            | [] -> No_binding
            | _ -> Access (s.sname, s.fields)
          in
          let s' = rename ~loc env typ s.sname in
          mk_tagged (Emit {signal = s';
                           svalue = mk_atom ~locals s.ivalue}) !+id

        | Derived.Nothing -> mk_tagged Nothing !+id
        | Derived.Pause -> mk_tagged Pause !+id

        | Derived.Await (s, tag, expopt) ->
          let typ = match tag with Some e -> Event (e, None) | None -> No_binding in
          let locals = (List.map (fun x -> x.signal) env.local_only_scope) in
          mk_tagged (Await (rename ~loc env typ s, Option.map (mk_atom ~locals) expopt)) !+id

        | Derived.Suspend (t, (s, tag, expopt)) ->
          let typ = match tag with Some e -> Event (e, None) | None -> No_binding in
          let s = rename ~loc:ast.loc env typ s in
          let locals = (List.map (fun x -> x.signal) env.local_only_scope) in
          mk_tagged (Suspend (visit env t, (s, Option.map (mk_atom ~locals) expopt))) !+id

        | Derived.(Trap (lbl, t)) ->
          let env, lbl = add_label env lbl in
          mk_tagged (Trap (lbl, visit env t)) !+id

        | Derived.Exit lbl ->
          mk_tagged (Exit (rename_ident env.labels lbl ast)) !+id

        | Derived.Present ((s, tag, expopt), t1, t2) ->
          let typ = match tag with Some e -> Event (e, None) | None -> No_binding in
          let s = rename ~loc env typ s in
          let locals = (List.map (fun x -> x.signal) env.local_only_scope) in
          mk_tagged (Present((s, Option.map (mk_atom ~locals) expopt), visit env t1, visit env t2)) !+id
        | Derived.Atom f -> mk_tagged (Atom (
            mk_atom ~locals:(List.map (fun x -> x.signal) env.local_only_scope) f
          )) !+id

        | Derived.Signal (vid,t) ->
          let locals = List.map (fun x -> x.signal) env.local_only_scope in
          let env, s' = add_signal env locals vid in
          mk_tagged (Signal (s', visit env t)) !+id

        | Derived.Run (mident, ids, loc) ->
          let sigs = List.map (function
              | Exp_param exp -> Exp_param exp
              | Sig_param (s, tag) ->
                let typ = match tag with Some e -> Event (e, None) | None -> No_binding in
                Sig_param (rename ~loc env typ s)
            ) ids
          in
          let machine_id =
            add_rename_machine env.machine_runs mident sigs
          in
          mk_tagged (Run (machine_id, sigs, loc)) !+id


        | Derived.Halt -> mk_tagged (Loop (mk_tagged Pause !+id)) !+id
        | Derived.Sustain s -> visit env Derived.(mkl @@ Loop ((mkl@@ Emit s)))
        | Derived.Present_then (test, st) ->
          visit env Derived.(mkl @@ Present (test, st, mkl @@ Nothing))
        | Derived.Await_imm s ->
          let env, lbl = add_label env trap_signal in
          mk_tagged (Trap (lbl,
                           mk_tagged (Loop (
                               mk_tagged (Seq (
                                   visit env Derived.(mkl @@ Present_then (s, mkl @@ Exit trap_signal)),
                                   mk_tagged Pause !+id)) !+id)) !+id)) !+id
        | Derived.Suspend_imm (st, s) ->
          visit env Derived.(mkl @@ Suspend ((
              mkl @@ Present_then (s, mkl @@ Seq (mkl Pause, st))), s))
        | Derived.Abort (st, s) ->
          let env, lbl = add_label env trap_signal in
          mk_tagged (Trap (lbl, mk_tagged (
              Par [
                mk_tagged (Seq (visit env (mkl @@ Derived.Suspend_imm (st, s)),
                                mk_tagged (Exit lbl) !+id)) !+id
                ; mk_tagged (Seq (visit env (mkl @@ Derived.Await s),
                                mk_tagged (Exit lbl) !+id)) !+id
              ]
            ) !+id)) !+id
        | Derived.Weak_abort (st, s) ->
          let env, lbl = add_label env trap_signal in
          mk_tagged (Trap (lbl, mk_tagged (
              Par [visit env st; mk_tagged (Seq (visit env (mkl @@ Derived.Await s), mk_tagged (Exit lbl) !+id))
                  !+id]) !+id)) !+id
        | Derived.Loop_each (st, s) ->
          mk_tagged (Loop (
              visit env Derived.(mkl @@ Abort (mkl @@ Seq (st, mkl Halt), s)))
            ) !+id
        | Derived.Every (s, st) ->
          mk_tagged (Seq (visit env (mkl @@ Derived.Await s), visit env
              (mkl @@ Derived.Loop_each (st, s)))) !+id

      in
      let tagged = visit start_env ast in
      tagged, start_env


    let pp_st fmt x =
      let open Format in
      match x.st.content with
        | Emit vs -> fprintf fmt "emit %s " vs.signal.ident.content
        | Nothing  -> fprintf fmt "nothing"
        | Pause  -> fprintf fmt "pause"
        | Exit (TLabel (s, _)) -> fprintf fmt "exit %s " s.content
        | Atom _ -> fprintf fmt "atom"
        | Await (s, _) -> fprintf fmt "await %s " s.ident.content
        | Run (id, sigs, _) ->
          fprintf fmt "run %s %s" id.content
            (String.concat " " @@ filter_param (fun x -> x.ident.content) sigs) 
        | Loop _ -> fprintf fmt "loop"
        | Signal (vs, _) -> fprintf fmt "signal(%s)" vs.signal.ident.content
        | Suspend (_, (s, _)) -> fprintf fmt "suspend %s "  s.ident.content
        | Trap (TLabel (s, _), _) -> fprintf fmt "trap %s "  s.content

        | Seq _ -> fprintf fmt "seq"
        | Par _ -> fprintf fmt "par"
        | Present ((s, _), _, _) ->
          fprintf fmt "present %s "  s.ident.content

    let pp_dot fmt tagged =
      let open Format in
      let pr x = fprintf fmt "N%d [label=\"%d %a\"];@\n" x.id x.id pp_st x in
      let edge dashed id1 id2 =
        fprintf fmt "N%d -> N%d %s;@\n" id1 id2
          (if dashed then "[style = dashed]" else "")
      in
      let rec visit x =
        pr x;
        match x.st.content with
        | Emit _ | Nothing | Pause | Exit _ | Atom _ | Run _ | Await _ -> ()
        | Suspend (st, _) | Trap (_, st) | Signal (_, st) | Loop st ->
          edge false x.id st.id;
          visit st
        | Seq (st1, st2) | Present (_, st1, st2) ->
          edge false x.id st1.id; edge true x.id st2.id;
          visit st1; visit st2
        | Par sts ->
          (* edge false x.id st1.id; edge false x.id st2.id; *)
          (* visit st1; visit st2 *)
          List.iter (fun st -> edge false x.id st.id; visit st) sts
      in
      fprintf fmt "@[<hov 2>digraph tagged {@\n";
      visit tagged;
      fprintf fmt "}@]\n@."


  end

  module Analysis = struct
    let rec blocking t =
      let open Tagged in
      match t.st.content with
      | Loop t -> blocking t
      | Seq (t1,t2) -> blocking t1 || blocking t2
      | Par ts -> List.exists blocking ts
      | Emit _ -> false
      | Nothing  -> false
      | Pause  -> true
      | Suspend (t,_) -> blocking t
      | Trap (_,t) -> blocking t
      | Exit _ -> false
      | Present (_,t1,t2) -> blocking t1 && blocking t2
      | Atom _ -> false
      | Signal (_,t) -> blocking t
      | Await _ -> true
      | Run _ -> true

    let rec non_blocking t =
      let open Tagged in
      match t.st.content with
      | Loop _ -> false
      | Seq (t1,t2) -> non_blocking t1 && non_blocking t2
      | Par ts -> List.for_all non_blocking ts
      | Emit _ -> true
      | Nothing  -> true
      | Pause  -> false
      | Suspend (t,_) -> non_blocking t
      | Trap (_,t) -> non_blocking t
      | Exit _ -> true
      | Present (_,t1,t2) -> non_blocking t1 && non_blocking t2
      | Atom _ -> true
      | Signal (_,t) -> non_blocking t
      | Await _  -> false
      | Run _ -> false

    let change t st =
      let open Tagged in
      {t with st = {t.st with content = st}}

    let filter_dead_trees : Tagged.t -> Tagged.t = fun t ->
      let open Tagged in
      let rec aux t =
        match t.st.content with
        | Loop t' ->
          let stuck, t' = aux t' in
          if stuck then true, t' else
            true, change t @@ Loop t'

        | Seq (t1,t2) ->
          let stuck, t1 = aux t1 in
          if stuck then true, t1 else
            let stuck, t2 = aux t2 in
            stuck, change t @@ Seq (t1, t2)

        | Par l ->
          let stucks, ts = List.(split (map (aux) l)) in
          List.exists (fun x -> x) stucks, change t @@ Par ts

        | Nothing | Await _ | Atom _ | Exit _ | Pause  | Emit _ | Run _ -> false, t

        | Suspend (t', s) ->
          let stuck, t' = aux t' in
          stuck, change t @@ Suspend (t', s)

        | Trap (l,t') ->
          let _, t' = aux t' in
          false, change t @@ Trap (l, t')
        | Present (s,t1,t2) ->
          let stuck1, t1 = aux t1 in
          let stuck2, t2 = aux t2 in
          stuck1 && stuck2, change t @@ Present (s, t1, t2)

        | Signal (s,t') ->
          let stuck, t' = aux t' in
          stuck, change t @@ Signal (s, t')

      in
      snd @@ aux t
  end


end
