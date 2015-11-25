open Utils

module type Location = sig
  type t
  val none : t
end

module type Exp = sig
  type t
  val print : Format.formatter -> t -> unit
  module Location : Location
end

module type S = sig

  type loc
  type exp

  val printexp : Format.formatter -> exp -> unit

  type 'a location = {
    loc : loc;
    content : 'a;
  }

  type ident = string location

  type signal_origin = Local | Input | Output

  type signal = { ident : ident; origin : signal_origin; tag : string option}
  type label = Label of ident
  type atom = { locals : signal list; exp : exp}

  type valued_signal = {signal : signal ; svalue : atom}
  type valued_ident = {sname : ident ; ivalue : exp}
  val mk_signal : ?origin:signal_origin -> ident -> signal

  val mk_vsig : signal -> signal list -> exp -> valued_signal
  val mk_vid : ident -> exp -> valued_ident

  val mk_atom : ?locals:signal list -> exp -> atom

  module IdentMap : Map.S with type key = ident
  module IdentSet : Set.S with type elt = ident
  module SignalMap : Map.S with type key = signal
  module SignalSet : Set.S with type elt = signal

  val dummy_loc : loc
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
      | Suspend of statement * ident
      | Trap of label * statement
      | Exit of label
      | Present of (ident * string) * statement * statement
      | Atom of exp
      | Signal of valued_ident * statement
      | Run of ident * ident list * loc

      | Halt
      | Sustain of valued_ident
      | Present_then of (ident * ident) * statement
      | Await of ident
      | Await_imm of ident
      | Suspend_imm of statement * ident
      | Abort of statement * ident
      | Weak_abort of statement * ident
      | Loop_each of statement * ident
      | Every of ident * statement
  end

  type error =
    | Unbound_identifier of string
  exception Error of loc * error
  val error : loc:loc -> error -> 'a
  val print_error : Format.formatter -> error -> unit

  module Tagged : sig

    type t = {id : int; st : tagged}
    and tagged_ast =
      | Loop of t
      | Seq of t * t
      | Par of t * t
      | Emit of valued_signal
      | Nothing
      | Pause
      | Suspend of t * signal
      | Trap of label * t
      | Exit of label
      | Present of signal * t * t
      | Atom of atom
      | Signal of valued_signal * t
      | Await of signal
      | Run of ident * signal list * loc
    and tagged = (tagged_ast) location


    type env = {
      labels : int IdentMap.t;
      global_scope : int IdentMap.t ref;
      signals : (int * signal_origin * string option) SignalMap.t;
      signals_tags : (string, string list) Hashtbl.t;
      local_signals : (valued_signal) list ref;
      local_scope : valued_signal list;
      machine_runs : (int * (int * signal list) list) IdentMap.t ref;
    }

    val print_env : Format.formatter -> env -> unit

    val of_ast : ?sigs:(signal list) -> Derived.statement -> t * env

    val print_to_dot : Format.formatter -> t -> unit
  end

  module Analysis : sig
    val blocking : Tagged.t -> bool
    val filter_dead_trees : Tagged.t -> Tagged.t
  end

end

module Make (E : Exp) = struct

  module Location = E.Location

  type loc = Location.t
  let printexp = E.print
  type exp = E.t

  type 'a location = {
    loc : loc;
    content : 'a;
  }
  let dummy_loc = Location.none

  type ident = string location

  type signal_origin = Local | Input | Output

  type signal = { ident : ident; origin : signal_origin; tag  : string option}

  type atom = { locals : signal list; exp : exp}

  type valued_signal = {signal : signal ; svalue : atom}
  type valued_ident = {sname : ident ; ivalue : exp}
  type label = Label of ident


  let mk_loc ?(loc=dummy_loc) content = {loc; content}
  let mk_signal ?(origin=Local) ident = {ident; origin; tag = None}

  let mk_vid sname ivalue = {sname; ivalue}
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
      | Suspend of statement * ident
      | Trap of label * statement
      | Exit of label
      | Present of (ident * ident) * statement * statement
      | Atom of exp
      | Signal of valued_ident * statement
      | Run of ident * ident list * loc

      | Halt
      | Sustain of valued_ident
      | Present_then of ident * statement
      | Await of ident
      | Await_imm of ident
      | Suspend_imm of statement * ident
      | Abort of statement * ident
      | Weak_abort of statement * ident
      | Loop_each of statement * ident
      | Every of ident * statement
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

    type t = {id : int; st : tagged}

    and tagged_ast =
      | Loop of t
      | Seq of t * t
      | Par of t * t
      | Emit of valued_signal
      | Nothing
      | Pause
      | Suspend of t * signal
      | Trap of label * t
      | Exit of label
      | Present of signal * t * t
      | Atom of atom
      | Signal of valued_signal * t
      | Await of signal
      | Run of ident * signal list * loc
    and tagged = (tagged_ast) location

    let mk_tagged ?(loc = dummy_loc) content id =
      {id; st = {loc ; content}}

    type env = {
      labels : int IdentMap.t;
      global_scope : int IdentMap.t ref;
      signals : (int * signal_origin * string option) SignalMap.t;
      signals_tags : (string, string list) Hashtbl.t;
      local_signals : (valued_signal) list ref;
      local_scope : valued_signal list;
      machine_runs : (int * (int * signal list) list) IdentMap.t ref;
    }

  let print_env fmt e =
    let open Format in
    fprintf fmt "====================\nglobal_scope: ";
    IdentMap.iter (fun k v -> fprintf fmt "%s (%d), " k.content v) (!(e.global_scope));
    fprintf fmt "\nmachine_runs: \n";
    IdentMap.iter (fun k (cntinst, insts) ->
        fprintf fmt "  mach(%s) (%d) : " k.content cntinst;
        List.iter (fun (int_id, args) -> fprintf fmt "%d, " int_id) insts;
        fprintf fmt "\n"
      ) (!(e.machine_runs));
    fprintf fmt "\n====================\n"

    let add_signal env locals vi =
      let signals, s' = match IdentMap.find vi.sname !(env.global_scope) with
        | exception Not_found ->
          let s = mk_signal vi.sname in
          env.global_scope := IdentMap.add vi.sname 0 !(env.global_scope);
          SignalMap.add (mk_signal vi.sname) (0, Local, s.tag) env.signals, s
        | i ->
          let s = mk_signal vi.sname in
          env.global_scope := IdentMap.add vi.sname (succ i) !(env.global_scope);
          SignalMap.add (mk_signal vi.sname) (succ i, Local, s.tag) env.signals,
          {s with ident = {s.ident with content = Format.sprintf "%s~%d" s.ident.content (succ i)}}
      in
      let vs = {signal = s'; svalue = mk_atom ~locals vi.ivalue} in
      env.local_signals := vs :: !(env.local_signals);
      {env with signals; local_scope = vs :: env.local_scope}, vs


    let add_label env s = IdentMap.(match find s env with
        | exception Not_found -> add s 0 env, s
        | i -> add s (i + 1) env,
               {s with content = Format.sprintf "%s%d" s.content (i + 1)})

    let rename_ident env s p =
      IdentMap.(match find s env with
          | exception Not_found ->
            error ~loc:p.loc @@ Unbound_identifier s.content
          | 0 -> s
          | i -> {s with content = Format.sprintf "%s~%d" s.content i})

    let add_rename_machine env s args =
      IdentMap.(match find s !env with
          | exception Not_found -> env := add s (1, [0, args]) !env; s
          | i, insts ->
            env := add s (succ i, (i, args) :: insts) !env ;
            {s with content = Format.sprintf "%s~%d" s.content i})

    let rename ~loc env tag s =
      SignalMap.(match find (mk_signal s) env.signals with
       | exception Not_found -> error ~loc @@ Unbound_identifier s.content
       | (0, origin, _) ->
          begin match tag with
          | Some ext ->
             let tags = try Hashtbl.find env.signals_tags s.content with Not_found -> [] in
             Hashtbl.add env.signals_tags s.content (ext.content :: tags);
             let ident = mk_loc ~loc:s.loc (Format.sprintf "%s##%s" s.content ext.content) in
             {ident; origin; tag = Some ext.content}
          | None -> {ident = s; origin; tag = None}
          end
       | (i, origin, _) ->
             {ident = mk_loc ~loc:s.loc (Format.sprintf "%s~%d" s.content i); origin; tag = None})


    let create_env sigs = {
      labels = IdentMap.empty;
      global_scope = ref IdentMap.(List.fold_left (fun accmap s ->
          add s.ident 0 accmap )
          empty sigs);

      signals = SignalMap.(List.fold_left (fun accmap s ->
          add s (0, s.origin, s.tag) accmap )
          empty sigs);

      signals_tags = Hashtbl.create 19;
      local_signals = ref [];
      local_scope = [];
      machine_runs = ref IdentMap.empty
    }

    let rec of_ast ?(sigs=[]) ast =
      let id = ref 0 in
      let start_env = create_env sigs in
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
          let st1 = visit env st1 in
          let st2 = visit env st2 in
          mk_tagged (Par (st1, st2)) !+id

        | Derived.Emit s ->
          let locals = (List.map (fun x -> x.signal) env.local_scope) in
          let s' = rename ~loc env None s.sname in
          mk_tagged (Emit {signal = s'; svalue = mk_atom ~locals s.ivalue}) !+id

        | Derived.Nothing -> mk_tagged Nothing !+id
        | Derived.Pause -> mk_tagged Pause !+id

        | Derived.Await s -> mk_tagged (Await (rename ~loc env None s)) !+id

        | Derived.Suspend (t, s) ->
          let s = rename ~loc:ast.loc env None s in
          mk_tagged (Suspend (visit env t, s)) !+id

        | Derived.Trap (Label s, t) ->
          let labels, s = add_label env.labels s in
          mk_tagged (Trap (Label s, visit {env with labels} t)) !+id

        | Derived.Exit (Label s) ->
          mk_tagged (Exit (Label (rename_ident env.labels s ast))) !+id

        | Derived.Present ((s, tag), t1, t2) ->
          let s = rename ~loc env (Some tag) s in
          mk_tagged (Present(s, visit env t1, visit env t2)) !+id
        | Derived.Atom f -> mk_tagged (Atom (
            mk_atom ~locals:(List.map (fun x -> x.signal) env.local_scope) f
          )) !+id

        | Derived.Signal (vid,t) ->
          let locals = List.map (fun x -> x.signal) env.local_scope in
          let env, s' = add_signal env locals vid in
          mk_tagged (Signal (s', visit env t)) !+id

        | Derived.Run (mident, ids, loc) ->
          let sigs = List.map (rename ~loc env None) ids in
          let machine_id = add_rename_machine env.machine_runs mident sigs in
          mk_tagged (Run (machine_id, sigs, loc)) !+id


        | Derived.Halt -> mk_tagged (Loop (mk_tagged Pause !+id)) !+id
        | Derived.Sustain s -> visit env Derived.(mkl @@ Loop ((mkl@@ Emit s)))
        | Derived.Present_then (s, st) ->
          visit env Derived.(mkl @@ Present (s, st, mkl @@ Nothing))
        | Derived.Await_imm s ->
          mk_tagged (Trap (trap_signal,
                           mk_tagged (Loop (
                               mk_tagged (Seq (
                                   visit env Derived.(mkl @@ Present_then (s, mkl @@ Exit trap_signal)),
                                   mk_tagged Pause !+id)) !+id)) !+id)) !+id
        | Derived.Suspend_imm (st, s) ->
          visit env Derived.(mkl @@ Suspend ((
              mkl @@ Present_then (s, mkl @@ Seq (mkl Pause, st))), s))
        | Derived.Abort (st, s) ->
          mk_tagged (Trap (trap_signal, mk_tagged (
              Par (mk_tagged (
                  Seq (visit env (mkl @@ Derived.Suspend_imm (st, s)),
                       mk_tagged (Exit trap_signal) !+id)
                ) !+id,
                   mk_tagged (Seq (visit env (mkl @@ Derived.Await s), mk_tagged (Exit trap_signal) !+id)) !+id)
            ) !+id)) !+id
        | Derived.Weak_abort (st, s) ->
          mk_tagged (Trap (trap_signal, mk_tagged (
              Par (visit env st, mk_tagged (
                  Seq (visit env (mkl @@ Derived.Await s), mk_tagged (Exit trap_signal) !+id))
                  !+id)) !+id)) !+id
        | Derived.Loop_each (st, s) ->
          mk_tagged (Loop (
              visit env Derived.(mkl @@ Abort (mkl @@ Seq (st, mkl Halt), s)))
            ) !+id
        | Derived.Every (s, st) -> mk_tagged (Seq (visit env (mkl @@ Derived.Await s), visit env
                                                     (mkl @@ Derived.Loop_each (st, s)))) !+id

      in
      let tagged = visit start_env ast in
      tagged, start_env

    let print_to_dot fmt tagged =
      let open Format in
      let rec visit x = match x.st.content with
        | Loop st ->
          fprintf fmt "N%d [label=\"%d loop\"];@\n" x.id x.id;
          fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
          visit st
        | Seq (st1, st2) ->
          fprintf fmt " N%d [label=\"%d seq\"];@\n" x.id x.id;
          fprintf fmt "N%d -> N%d ;@\n" x.id st1.id;
          fprintf fmt "N%d -> N%d ;@\n" x.id st2.id;
          visit st1;
          visit st2;
        | Par (st1, st2) ->
          fprintf fmt "N%d [label=\"%d par\"]; @\n" x.id x.id;
          fprintf fmt "N%d -> N%d ;@\n" x.id st1.id;
          fprintf fmt "N%d -> N%d ;@\n" x.id st2.id;
          visit st1;
          visit st2;
        | Emit vs -> fprintf fmt "N%d [label=\"%d emit(%s)\"];@\n"  x.id x.id vs.signal.ident.content
        | Nothing  ->
          fprintf fmt "N%d [label=\"%d nothing\"]; @\n" x.id x.id
        | Pause  ->
          fprintf fmt "N%d [label=\"%d pause\"]; @\n" x.id x.id
        | Suspend (st, s) ->
          fprintf fmt "N%d [label=\"%d suspend(%s)\"]; @\n" x.id x.id s.ident.content;
          fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
          visit st
        | Trap (Label s, st) ->
          fprintf fmt "N%d [label=\" %d trap(%s)\"]; @\n" x.id x.id s.content;
          fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
          visit st
        | Exit (Label s) -> fprintf fmt "N%d [label=\"%d exit(%s)\"]; @\n" x.id x.id s.content
        | Atom f -> fprintf fmt "N%d [label=\"%d atom\"]; @\n" x.id x.id
        | Present (s, st1, st2) ->
          fprintf fmt "N%d [label=\"%d present(%s)\"]; @\n" x.id x.id s.ident.content;
          fprintf fmt "N%d -> N%d ;@\n" x.id st1.id;
          fprintf fmt "N%d -> N%d ;@\n" x.id st2.id;
          visit st1;
          visit st2;
        | Await s ->
          fprintf fmt "N%d [label=\"%d await(%s)\"]; @\n" x.id x.id s.ident.content
        | Signal (vs, st) ->
          fprintf fmt "N%d [label=\"%d signal(%s)\"]; @\n" x.id x.id vs.signal.ident.content;
          fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
          visit st
        | Run (id, sigs, _) ->
          fprintf fmt "N%d [label=\"%d run %s %s\"]; @\n" x.id x.id id.content
            (String.concat " " @@ List.map (fun x -> x.ident.content) sigs) 

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
      | Par (t1,t2) -> blocking t1 || blocking t2
      | Emit _ -> false
      | Nothing  -> false
      | Pause  -> true
      | Suspend (t,_) -> blocking t
      | Trap (_,t) -> blocking t
      | Exit _ -> false
      | Present (_,t1,t2) -> blocking t1 || blocking t2
      | Atom _ -> false
      | Signal (_,t) -> blocking t
      | Await s -> true
      | Run _ -> true

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

        | Par (t1,t2) ->
          let stuck1, t1 = aux t1 in
          let stuck2, t2 = aux t2 in
          stuck1 || stuck2, change t @@ Par (t1, t2)

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
