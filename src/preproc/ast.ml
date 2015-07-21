

type 'a location = {
  loc : Location.t;
  content : 'a;
}
let dummy_loc = Location.none

type ident = string location

type signal_origin = Local | Input | Output

type signal = { ident : ident; origin : signal_origin }

type 'a valued_signal = {signal : signal ; value : 'a}
type 'a valued_ident = {sname : ident ; value : 'a}
type label = Label of ident

type atom = { locals : signal list; exp : Parsetree.expression}

let mk_loc ?(loc=dummy_loc) content = {loc; content}
let mk_signal ?(origin=Local) ident = {ident; origin}

let mk_vid sname value = {sname; value}
let mk_vsig signal value = {signal; value}

let mk_atom ?(locals = []) exp = {locals; exp}

module Derived = struct
  type statement = statement_tree location
  and statement_tree =
  | Loop of statement
  | Seq of statement * statement
  | Par of statement * statement
  | Emit of Parsetree.expression valued_ident
  | Nothing
  | Pause
  | Suspend of statement * ident
  | Trap of label * statement
  | Exit of label
  | Present of ident * statement * statement
  | Atom of Parsetree.expression
  | Signal of Parsetree.expression valued_ident * statement

  | Halt
  | Sustain of Parsetree.expression valued_ident
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

let trap_signal = Label  (mk_loc "Trap")
let (!+) a = incr a; !a


module Tagged = struct

  type t = {id : int; st : tagged}


  and tagged_ast =
    | Loop of t
    | Seq of t * t
    | Par of t * t
    | Emit of Parsetree.expression valued_signal
    | Nothing
    | Pause
    | Suspend of t * signal
    | Trap of label * t
    | Exit of label
    | Present of signal * t * t
    | Atom of atom [@printer fun fmt x -> Pprintast.expression fmt x.exp]
    | Signal of Parsetree.expression valued_signal * t
    | Await of signal
  and tagged = tagged_ast location

  let mk_tagged ?(loc = dummy_loc) content id =
    {id; st = {loc ; content}}

  type env = {
    labels : int IdentMap.t;
    global_namespace : int SignalMap.t ref;
    signals : (int * signal_origin) SignalMap.t;
    all_local_signals : Parsetree.expression valued_signal list ref;
    local_signals : Parsetree.expression valued_signal list;
  }

  let add_signal (env : env) vi =
    let signals, s' = SignalMap.(match find (mk_signal vi.sname) env.signals with
      | exception Not_found ->

        let i, sname' = match find (mk_signal vi.sname) !(env.global_namespace) with
          | exception Not_found -> 0, vi.sname
          | i -> i, {vi.sname with content = Format.sprintf "%s~%d" vi.sname.content (i + 1)}
        in
        let s = mk_signal sname' in
        env.global_namespace := add s (i + 1) !(env.global_namespace);
        add (mk_signal vi.sname) (i, Local) env.signals, s

      | (i, orig) ->
        let i = match find (mk_signal vi.sname) !(env.global_namespace) with
          | exception Not_found -> assert false
          | i -> i
        in
        let s = mk_signal vi.sname in
        env.global_namespace := add s (i + 1) !(env.global_namespace);
        add s ((i + 1), Local) env.signals,
        {s with ident = {s.ident with content = Format.sprintf "%s~%d" s.ident.content (i + 1)}}
      )
    in
    let s' = {signal = s'; value = vi.value} in
    env.all_local_signals := s' :: !(env.all_local_signals);
    let env, s' = {env with signals; local_signals = s' :: env.local_signals}, s' in
    env, s'


  let add_label env s = IdentMap.(match find s env with
      | exception Not_found -> add s 0 env, s
      | i -> add s (i + 1) env,
             {s with content = Format.sprintf "%s%d" s.content (i + 1)})

  let rename_label env s p =
    IdentMap.(match find s env with
        | exception Not_found ->
          error ~loc:p.loc @@ Unbound_identifier s.content
        | 0 -> s
        | i -> {s with content = Format.sprintf "%s~%d" s.content i})

  let rename env s p =
    SignalMap.(match find (mk_signal s) env with
        | exception Not_found ->
          error ~loc:p.loc @@ Unbound_identifier s.content
        | (0, origin) -> {ident = s; origin}
        | (i, origin) -> {ident = mk_loc ~loc:s.loc (Format.sprintf "%s~%d" s.content i); origin})

  let create_env sigs = {
    labels = IdentMap.empty;
    global_namespace = ref (List.fold_left (fun accmap s ->
        SignalMap.add s 0 accmap )
        SignalMap.empty sigs);

    signals = List.fold_left (fun accmap s ->
        SignalMap.add s (0, s.origin) accmap )
        SignalMap.empty sigs;

    all_local_signals = ref [];
    local_signals = [];
  }

  let rec of_ast ?(sigs=[]) ast =
    let id = ref 0 in
    let start_env = create_env sigs in
    let rec visit : env -> Derived.statement -> t = fun env ast ->
      let mk_tagged tagged = mk_tagged ~loc:ast.loc tagged in
      let mkl stmt = mk_loc ~loc:ast.loc stmt in
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

      | Derived.Emit s -> mk_tagged (Emit {signal = (rename env.signals s.sname ast); value = s.value}) !+id
      | Derived.Nothing -> mk_tagged Nothing !+id
      | Derived.Pause -> mk_tagged Pause !+id

      | Derived.Await s -> mk_tagged (Await (rename env.signals s ast)) !+id

      | Derived.Suspend (t, s) ->
        let s = rename env.signals s ast in
        mk_tagged (Suspend (visit env t, s)) !+id

      | Derived.Trap (Label s, t) ->
        let labels, s = add_label env.labels s in
        mk_tagged (Trap (Label s, visit {env with labels} t)) !+id

      | Derived.Exit (Label s) ->
        mk_tagged (Exit (Label (rename_label env.labels s ast))) !+id

      | Derived.Present (s, t1, t2) ->
        let s = rename env.signals s ast in
        mk_tagged (Present(s, visit env t1, visit env t2)) !+id
      | Derived.Atom f -> mk_tagged (Atom {
          locals = List.map (fun x -> x.signal) env.local_signals;
          exp = f
        }) !+id

      | Derived.Signal (s,t) ->
        let env, s = add_signal env s in
        mk_tagged (Signal (s, visit env t)) !+id


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
end
