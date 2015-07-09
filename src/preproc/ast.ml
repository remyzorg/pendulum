

type 'a location = {
  loc : Location.t;
  content : 'a;
}

type ident = string location


type signal = ident
type 'a valued_signal = {ident : ident; value : 'a}
type label = Label of ident

type atom = { locals : signal list; exp : Parsetree.expression}

let dummy_loc = Location.none
let mk_loc ?(loc=dummy_loc) content = {loc; content}
let mk_vsig ident value = {ident; value}

module Derived = struct
  type statement = statement_tree location
  and statement_tree =
  | Loop of statement
  | Seq of statement * statement
  | Par of statement * statement
  | Emit of Parsetree.expression valued_signal
  | Nothing
  | Pause
  | Suspend of statement * signal
  | Trap of label * statement
  | Exit of label
  | Present of signal * statement * statement
  | Atom of Parsetree.expression
  | Signal of Parsetree.expression valued_signal * statement

  | Halt
  | Sustain of Parsetree.expression valued_signal
  | Present_then of signal * statement
  | Await of signal
  | Await_imm of signal
  | Suspend_imm of statement * signal
  | Abort of statement * signal
  | Weak_abort of statement * signal
  | Loop_each of statement * signal
  | Every of signal * statement
end


type error = Unbound_identifier of string | Syntax
exception Error of Location.t * error
let error ~loc e = raise (Error (loc, e))

let print_error fmt e =
  let open Format in
  fprintf fmt "%s"
    begin match e with
      | Unbound_identifier s -> sprintf "unbound signal %s" s
      | Syntax -> "Syntax error"
    end

let syntax_error ~loc () = raise (Location.Error (
    Location.error ~loc ("[pendulum] Syntax error")))

let syntax_error_reason ~loc s = raise (Location.Error (
    Location.error ~loc ("[pendulum] Syntax error : " ^ s)))


module IntMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)

module IdentMap = Map.Make(struct
    type t = ident
    let compare a b = compare a.content b.content
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
    signals : int IdentMap.t;
    mutable all_local_signals : Parsetree.expression valued_signal list;
    local_signals : Parsetree.expression valued_signal list;
  }

  let add_env (env : env) s =
    let signals, s' = IdentMap.(match find s.ident env.signals with
      | exception Not_found ->
        add s.ident 0 env.signals, s.ident
      | i ->
        add s.ident (i + 1) env.signals,
        {s.ident with content = Format.sprintf "%s~%d" s.ident.content (i + 1)}
      )
    in
    env.all_local_signals <- {s with ident = s'} :: env.all_local_signals;
    {env with signals; local_signals = {s with ident = s'} :: env.local_signals}, s

  let add_label env s = IdentMap.(match find s env with
      | exception Not_found -> add s 0 env, s
      | i -> add s (i + 1) env,
             {s with content = Format.sprintf "%s%d" s.content (i + 1)})

  let rename env s p =
    IdentMap.(match find s env with
        | exception Not_found ->
          iter (fun a b -> Format.printf "%s %d @\n" a.content b) env;
          error ~loc:p.loc @@ Unbound_identifier s.content
        | 0 -> s
        | i -> {s with content = Format.sprintf "%s~%d" s.content i})

  let create_env sigs = {
    labels = IdentMap.empty;
    signals = List.fold_left (fun accmap s ->
        IdentMap.add s.ident 0 accmap )
        IdentMap.empty sigs;
    all_local_signals = [];
    local_signals = [];
  }

  let rec of_ast ?(sigs=[]) ast =
    let id = ref 0 in
    let env = create_env sigs in
    let rec visit : env -> Derived.statement -> t = fun env ast ->
      let mk_tagged tagged = mk_tagged ~loc:ast.loc tagged in
      let mkl stmt = mk_loc ~loc:ast.loc stmt in
      match ast.content with
      | Derived.Loop t -> mk_tagged (Loop (visit env t)) !+id

      | Derived.Seq (st1, st2) ->
        mk_tagged (Seq (visit env st1, visit env st2)) !+id

      | Derived.Par (st1, st2) ->
        mk_tagged (Par (visit env st1, visit env st2)) !+id

      | Derived.Emit s -> mk_tagged (Emit {s with ident = (rename env.signals s.ident ast)}) !+id
      | Derived.Nothing -> mk_tagged Nothing !+id
      | Derived.Pause -> mk_tagged Pause !+id

      | Derived.Await s -> mk_tagged (Await (rename env.signals s ast)) !+id

      | Derived.Suspend (t, s) ->
        mk_tagged (Suspend (visit env t, rename env.signals s ast)) !+id

      | Derived.Trap (Label s, t) ->
        let labels, s = add_label env.labels s in
        mk_tagged (Trap (Label s, visit {env with labels} t)) !+id

      | Derived.Exit (Label s) -> mk_tagged (Exit (Label (rename env.labels s ast))) !+id
      | Derived.Present (s, t1, t2) ->
        mk_tagged (Present(
            rename env.signals s ast, visit env t1, visit env t2))
          !+id
      | Derived.Atom f -> mk_tagged (Atom {
          locals = List.map (fun x -> x.ident) env.local_signals;
          exp = f
        }) !+id

      | Derived.Signal (s,t) ->
        let env, s = add_env env s in
        mk_tagged (Signal (s, visit env t)) !+id


      | Derived.Halt -> mk_tagged (Loop (mk_tagged Pause !+id)) !+id
      | Derived.Sustain s -> mk_tagged (Loop (mk_tagged (Emit s) !+id)) !+id
      | Derived.Present_then (s, st) -> mk_tagged (Present (s, (visit env st), mk_tagged Nothing !+id) ) !+id
      | Derived.Await_imm s ->
        mk_tagged (Trap (trap_signal,
              mk_tagged (Loop (
                  mk_tagged (Seq (
                    visit env Derived.(mkl @@ Present_then (s, mkl @@ Exit trap_signal)),
                    mk_tagged Pause !+id)) !+id)) !+id)) !+id
      | Derived.Suspend_imm (st, s) ->
        mk_tagged (Suspend (visit env Derived.(
          mkl @@ Present_then (s, mkl @@ Seq (mkl Pause, st))), s)) !+id
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
    let tagged = visit env ast in
    tagged, env

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
    | Emit s -> fprintf fmt "N%d [label=\"%d emit(%s)\"];@\n"  x.id x.id s.ident.content
    | Nothing  ->
      fprintf fmt "N%d [label=\"%d nothing\"]; @\n" x.id x.id
    | Pause  ->
      fprintf fmt "N%d [label=\"%d pause\"]; @\n" x.id x.id
    | Suspend (st, s) ->
      fprintf fmt "N%d [label=\"%d suspend(%s)\"]; @\n" x.id x.id s.content;
      fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
      visit st
    | Trap (Label s, st) ->
      fprintf fmt "N%d [label=\" %d trap(%s)\"]; @\n" x.id x.id s.content;
      fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
      visit st
    | Exit (Label s) -> fprintf fmt "N%d [label=\"%d exit(%s)\"]; @\n" x.id x.id s.content
    | Atom f -> fprintf fmt "N%d [label=\"%d atom\"]; @\n" x.id x.id
    | Present (s, st1, st2) ->
      fprintf fmt "N%d [label=\"%d present(%s)\"]; @\n" x.id x.id s.content;
      fprintf fmt "N%d -> N%d ;@\n" x.id st1.id;
      fprintf fmt "N%d -> N%d ;@\n" x.id st2.id;
      visit st1;
      visit st2;
    | Await s ->
      fprintf fmt "N%d [label=\"%d await(%s)\"]; @\n" x.id x.id s.content
    | Signal (s, st) ->
      fprintf fmt "N%d [label=\"%d signal(%s)\"]; @\n" x.id x.id s.ident.content;
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
