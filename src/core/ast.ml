

type signal = string [@@deriving show]
type label = Label of string [@@deriving show]


type statement =
  | Loop of statement
  | Seq of statement list
  | Par of statement list
  | Emit of signal
  | Nothing
  | Pause
  | Suspend of statement * signal
  | Trap of label * statement
  | Exit of label
  | Present of signal * statement * statement
  | Atom of (unit -> unit)
  | Signal of signal * statement
  | Await of signal
[@@deriving show]

type dstatement =
  | Loop of dstatement
  | Seq of dstatement list
  | Par of dstatement list
  | Emit of signal
  | Nothing
  | Pause
  | Suspend of dstatement * signal
  | Trap of label * dstatement
  | Exit of label
  | Present of signal * dstatement * dstatement
  | Atom of (unit -> unit)

  | Signal of signal * dstatement

  | Halt
  | Sustain of signal
  | Present_then of signal * dstatement
  | Await of signal
  | Await_imm of signal
  | Suspend_imm of dstatement * signal
  | Abort of dstatement * signal
  | Weak_abort of dstatement * signal
  | Loop_each of dstatement * signal
  | Every of signal * dstatement
[@@deriving show]


type error = Unbound_identifier of string * statement
exception Error of error
let error e = raise @@ Error e

let print_error fmt e =
  let open Format in
  fprintf fmt "%s"
    begin match e with
      | Unbound_identifier (s, p) -> sprintf "unbound signal %s in %s" s
                                       (show_statement p)
    end

module SignalSet = Set.Make (struct
    type t = signal
    let compare = compare
  end)

module IntMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)




let trap_signal = Label "Trap"

let rec normalize : dstatement -> statement = function
  | Emit ds -> Emit ds
  | Exit lbl -> Exit lbl
  | Pause -> Pause
  | Nothing -> Nothing
  | Atom f -> Atom f
  | Await s -> Await s

  | Loop st -> Loop (normalize st)
  | Seq sts -> Seq (List.map normalize sts)
  | Par sts -> Par (List.map normalize sts)
  | Suspend (st, s) -> Suspend (normalize st, s)
  | Present (s, st1, st2) -> Present (s, normalize st1, normalize st2)
  | Trap (lbl, st) -> Trap (lbl, normalize st)
  | Signal (s, st) -> Signal (s, normalize st)

  | Halt -> Loop Pause
  | Sustain s -> Loop (Emit s)
  | Present_then (s, st) -> Present (s, (normalize st), Nothing)
  | Await_imm s ->
    Trap (trap_signal, (Loop (Seq [normalize (Present_then (s, (Exit trap_signal))); Pause])))
  | Suspend_imm (st, s) -> Suspend (normalize (Present_then (s, Seq [Pause; st])), s)
  | Abort (st, s) -> Trap (trap_signal, Par [Seq [normalize (Suspend_imm (st, s)); Exit trap_signal];
                            Seq [normalize (Await s); Exit trap_signal]])
  | Weak_abort (st, s) ->
    Trap (trap_signal, Par [normalize st; Seq [normalize (Await s); Exit trap_signal]])
  | Loop_each (st, s) -> Loop (normalize (Abort (Seq [st; Halt], s)))
  | Every (s, st) -> Seq [normalize (Await s); normalize (Loop_each (st, s))]


let (!+) a = incr a; !a


let normalize_await : dstatement -> statement = function
  | Await s ->
    Trap (trap_signal, (Loop (Seq [
        Pause;
        normalize (Present_then (s, (Exit trap_signal)))
      ])))
  | _ -> assert false

module Tagged = struct

  type t = {id : int; st : tagged}
  [@@deriving show]

  and tagged =
    | Loop of t
    | Seq of t * t
    | Par of t * t
    | Emit of signal
    | Nothing
    | Pause
    | Suspend of t * signal
    | Trap of label * t
    | Exit of label
    | Present of signal * t * t
    | Atom of (unit -> unit)
    | Signal of signal * t
    | Await of signal
  [@@deriving show]

  let mk_tagged st id = {id = id; st = st}

  type env = {
    labels : int StringMap.t;
    signals : int StringMap.t;
  }

  let empty_env = {labels = StringMap.empty; signals = StringMap.empty}

  let add_env env s = StringMap.(match find s env with
      | exception Not_found -> add s 0 env, s
      | i -> add s (i + 1) env, Format.sprintf "%s%d" s (i + 1))

  let rename env s p = StringMap.(match find s env with
      | exception Not_found ->
        StringMap.iter (fun a b -> Format.printf "%s %d @\n" a b) env;
        error @@ Unbound_identifier (s,  p)
      | 0 -> s
      | i -> Format.sprintf "%s%d" s i)


  let create_env sigs = {
    labels = StringMap.empty;
    signals = List.fold_left (fun accmap s ->
        StringMap.add s 0 accmap )
        StringMap.empty sigs
  }


  let rec of_ast ?env:(env=[]) ast =
    let id = ref 0 in
    let env = create_env env in

    let rec visit : env -> statement -> t = fun env ast ->
      match ast with
      | Loop t -> mk_tagged (Loop (visit env t)) !+id

      | Seq [] -> mk_tagged Nothing !+id
      | Seq [e] -> visit env e
      | Seq (h :: t) ->
        let id = !+id in
        let fs = visit env h in
        let sn = visit env (Seq t) in
        mk_tagged (Seq (fs, sn)) id



      | Par [] -> mk_tagged Nothing !+id
      | Par [e] -> visit env e
      | Par (h :: t) ->
        let id = !+id in
        let fs = visit env h in
        let sn = visit env (Par t) in
        mk_tagged (Par (fs, sn)) id

      | Emit s -> mk_tagged (Emit (rename env.signals s ast)) !+id
      | Nothing -> mk_tagged Nothing !+id
      | Pause -> mk_tagged Pause !+id

      | Await s -> mk_tagged (Await (rename env.signals s ast)) !+id

      | Suspend (t, s) ->
        mk_tagged (Suspend (visit env t, rename env.signals s ast)) !+id

      | Trap (Label s, t) ->
        let labels, s = add_env env.labels s in
        mk_tagged (Trap (Label s, visit {env with labels} t)) !+id

      | Exit (Label s) -> mk_tagged (Exit (Label (rename env.labels s ast))) !+id
      | Present (s, t1, t2) ->
        mk_tagged (Present(
            rename env.signals s ast, visit env t1, visit env t2))
          !+id
      | Atom f -> mk_tagged (Atom f) !+id

      | Signal (s,t) ->
        let signals, s = add_env env.signals s in
        mk_tagged (Signal (s, visit {env with signals} t)) !+id
    in
    visit env ast

let print_to_dot fmt tagged =
  let open Format in
  let rec visit x = match x.st with
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
    | Emit s -> fprintf fmt "N%d [label=\"%d emit(%s)\"];@\n"  x.id x.id s
    | Nothing  ->
      fprintf fmt "N%d [label=\"%d nothing\"]; @\n" x.id x.id
    | Pause  ->
      fprintf fmt "N%d [label=\"%d pause\"]; @\n" x.id x.id
    | Suspend (st, s) ->
      fprintf fmt "N%d [label=\"%d suspend(%s)\"]; @\n" x.id x.id s;
      fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
      visit st
    | Trap (Label s, st) ->
      fprintf fmt "N%d [label=\" %d trap(%s)\"]; @\n" x.id x.id s;
      fprintf fmt "N%d -> N%d ;@\n" x.id st.id;
      visit st
    | Exit (Label s) -> fprintf fmt "N%d [label=\"%d exit(%s)\"]; @\n" x.id x.id s
    | Atom f -> fprintf fmt "N%d [label=\"%d atom\"]; @\n" x.id x.id
    | Present (s, st1, st2) ->
      fprintf fmt "N%d [label=\"%d present(%s)\"]; @\n" x.id x.id s;
      fprintf fmt "N%d -> N%d ;@\n" x.id st1.id;
      fprintf fmt "N%d -> N%d ;@\n" x.id st2.id;
      visit st1;
      visit st2;
    | Await s ->
      fprintf fmt "N%d [label=\"%d await(%s)\"]; @\n" x.id x.id s
    | Signal (s, st) ->
      fprintf fmt "N%d [label=\"%d signal(%s)\"]; @\n" x.id x.id s;
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
    match t.st with
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


let rec flatten : statement list -> statement list = function
  | [] -> []
  | (Seq l) :: t -> l @ flatten t
  | st :: t -> st :: flatten t

(* let rec flatten_par : statement list -> statement list = function *)
(*   | [] -> [] *)
(*   | (Par l) :: t -> l @ flatten_par t *)
(*   | st :: t -> st :: flatten_par t *)


let (//) a b = Par [a; b]
let (!!) l = Seq l
let loop_each r p = Loop_each (p, r)
let loop l = Loop (Seq l)
let atom f = Atom f
let await a = Await a
let emit a = Emit a
let pause = Pause
let trap s st = Trap (Label s, st)
let exit_l s = Exit (Label s)
let abort s p = Abort (p ,s)
