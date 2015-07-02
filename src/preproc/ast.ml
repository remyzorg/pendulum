

type 'a location = {
  loc : Location.t [@printer Location.print_loc] ;
  content : 'a;
}[@@deriving show]

type ident = string location [@@deriving show]

type signal = ident [@@deriving show]
type label = Label of ident [@@deriving show]

let dummy_loc = Location.none
let mk_loc ?(loc=dummy_loc) content = {loc; content}

module Derived = struct
  type statement = statement_tree location
  and statement_tree =
  | Loop of statement
  | Seq of statement * statement
  | Par of statement * statement
  | Emit of signal
  | Nothing
  | Pause
  | Suspend of statement * signal
  | Trap of label * statement
  | Exit of label
  | Present of signal * statement * statement
  | Atom of Parsetree.expression [@printer Printast.expression 0]
  | Signal of signal * statement

  | Halt
  | Sustain of signal
  | Present_then of signal * statement
  | Await of signal
  | Await_imm of signal
  | Suspend_imm of statement * signal
  | Abort of statement * signal
  | Weak_abort of statement * signal
  | Loop_each of statement * signal
  | Every of signal * statement
      [@@deriving show]
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


let syntax_error ~loc s = raise (Location.Error (
    Location.error ~loc ("[%sync] " ^ s)))


module IntMap = Map.Make(struct
    type t = int
    let compare = compare
  end)

module StringMap = Map.Make(struct
    type t = string
    let compare = compare
  end)

let trap_signal = Label  (mk_loc "Trap")
let (!+) a = incr a; !a


module Tagged = struct

  type t = {id : int; st : tagged}
  [@@deriving show]

  and tagged_ast =
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
    | Atom of Parsetree.expression [@printer Printast.expression 0]
    | Signal of signal * t
    | Await of signal
  [@@deriving show]
  and tagged = tagged_ast location

  let mk_tagged ?(loc = dummy_loc) content id =
    {id; st = {loc ; content}}

  type env = {
    labels : int StringMap.t;
    signals : int StringMap.t;
  }

  let empty_env = {labels = StringMap.empty; signals = StringMap.empty}

  let add_env env s = StringMap.(match find s.content env with
      | exception Not_found -> add s.content 0 env, s
      | i -> add s.content (i + 1) env,
             {s with content = Format.sprintf "%s%d" s.content (i + 1)})

  let rename env s p =
    StringMap.(match find s.content env with
      | exception Not_found ->
        StringMap.iter (fun a b -> Format.printf "%s %d @\n" a b) env;
        error ~loc:p.loc @@ Unbound_identifier s.content
      | 0 -> s
      | i -> {s with content = Format.sprintf "%s%d" s.content i})


  let create_env sigs = {
    labels = StringMap.empty;
    signals = List.fold_left (fun accmap s ->
        StringMap.add s.content 0 accmap )
        StringMap.empty sigs
  }


  let rec of_ast ?(sigs=[]) ast =
    let id = ref 0 in
    let env = create_env sigs in
    let rec visit : env -> Derived.statement -> t = fun env ast ->
      let mk_tagged tagged = mk_tagged ~loc:ast.loc tagged in
      let mkl stmt = mk_loc ~loc:ast.loc stmt in
      match ast.content with
      | Derived.Loop t -> mk_tagged (Loop (visit env t)) !+id

      (* | (Derived.Par (n, st) *)
      (*   | Derived.Par (st, n) *)
      (*   | Derived.Seq (st, n) *)
      (*   | Derived.Seq (n, st)) when n.content = Nothing -> *)
      (*   visit env st *)

      | Derived.Seq (st1, st2) ->
        mk_tagged (Seq (visit env st1, visit env st2)) !+id

      | Derived.Par (st1, st2) ->
        mk_tagged (Par (visit env st1, visit env st2)) !+id

      | Derived.Emit s -> mk_tagged (Emit (rename env.signals s ast)) !+id
      | Derived.Nothing -> mk_tagged Nothing !+id
      | Derived.Pause -> mk_tagged Pause !+id

      | Derived.Await s -> mk_tagged (Await (rename env.signals s ast)) !+id

      | Derived.Suspend (t, s) ->
        mk_tagged (Suspend (visit env t, rename env.signals s ast)) !+id

      | Derived.Trap (Label s, t) ->
        let labels, s = add_env env.labels s in
        mk_tagged (Trap (Label s, visit {env with labels} t)) !+id

      | Derived.Exit (Label s) -> mk_tagged (Exit (Label (rename env.labels s ast))) !+id
      | Derived.Present (s, t1, t2) ->
        mk_tagged (Present(
            rename env.signals s ast, visit env t1, visit env t2))
          !+id
      | Derived.Atom f -> mk_tagged (Atom f) !+id

      | Derived.Signal (s,t) ->
        let signals, s = add_env env.signals s in
        mk_tagged (Signal (s, visit {env with signals} t)) !+id


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
    visit env ast

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
    | Emit s -> fprintf fmt "N%d [label=\"%d emit(%s)\"];@\n"  x.id x.id s.content
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
      fprintf fmt "N%d [label=\"%d signal(%s)\"]; @\n" x.id x.id s.content;
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


(* let rec flatten_par : statement list -> statement list = function *)
(*   | [] -> [] *)
(*   | (Par l) :: t -> l @ flatten_par t *)
(*   | st :: t -> st :: flatten_par t *)

open Derived
(* let list_to_seq l = *)
(*   let rec step l = match l with *)
(*   | [] -> mk_loc Nothing *)
(*   | [e] -> e *)
(*   | h :: t -> (mk_loc @@ Seq (h, step t)) *)
(*   in step l *)

(* let (//) a b = mk_loc @@ Par (a, b) *)
(* let (!!) l = list_to_seq l *)
(* let loop_each r p = mk_loc @@ Loop_each (p, r) *)
(* let loop l = mk_loc @@ Loop (!! l) *)
(* let atom f = mk_loc @@ Atom f *)
(* let await a = mk_loc @@ Await a *)
(* let emit a = mk_loc @@ Emit a *)
(* let pause = Pause *)
(* let trap s st = Trap (Label s, st) *)
(* let exit_l s = Exit (Label s) *)
(* let abort s p = Abort (p ,s) *)
