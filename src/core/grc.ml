open Ast
open Utils

(* type tag = int [@@deriving show] *)
type id = int [@@deriving show]


type error = Unbound_label of string
exception Error of error

let error e = raise (Error e)

let print_error fmt e =
  let open Format in
  fprintf fmt "Error: %s @\n"
    begin match e with
      | Unbound_label s -> "unbound label " ^ s
    end

module Selection_tree = struct

  open Tagged

  type t = {mutable status : bool; label : int; t : repr}
    [@@deriving show]
  and repr =
    | Bottom
    | Pause
    | Par of t list
    | Excl of t list
    | Ref of t

  let none = {status = false; label = 0; t = Excl []}

  let (!+) a = incr a; !a

  let mk_tree stt id = {
    t = stt;
    status = false;
    label = id;
  }

  let of_ast ast =
    let rec visit : Tagged.t -> t = fun tagged ->
      match tagged.st with
      | Emit _ | Nothing | Exit _ | Atom _ -> mk_tree Bottom tagged.id
      | Pause -> mk_tree Pause tagged.id

      | Par (t1, t2) -> mk_tree (Par [visit t1; visit t2]) tagged.id

      | Seq (t1, t2) -> mk_tree (Excl [visit t1; visit t2]) tagged.id
      | Present (_, st1, st2) -> mk_tree (Excl [visit st1; visit st2]) tagged.id

      | Loop st -> mk_tree (Ref (visit st)) tagged.id
      | Suspend (st, _) -> mk_tree (Ref (visit st)) tagged.id
      | Signal (_, st) -> mk_tree (Ref (visit st)) tagged.id
      | Trap (_, st) -> mk_tree (Ref (visit st)) tagged.id
      | Await s -> mk_tree Pause tagged.id
    in
    visit ast

  module Primitive = struct
    let rec exit stree =
      stree.status <- false;
      match stree.t with
      | Pause | Bottom -> ()
      | Ref s -> exit s
      | Excl sts | Par sts -> List.iter exit sts

    let enter stree = stree.status <- true

    let sync stree = match stree.t with
      | Par sts -> List.for_all (fun x -> x.status) sts
      | _ -> assert false
  end


let print_to_dot fmt selection =
  let open Format in
  let rec visit x = match x.t with
    | Bottom -> fprintf fmt "N%d [shape = none, label=\"Bottom %d\"]; @\n" x.label x.label
    | Pause -> fprintf fmt "N%d [shape = none, label=\"Pause %d\"]; @\n" x.label x.label
    | Par sels ->
      fprintf fmt "N%d [shape = none, label=\"Par %d\"]; @\n" x.label x.label;
      List.iter (fun sel -> fprintf fmt "N%d -> N%d ;@\n" x.label sel.label; visit sel) sels
    | Excl sels ->
      fprintf fmt "N%d [shape = none, label=\"Excl %d\"]; @\n" x.label x.label;
      List.iter (fun sel -> fprintf fmt "N%d -> N%d ;@\n" x.label sel.label; visit sel) sels
    | Ref sel ->
      fprintf fmt "N%d [shape = none, label=\"Ref %d\"]; @\n" x.label x.label;
      fprintf fmt "N%d -> N%d ;@\n" x.label sel.label;
      visit sel
  in
  fprintf fmt "@[<hov 2>digraph selection {@\nmargin = 0;@\n";
  visit selection;
  fprintf fmt "}@]\n@."

end



module Flowgraph = struct

  type action =
    | Emit of string [@printer fun fmt -> Format.fprintf fmt "%s"]
    | Atom of (unit -> unit)
    | Enter of int
    | Exit of int
    | Finish
    | SetFinish of bool

  let pp_action fmt a =
    Format.(fprintf fmt "%s" begin
        match a with
        | Emit s -> "emit <B>" ^ s ^ "</B>"
        | Atom _ -> "atom"
        | Enter i -> sprintf "enter %d" i
        | Exit i -> sprintf "exit %d" i
        | Finish -> "finish"
        | SetFinish b -> sprintf "set_finish %B" b
      end)


  type test_value =
    | Signal of string [@printer fun fmt -> Format.fprintf fmt "%s"]
    | Selection of int
    | Finished

  let pp_test_value fmt tv =
    Format.(begin
        match tv with
        | Signal s -> fprintf fmt "%s" s
        | Selection i -> fprintf fmt "%d" i
        | Finished -> fprintf fmt "finished"
      end)


  type node =
    | Call of action
    | Test of test_value
    | Sync of int * int
    | Fork
    | Dep

  let pp_node fmt node =
    Format.(begin
        match node with
        | Call a -> fprintf fmt "%a" pp_action a
        | Test tv -> fprintf fmt "test <B>%a</B>  " pp_test_value tv
        | Sync (i1, i2) -> fprintf fmt "sync(%d, %d)" i1 i2
        | Fork -> fprintf fmt "fork"
        | Dep -> fprintf fmt "dep"
      end)

  type t =
    | Node_bin of node * t * t
    | Node of node * t
    | Leaf of node

  type flowgraph = t

  module Fgtbl = Hashtbl.Make(struct
      type t = flowgraph
      let hash = Hashtbl.hash
      let equal = (=)
    end)

  let (>>) s c = Node (s, c)

  let binary_node s (c1, c2) = Node_bin (s, c1, c2)

  let test_node t (c1, c2) = if c1 == c2 then c1 else
      Node_bin ((Test t), c1, c2)

  let exit_node p next = Call (Exit p.Tagged.id) >> next
  let enter_node p next = Call (Enter p.Tagged.id) >> next

  let style_of_node = function
    | Call a -> "shape=oval" ^ begin match a with
        | Emit s ->", fontcolor=blue, "
        | Atom _ -> ", "
        | Enter i -> ", fontcolor=darkgreen, "
        | Exit i -> ", fontcolor=red, "
        | Finish -> ", fontcolor=red, "
        | SetFinish b -> if b then ", fontcolor=darkgreen, " else ", fontcolor=red, "
      end
    | Test tv -> "shape=box, "
    | Sync (i1, i2) -> "shape=invtrapezium"
    | Fork | Dep -> ""


  let print_to_dot fmt fg =
    let id = let id = ref 0 in fun () -> incr id;!id in
    let open Format in
    let h = Fgtbl.create 17 in
    let rec visit fg =
      try Fgtbl.find h fg with
      | Not_found ->
        let id = begin match fg with
          | Node_bin (n, fg1, fg2) ->
            let my_id = id () in
            let shape = match n with Test _ | Sync _ -> "[style = dotted]" | _ -> "" in
            fprintf fmt "N%d [%s label=<%a>]; @\n" my_id (style_of_node n) pp_node n ;
            let fg1_id, fg2_id = visit fg1, visit fg2 in
            fprintf fmt "N%d -> N%d;@\n" my_id fg1_id;
            fprintf fmt "N%d -> N%d %s;@\n" my_id fg2_id shape;
            my_id
          | Node (n, fg) ->
            let my_id = id () in
            fprintf fmt "N%d [%slabel=<%a>]; @\n" my_id (style_of_node n) pp_node n;
            let fg_id = visit fg in
            fprintf fmt "N%d -> N%d ;@\n" my_id fg_id;
            my_id
          | Leaf n ->
            let my_id = id () in
            fprintf fmt "N%d [shape = none, label=<%a>]; @\n" my_id pp_node n;
            my_id
        end
        in Fgtbl.add h fg id; id
    in
    fprintf fmt "@[<hov 2>digraph flowgraph {@\n";
    ignore(visit fg);
    fprintf fmt "}@]@\n"




end

type env = {
  exits : Flowgraph.t StringMap.t;
  under_suspend : bool;
  synctbl : (int * int, Flowgraph.t) Hashtbl.t
}

let extend_labels env l =
  {env with exits = l}

type flow_builder = env -> Tagged.t -> Flowgraph.t -> Flowgraph.t -> Flowgraph.t



let memo_rec :
  (int, Flowgraph.t) Hashtbl.t ->
  (flow_builder -> flow_builder) ->
  flow_builder =
  fun h f ->
    let open Tagged in
    let rec g env x p e =
      try Hashtbl.find h x.id with
      | Not_found ->
        let y = f g env x p e in
        Hashtbl.add h x.id y; y
    in g


let surface h =
  let open Tagged in let open Flowgraph in
  let surface surface env p pause endp =
    match p.st with
    | Pause -> enter_node p pause

    | Await s ->
      enter_node p @@
      test_node (Signal s) (
        exit_node p endp,
        pause
      )

    | Emit s ->
      Call (Emit s) >> endp
    | Nothing -> endp
    | Atom f -> Call(Atom f) >> endp

    | Suspend (q, _)
    | Signal (_, q) ->
      enter_node p
      @@ surface env q pause
      @@ exit_node p endp

    | Seq (q,r) ->
      enter_node p
      @@ surface env q pause
      @@ surface env r pause endp

    | Present (s, q, r) ->
      let end_pres = exit_node p endp in
      enter_node p
      @@ test_node (Signal s) (
        surface env q pause end_pres,
        surface env r pause end_pres
      )

    | Loop q -> enter_node p @@ surface env q pause endp

    | Par (q, r) ->
      let syn = try Hashtbl.find env.synctbl (q.id, r.id) with
        | Not_found ->
          let n = binary_node (Sync(q.id, r.id))
          (pause, exit_node p endp)
          in Hashtbl.add env.synctbl (q.id, r.id) n; n
      in
      binary_node (Call(Enter p.id)) (
        surface env q syn syn,
        surface env r syn syn
      )

    | Exit (Label s) ->
      begin try StringMap.find s env.exits
        with Not_found -> error (Unbound_label s)
      end

    | Trap (Label s, q) ->
      let end_trap = exit_node p endp in
      enter_node p
      @@ surface {env with exits = (StringMap.add s end_trap env.exits)} q pause end_trap
  in
  memo_rec h surface


let depth h surface =
  let open Tagged in let open Flowgraph in
  let depth depth env p pause endp =
    match p.st with
    | Emit s -> endp
    | Nothing -> endp

    | Pause -> (Call (Exit p.id)) >> endp

    | Await s ->
      test_node (Signal s) (
        exit_node p endp,
        pause
      )

    | Atom f -> endp
    | Exit _ -> endp
    | Loop q -> depth env q pause (surface env q pause pause)

    | Seq (q, r) ->
      let end_seq = exit_node p endp in
      if env.under_suspend || Ast.Analysis.blocking q then
        test_node (Selection q.id) (
          depth env q pause (surface env r pause end_seq),
          depth env r pause end_seq
        )
      else
        depth env r pause end_seq

    | Par (q, r) ->
      let syn = try Hashtbl.find env.synctbl (q.id, r.id) with
        | Not_found ->
          let n = binary_node (Sync(q.id, r.id))
          (pause, exit_node p endp)
          in Hashtbl.add env.synctbl (q.id, r.id) n; n
      in
      binary_node Fork (
        test_node (Selection q.id) (
          depth env q syn syn,
          syn
        ),
        test_node (Selection r.id) (
          depth env r syn syn,
          syn)
      )

    | Present (s, q, r) ->
      let end_pres = exit_node p endp in
      test_node (Selection q.id) (
        depth env q pause end_pres,
        depth env r pause end_pres
      )

    | Signal (s,q) -> depth env q pause @@ exit_node p endp
    | Suspend (q, s) ->
      test_node (Signal s) (
        pause,
        depth env q pause (Call(Exit p.id) >> endp))

    | Trap (Label s, q) ->
      let end_trap = exit_node p endp in
      depth {env with exits = StringMap.add s end_trap env.exits} q pause end_trap
  in memo_rec h depth


let flowgraph p =
  let open Flowgraph in
  let open Tagged in
  let env = {under_suspend = false; exits = StringMap.empty; synctbl = Hashtbl.create 17} in
  let set_finish = Leaf (Call (SetFinish true)) in
  let set_not_finish = Leaf (Call (SetFinish false)) in
  let depthtbl, surftbl = Hashtbl.create 30, Hashtbl.create 30 in

  let surface = surface surftbl in

  let s = surface env p set_not_finish set_finish in
  let d = depth depthtbl surface env p set_not_finish set_finish in

  test_node Finished (
    Leaf(Call(Finish)),
    test_node (Selection p.id) (d, s)
  )


let of_ast p =
  Selection_tree.of_ast, flowgraph p
