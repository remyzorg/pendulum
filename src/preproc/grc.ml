open Ast
open Utils

module Selection_tree = struct

  open Tagged

  type t = {label : int; t : repr; mutable tested : bool}
      [@@deriving show]
  and repr =
    | Bottom
    | Pause
    | Par of t list
    | Excl of t list
    | Ref of t

  let none = {label = 0; t = Excl []; tested = false}

  let (!+) a = incr a; !a

  let mk_tree stt id = {
    t = stt;
    label = id;
    tested = false;
  }

  let of_ast ast =
    let rec visit : Tagged.t -> t = fun tagged ->
      match tagged.st.content with
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
    | Emit of Ast.signal [@printer fun fmt s -> Format.fprintf fmt "%s" s.content]
    | Atom of Ast.atom [@printer fun fmt _ -> Format.fprintf fmt ""]
    | Enter of int
    | Exit of int
    [@@deriving show]

  let pp_action_dot fmt a =
    Format.(fprintf fmt "%s" begin
        match a with
        | Emit s -> "emit <B>" ^ s.content ^ "</B>"
        | Atom e -> asprintf "%a" Pprintast.expression e.Ast.exp
        | Enter i -> sprintf "enter %d" i
        | Exit i -> sprintf "exit %d" i
      end)


  type test_value =
    | Signal of Ast.signal [@printer fun fmt s -> Format.fprintf fmt "%s" s.content]
    | Selection of int
    | Finished
    [@@deriving show]

  let pp_test_value_dot fmt tv =
    Format.(begin
        match tv with
        | Signal s -> fprintf fmt "%s" s.content
        | Selection i -> fprintf fmt "%d" i
        | Finished -> fprintf fmt "finished"
      end)

  type t =
    | Call of action * t
    | Test of test_value * t * t (* then * else *)
    | Fork of t * t * t (* left * right * sync *)
    | Sync of (int * int) * t * t
    | Pause
    | Finish
    [@@deriving show]

  let pp_dot fmt t =
    Format.(begin
        match t with
        | Call (a, _) -> fprintf fmt "%a" pp_action_dot a
        | Test (tv, _, _) -> fprintf fmt "test <B>%a</B>  " pp_test_value_dot tv
        | Sync ((i1, i2), _, _) -> fprintf fmt "sync(%d, %d)" i1 i2
        | Fork (_, _, _) -> fprintf fmt "fork"
        | Pause -> fprintf fmt "pause"
        | Finish -> fprintf fmt "finish"
      end)

  type flowgraph = t

  module Fgtbl = Hashtbl.Make(struct
      type t = flowgraph
      let hash = Hashtbl.hash
      let equal = (==)
    end)

  module FgEmitsTbl = Hashtbl.Make(struct
      type t = flowgraph * flowgraph * Ast.ident
      let hash = Hashtbl.hash
      let equal (fg, stop, s) (fg', stop', s') =
        fg == fg' && stop = stop' && s.content = s'.content
    end)

  module Fgtbl2 = Hashtbl.Make(struct
      type t = flowgraph * flowgraph
      let hash = Hashtbl.hash
      let equal (a1, b1) (a2, b2) = (a1 == a2) && (b1 == b2)
    end)

  module Fgtbl3 = Hashtbl.Make(struct
      type t = flowgraph * flowgraph * flowgraph
      let hash = Hashtbl.hash
      let equal (a1, b1, c1) (a2, b2, c2) = (a1 == a2) && (b1 == b2) && (c1 == c2)
    end)

  module Fgstbl = Hashtbl.Make(struct
      type t = flowgraph list
      let hash = Hashtbl.hash
      let rec equal l1 l2 =
        match l1, l2 with
        | [], [] -> true
        | [x1], [x2] -> x1 == x2
        | [x1; y1], [x2; y2] -> x1 == x2 && y1 == y2
        | [x1; y1; z1], [x2; y2; z2] -> x1 == x2 && y1 == y2 && z1 == z2
        | x1 :: t1, x2 :: t2  -> x1 == x2 && equal t1 t2
        | [], _ | _, [] -> false
    end)

  let test_node t (c1, c2) = if c1 == c2 then c1 else
      Test (t, c1, c2)

  let (>>) s c = Call (s, c)
  let exit_node p next = Exit p.Tagged.id >> next
  let enter_node p next = Enter p.Tagged.id >> next

  let style_of_node = function
    | Call (a, _) -> "shape=oval" ^ begin match a with
        | Emit s ->", fontcolor=blue, "
        | Atom _ -> ", "
        | Enter i -> ", fontcolor=darkgreen, "
        | Exit i -> ", fontcolor=red, "
      end
    | Test _ -> "shape=box, "
    | Sync _ -> "shape=invtrapezium"
    | Finish -> ", fontcolor=red, "
    | Pause -> ", fontcolor=darkgreen, "
    | Fork _ -> ""


  let print_to_dot fmt fg =
    let id = let id = ref 0 in fun () -> incr id;!id in
    let open Format in
    let h = Fgtbl.create 17 in
    let rec visit fg =
      try Fgtbl.find h fg with
      | Not_found ->
        let id = begin match fg with
          | Call (action, t) ->
            let my_id = id () in
            fprintf fmt "N%d [%slabel=<%a>]; @\n" my_id (style_of_node fg) pp_dot fg;
            let fg_id = visit t in
            fprintf fmt "N%d -> N%d ;@\n" my_id fg_id;
            my_id

          | Test (_, t1, t2) | Fork (t1, t2, _) | Sync (_, t1, t2) ->
            let my_id = id () in
            let shape = match fg with Test _ | Sync _ -> "[style = dashed]" | _ -> "" in
            fprintf fmt "N%d [%s label=<%a>]; @\n" my_id (style_of_node fg) pp_dot fg;
            let fg1_id, fg2_id = visit t1, visit t2 in
            fprintf fmt "N%d -> N%d;@\n" my_id fg1_id;
            fprintf fmt "N%d -> N%d %s;@\n" my_id fg2_id shape;
            my_id

          | Pause | Finish ->
            let my_id = id () in
            fprintf fmt "N%d [shape = none, label=<%a>]; @\n" my_id pp_dot fg;
            my_id

        end
        in Fgtbl.add h fg id; id
    in
    fprintf fmt "@[<hov 2>digraph flowgraph {@\n";
    ignore(visit fg);
    fprintf fmt "}@]@\n"

end

type id = int [@@deriving show]

type error =
  | Unbound_label of string
  | Cyclic_causality of Flowgraph.t
  | Par_leads_to_finish of Flowgraph.t

exception Error of Location.t * error

let print_error fmt e =
  let open Format in
  fprintf fmt "Error: %s @\n"
    begin match e with
      | Unbound_label s -> "unbound label " ^ s
      | Cyclic_causality fg -> "Cyclic causality"
      | Par_leads_to_finish fg ->
        Format.printf "%a\n" Flowgraph.pp fg; "Par leads to pause or exit"
    end

let error ~loc e = raise (Error (loc, e))


module Of_ast = struct

  type env = {
    exits : Flowgraph.t StringMap.t;
    under_suspend : bool;
    synctbl : (int * int, Flowgraph.t) Hashtbl.t;
  }

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
      match p.st.content with
      | Pause -> enter_node p pause

      | Await s ->
        enter_node p @@
        test_node (Signal s) (
          exit_node p endp,
          pause
        )

      | Emit s -> Emit s >> endp
      | Nothing -> endp
      | Atom f -> Atom f >> endp

      | Suspend (q, _) ->
        enter_node p
        @@ surface env q pause
        @@ exit_node p endp

      | Signal (s, q) ->
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
            let n = Sync ((q.id, r.id), pause, exit_node p endp)
            in Hashtbl.add env.synctbl (q.id, r.id) n; n
        in
        enter_node p @@
        Fork (
          surface env q syn syn,
          surface env r syn syn,
          syn
        )

      | Exit (Label s) ->
        begin try StringMap.find s.content env.exits
          with Not_found -> error ~loc:Ast.dummy_loc @@ Unbound_label s.content
        end

      | Trap (Label s, q) ->
        let end_trap = exit_node p endp in
        enter_node p
        @@ surface {env with exits =
                               (StringMap.add s.content end_trap env.exits)} q pause end_trap
    in
    memo_rec h surface


  let depth h surface =
    let open Tagged in let open Flowgraph in
    let depth depth env p pause endp =
      match p.st.content with
      | Emit s -> endp
      | Nothing -> endp

      | Pause -> Exit p.id >> endp

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
            let n = Sync ((q.id, r.id), pause, exit_node p endp)
            in Hashtbl.add env.synctbl (q.id, r.id) n; n
        in
        Fork (
          test_node (Selection q.id) (
            depth env q syn syn,
            syn
          ),
          test_node (Selection r.id) (
            depth env r syn syn,
            syn),
          syn
        )

      | Present (s, q, r) ->
        let end_pres = exit_node p endp in
        test_node (Selection q.id) (
          depth env q pause end_pres,
          depth env r pause end_pres
        )

      | Signal (s,q) ->
        depth env q pause @@ exit_node p endp
      | Suspend (q, s) ->
        test_node (Signal s) (
          pause,
          depth env q pause (Exit p.id >> endp)
        )

      | Trap (Label s, q) ->
        let end_trap = exit_node p endp in
        depth {env with exits = StringMap.add s.content end_trap env.exits} q pause end_trap
    in memo_rec h depth


  let flowgraph p =
    let open Flowgraph in
    let open Tagged in
    let env = {
      under_suspend = false;
      exits = StringMap.empty;
      synctbl = Hashtbl.create 17;
    } in
    let depthtbl, surftbl = Hashtbl.create 30, Hashtbl.create 30 in

    let surface = surface surftbl in

    let s = surface env p Pause Finish in
    let d = depth depthtbl surface env p Pause Finish in

    test_node Finished (
      Finish,
      test_node (Selection p.id) (d, s)
    )


  let construct p =
    Selection_tree.of_ast p, flowgraph p

end


module Schedule = struct

  open Flowgraph

  let check_causality_cycles grc =
    let st, fg = grc in
    let rec visit m fg =
      match fg with
      | Test (Signal s, t1, t2) ->
        let prev = try IdentMap.find s m with
          | Not_found -> []
        in
        let m = IdentMap.add s (fg :: prev) m in
        let m1 = visit m t1 in
        let m2 = visit m t2 in
        IdentMap.merge (fun k v1 v2 ->
            match v1, v2 with
            | Some v1, Some v2 -> Some (v1 @ v2)
            | Some v, None | None, Some v -> Some v
            | _ -> None
          ) m1 m2

      | Fork (t1, t2, _) | Sync (_, t1, t2) ->
        let m1 = visit m t1 in
        let m2 = visit m t2 in
        IdentMap.merge (fun k v1 v2 ->
            match v1, v2 with
            | Some v1, Some v2 -> Some (v1 @ v2)
            | Some v, None | None, Some v -> Some v
            | _ -> None
          ) m1 m2

      | Call(Emit s, t) ->
        begin match IdentMap.find s m with
          | h :: fgs -> error ~loc:Ast.dummy_loc @@ Cyclic_causality h
          | [] -> m
          | exception Not_found -> m
        end
      | _ -> m
    in
    visit IdentMap.empty fg

  let memo_rec (type a) (module H : Hashtbl.S with type key = a) =
    let h = H.create 17 in
    fun f ->
      let rec g x =
        try H.find h x with
        | Not_found ->
          let y = f g x in
          H.add h x y; y
      in g

  let tag_tested_stmts sel fg =
    let open Flowgraph in
    let open Selection_tree in
    let lr = ref [] in
    let rec aux fg =
      match fg with
      | Call (_, t) -> aux t
      | Test (Selection i, t1, t2) ->
        lr := i :: !lr
      | Sync ((i1, i2) , t1, t2) ->
        lr := i1 :: i2 :: !lr
      | Test (_, t1, t2) | Fork (t1, t2, _)  ->
        aux t1; aux t2
      | Pause | Finish -> ()
    in
    let rec aux_sel sel =
      if List.mem sel.label !lr then sel.tested <- true;
      match sel.t with
      | Bottom -> ()
      | Pause -> ()
      | Par sels | Excl sels -> List.iter aux_sel sels
      | Ref sel -> aux_sel sel
    in
    aux fg; aux_sel sel



  let emits =
    let aux aux (fg, stop, s) =
      match fg with
      | Call (Emit s', t) when s = s' -> true
      | fg when fg == stop -> false
      | Call (_, t) -> aux (t, stop, s)
      | Test (_, t1, t2) | Fork (t1, t2, _) | Sync (_ , t1, t2) ->
        aux (t1, stop, s) || aux (t2, stop, s)
      | Pause | Finish -> false
    in
    let f = memo_rec (module FgEmitsTbl) aux in fun fg stop s -> f (fg, stop, s)

  let children =
    let children _ (fg, t1, t2) =
      let newfg = match fg with
        | Sync (ids, _, _) -> Sync (ids, t1, t2)
        | Test (tv, _, _) -> Test (tv, t1, t2)
        | Fork (_, _, sync) -> Fork (t1, t2, sync)
        | Call (a, _) -> Call(a, t1)
        | Pause | Finish -> fg
      in newfg
    in let f = memo_rec (module Fgtbl3) children in fun fg t1 t2 -> f (fg, t1, t2)


  let find =
    let aux aux (fg, elt) =
      if fg == elt then Some fg
      else match fg with
        | Call (a, t) -> aux (t, elt)
        | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->
          Option.mapn (aux (t1, elt)) (fun () -> aux (t2, elt))
        | Pause | Finish -> None
    in let f = memo_rec (module Fgtbl2) aux in fun fg t -> f (fg, t)


  let find_and_replace replf =
    let aux aux (fg, elt) =
      if fg == elt then true, replf fg
      else match fg with
        | Call (a, t) ->
          let res, t' = aux (t, elt) in
          let t = if res then t' else t in
          res, children fg t t
        | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _)->
          let res1, t1' = aux (t1, elt) in
          let res2, t2' = aux (t2, elt) in
          res1 || res2,
          children fg (if res1 then t1' else t1) (if res2 then t2' else t2)
        | Pause | Finish -> false, fg
    in let f = memo_rec (module Fgtbl2) aux in fun fg elt -> f (fg, elt)

  let rec find_join fg1 fg2 =
    Option.mapn (find fg2 fg1) begin fun () ->
      match fg1 with
      | Call(a, t) -> find_join fg2 t
      | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->
        Option.mapn (find_join fg2 t1) (fun () -> find_join fg2 t2)
      | Pause | Finish -> None
    end

  let rec replace_join fg1 fg2 replf =
    let res, fg2' = find_and_replace replf fg2 fg1 in
    if res then replf fg1, fg2'
    else match fg1 with
      | Call(a, t) ->
        let t, fg2 = replace_join t fg2 replf in
        Call (a, t), fg2
      | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->
        let t1, _ = replace_join t1 fg2 replf in
        let t2, _ = replace_join t2 fg2 replf in
        children fg1 t1 t2, fg2
      | Pause | Finish -> fg1, fg2


  let fork_id = function
    | Flowgraph.Sync (c, _, _) -> c
    | _ -> 0, 0

  let rec interleave fg =
    let fork_tbl = Fgtbl2.create 17 in
    let visit_tbl = Fgtbl.create 17 in
    let rec sequence_of_fork (stop: Flowgraph.t) fg1 fg2 =
      try Fgtbl2.find fork_tbl (fg1, fg2) with | Not_found ->
        try Fgtbl2.find fork_tbl (fg2, fg1) with | Not_found ->
          let fg = match fg1, fg2 with
            | fg1, fg2 when fg1 == fg2 -> fg1
            | fg1, fg2 when fork_id fg1 = fork_id fg2 &&
                            fork_id fg1 = fork_id stop
              -> fg1
            | fg1, fg2 when fork_id fg1 = fork_id stop
              -> sequence_of_fork stop fg2 fg1

            | (Pause | Finish), _ ->
              Format.printf "left is wrong@\n";
              error ~loc:Ast.dummy_loc (Par_leads_to_finish fg2)
            | _, (Finish | Pause) ->
              Format.printf "right is wrong@\n";
              error ~loc:Ast.dummy_loc (Par_leads_to_finish fg1)

            | Test (Signal s, t1, t2), fg2 ->
              if emits fg2 stop s then match fg2 with
                | Call (a, t) ->
                  Call (a, sequence_of_fork stop t fg1)
                | Pause | Finish -> assert false (* TODO: Raise exn *)
                | Sync(_, t1, t2) -> assert false
                | Test (_, t1, t2) ->
                  let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg1) in
                  children fg2 t1 t2
                | Fork (t1, t2, sync) ->
                  let fg2 = sequence_of_fork stop t1 t2 in
                  sequence_of_fork sync fg1 fg2
              else
                let t1, t2 = replace_join t1 t2 (fun x -> sequence_of_fork stop x fg2) in
                children fg1 t1 t2

            | Call (action, t), fg2 ->
              Call (action, sequence_of_fork stop fg2 t)

            | (Fork (t1, t2, sync)), (_ as fg2)
            | (_ as fg2), (Fork (t1, t2, sync)) ->
              let fg1 = sequence_of_fork sync t1 t2 in
              sequence_of_fork stop fg1 fg2

            | Sync (_, t1, t2), fg2 ->
              Format.printf "====@\n";
              let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg2) in
              children fg1 t1 t2

            | Test (_, t1, t2), fg2 ->
              let t1, t2 = replace_join t1 t2 (sequence_of_fork stop fg2) in
              children fg1 t1 t2
          in
          Fgtbl2.add fork_tbl (fg1, fg2) fg; fg
    in

    let rec visit fg =
      try
        Fgtbl.find visit_tbl fg
      with Not_found ->
        let fg' = match fg with
          | Call (a, t) -> Call (a, visit t)
          | Test (tv, t1, t2) -> Test (tv, visit t1, visit t2)
          | Fork (t1, t2, sync) -> sequence_of_fork sync (visit t1) (visit t2)
          | Sync ((i1, i2), t1, t2) -> Sync ((i1, i2), visit t1, visit t2)
          | Pause -> Pause
          | Finish -> Finish
        in
        Fgtbl.add visit_tbl fg fg'; fg'
    in
    visit fg

end
