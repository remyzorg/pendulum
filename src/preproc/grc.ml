open Ast
open Utils


module Selection_tree = struct

  module type S = sig

    module Ast : Ast.S

    type t = {label : int; t : repr; mutable tested : bool}
    and repr =
      | Bottom
      | Pause
      | Par of t list
      | Excl of t list
      | Ref of t

    val print_to_dot : Format.formatter -> t -> unit

    val of_ast : Ast.Tagged.t -> t

  end

  module Make (Ast : Ast.S) = struct

    module Ast = Ast

    open Ast
    open Tagged

    type t = {label : int; t : repr; mutable tested : bool}
    and repr =
      | Bottom
      | Pause
      | Par of t list
      | Excl of t list
      | Ref of t

    let none = {label = 0; t = Excl []; tested = false}

    let mk_tree stt id = {
      t = stt;
      label = id;
      tested = false;
    }

    let of_ast ast =
      let rec visit : Ast.Tagged.t -> t = fun tagged ->
        match tagged.st.content with
        | Emit _ | Nothing | Exit _ | Atom _ | Run _ -> mk_tree Bottom tagged.id
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

end

module Flowgraph = struct

  module type S = sig

    module Ast : Ast.S

    type action =
      | Emit of Ast.valued_signal
      | Atom of Ast.atom
      | Enter of int
      | Exit of int
      | Local_signal of Ast.valued_signal
      | Instantiate_run of Ast.ident * Ast.signal list

    type test_value =
      | Signal of Ast.signal
      | Selection of int
      | Is_paused of Ast.ident * Ast.signal list
      | Finished

    type t =
      | Call of action * t
      | Test of test_value * t * t (* then * else *)
      | Fork of t * t * t (* left * right * sync *)
      | Sync of (int * int) * t * t
      | Pause
      | Finish

    type flowgraph = t

    module Fgtbl : Hashtbl.S with type key = flowgraph
    module FgEmitsTbl : Hashtbl.S with type key = flowgraph * flowgraph * Ast.signal
    module Fgtbl2 : Hashtbl.S with type key = flowgraph * flowgraph
    module Fgtbl3 : Hashtbl.S with type key = flowgraph * flowgraph * flowgraph
    module Fgstbl : Hashtbl.S with type key = flowgraph list

    val print_to_dot : Format.formatter -> t -> unit
    val pp : Format.formatter -> t -> unit
    val pp_test_value : Format.formatter -> test_value -> unit
    val pp_action: Format.formatter -> action -> unit
    val test_node : test_value -> t * t -> t

    val (>>) : action -> t -> t
    val exit_node : Ast.Tagged.t -> t -> t
    val enter_node : Ast.Tagged.t -> t -> t


    type error =
      | Unbound_label of string
      | Cyclic_causality of t
      | Par_leads_to_finish of t

    val error : loc:Ast.loc -> error -> 'a

    exception Error of Ast.loc * error
    val print_error : Format.formatter -> error -> unit
  end

  module Make (Ast : Ast.S) = struct

    module Ast = Ast
    open Ast

    type action =
      | Emit of Ast.valued_signal
      | Atom of Ast.atom
      | Enter of int
      | Exit of int
      | Local_signal of Ast.valued_signal
      | Instantiate_run of Ast.ident * Ast.signal list

    let pp_action_dot fmt a =
      Format.(fprintf fmt "%s" begin
          match a with
          | Emit vs -> "emit <B>" ^ vs.signal.ident.content ^ "</B>"
          | Atom e -> asprintf "%a" printexp e.exp
          | Enter i -> sprintf "enter %d" i
          | Exit i -> sprintf "exit %d" i
          | Instantiate_run (id, _) -> sprintf "instantiate %s" id.content
          | Local_signal vs ->
            asprintf "signal %s (%a)" vs.signal.ident.content printexp vs.svalue.exp
        end)

    let pp_action fmt a =
      Format.(fprintf fmt "%s" begin
          match a with
          | Emit vs -> "Emit " ^ vs.signal.ident.content
          | Atom e -> asprintf "Atom (%a)" printexp e.exp
          | Enter i -> sprintf "Enter %d" i
          | Exit i -> sprintf "Exit %d" i
          | Instantiate_run (id, _) -> sprintf "Instantiate_run %s" id.content
          | Local_signal vs -> asprintf "Local_signal %s" vs.signal.ident.content
        end)

    type test_value =
      | Signal of Ast.signal
      | Selection of int
      | Is_paused of Ast.ident * Ast.signal list
      | Finished

    let pp_test_value_dot fmt tv =
      Format.(begin
          match tv with
          | Signal s -> fprintf fmt "%s ?" s.ident.content
          | Selection i -> fprintf fmt "%d ?" i
          | Finished -> fprintf fmt "finished ?"
          | Is_paused (id, _) -> fprintf fmt "paused %s ?" id.content
        end)

    let pp_test_value fmt tv =
      Format.(begin
          match tv with
          | Signal s -> fprintf fmt "Signal %s" s.ident.content
          | Selection i -> fprintf fmt "Selection %d" i
          | Finished -> fprintf fmt "Finished"
          | Is_paused (id, _) -> fprintf fmt "Is_paused %s" id.content
        end)

    type t =
      | Call of action * t
      | Test of test_value * t * t (* then * else *)
      | Fork of t * t * t (* left * right * sync *)
      | Sync of (int * int) * t * t
      | Pause
      | Finish

    let rec pp fmt t =
      let rec aux lvl fmt t =
        let indent = String.init lvl (fun _ -> ' ') in
        Format.(begin
            match t with
            | Call (a, t) ->
              fprintf fmt "%sCall(%a,\n%a)"
                indent pp_action a (aux (lvl + 1)) t
            | Test (tv, t1, t2) ->
              fprintf fmt "%sTest(%a,\n%a,\n%a) "
                indent pp_test_value tv (aux @@ lvl + 1) t1 (aux @@ lvl + 1) t2
            | Sync ((i1, i2), t1, t2) ->
              fprintf fmt "%sSync((%d, %d),\n%a,\n%a)" indent
                i1 i2 (aux @@ lvl + 1) t1 (aux @@ lvl + 1) t2
            | Fork (t1, t2, _) ->
              fprintf fmt "%sFork(\n%a, \n%a)"
                indent (aux @@ lvl + 1) t1 (aux @@ lvl + 1) t2
            | Pause -> fprintf fmt "%sPause" indent
            | Finish -> fprintf fmt "%sFinish" indent
          end)
      in aux 0 fmt t

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
        type t = flowgraph * flowgraph * Ast.signal
        let hash = Hashtbl.hash
        let equal (fg, stop, s) (fg', stop', s') =
          fg == fg' && stop = stop' && s.ident.content = s'.ident.content
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
          | Emit _ | Local_signal _ ->", fontcolor=blue, "
          | Atom _ -> ", "
          | Enter i -> ", fontcolor=darkgreen, "
          | Exit i -> ", fontcolor=red, "
          | Instantiate_run _ -> ", fontcolor=darkgreen, "
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


    type error =
      | Unbound_label of string
      | Cyclic_causality of t
      | Par_leads_to_finish of t

    exception Error of Ast.loc * error

    let print_error fmt e =
      let open Format in
      fprintf fmt "Error: %s @\n"
        begin match e with
          | Unbound_label s -> "unbound label " ^ s
          | Cyclic_causality fg -> "Cyclic causality"
          | Par_leads_to_finish fg -> "Par leads to pause or exit"
        end

    let error ~loc e = raise (Error (loc, e))

  end
end


module Of_ast = struct

  module type S = sig

    module Ast : Ast.S
    module Fg : Flowgraph.S
    module St : Selection_tree.S

    val flowgraph : Ast.Tagged.t -> Fg.t
    val construct : Ast.Tagged.t -> St.t * Fg.t
  end

  module Make (Fg : Flowgraph.S) (St : Selection_tree.S with module Ast = Fg.Ast) = struct

    module Fg = Fg
    module St = St
    module Ast = Fg.Ast
    open Ast

    type env = {
      exits : Fg.t StringMap.t;
      under_suspend : bool;
      synctbl : (int * int, Fg.t) Hashtbl.t;
    }

    type flow_builder = env -> Fg.Ast.Tagged.t -> Fg.t -> Fg.t -> Fg.t

    let memo_rec =
      fun h f ->
        let open Tagged in
        let rec g env x p e =
          try Hashtbl.find h x.id with
          | Not_found ->
            let y = f g env x p e in
            Hashtbl.add h x.id y; y
        in g


    let surface h =
      let open Tagged in let open Fg in
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
          enter_node p (
            Local_signal s
            >> surface env q pause
            @@ exit_node p endp)

        | Seq (q,r) ->
          let surf_r = (surface env r pause @@ exit_node p endp) in
          Hashtbl.remove h r.id;
          enter_node p
          @@ surface env q pause
          @@ surf_r

        | Present (s, q, r) ->
          let end_pres = exit_node p endp in
          enter_node p
          @@ test_node (Signal s) (
            surface env q pause end_pres,
            surface env r pause end_pres
          )

        | Run (id, sigs) ->
          let endrun = exit_node p endp in
          enter_node p (
            Instantiate_run (id, sigs)
            >> test_node (Is_paused (id, sigs)) (pause, endrun))


        | Loop q -> enter_node p @@ surface env q pause pause

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
      let open Tagged in let open Fg in
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
        | Loop q ->
          let surf_q = surface env q pause pause in
          depth env q pause surf_q

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

        | Run (id, sigs) ->
          let endrun = exit_node p endp in
          test_node (Is_paused (id, sigs)) (pause, endrun)

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
      let open Fg in
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
      St.of_ast p, flowgraph p

  end
end


module Schedule = struct
  module type S = sig

    module Ast : Ast.S
    module Fg : Flowgraph.S
    module St : Selection_tree.S

    val check_causality_cycles : 'a * Fg.t -> Fg.t list Ast.SignalMap.t
    val tag_tested_stmts : St.t -> Fg.t -> unit
    val find : bool -> Fg.t -> Fg.t -> Fg.t option
    val find_and_replace :
      (Fg.t -> Fg.t) ->
      Fg.t -> Fg.t -> bool * Fg.t

    val find_join : bool -> Fg.t -> Fg.t -> Fg.t option
    val replace_join : Fg.t -> Fg.t -> (Fg.t -> Fg.t)
      -> Fg.t * Fg.t
    val children: Fg.t -> Fg.t -> Fg.t -> Fg.t
    val interleave: Fg.t -> Fg.t
  end

  module Make
      (Fg : Flowgraph.S)
      (St : Selection_tree.S with module Ast = Fg.Ast)
  = struct

    module Fg = Fg
    module St = St
    module Ast = Fg.Ast

    open Fg
    open Ast

    let check_causality_cycles grc =
      let open Ast.SignalMap in
      let st, fg = grc in
      let rec visit m fg =
        match fg with
        | Test (Signal s, t1, t2) ->
          let prev = try find s m with
            | Not_found -> []
          in
          let m = add s (fg :: prev) m in
          let m1 = visit m t1 in
          let m2 = visit m t2 in
          merge (fun k v1 v2 ->
              match v1, v2 with
              | Some v1, Some v2 -> Some (v1 @ v2)
              | Some v, None | None, Some v -> Some v
              | _ -> None
            ) m1 m2

        | Fork (t1, t2, _) | Sync (_, t1, t2) ->
          let m1 = visit m t1 in
          let m2 = visit m t2 in
          merge (fun k v1 v2 ->
              match v1, v2 with
              | Some v1, Some v2 -> Some (v1 @ v2)
              | Some v, None | None, Some v -> Some v
              | _ -> None
            ) m1 m2

        | Call(Emit s, t) ->
          begin match find s.signal m with
            | h :: fgs -> Fg.error ~loc:Ast.dummy_loc @@ Fg.Cyclic_causality h
            | [] -> m
            | exception Not_found -> m
          end
        | _ -> m
      in
      visit empty fg

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
      let open Fg in
      let open St in
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



    let emits fg stop s =
      let aux aux (fg, stop, (s : Ast.signal)) =
        match fg with
        | Call (Emit vs, t) when s.ident.content = vs.signal.ident.content -> true
        | fg when fg == stop -> false
        | Call (_, t) -> aux (t, stop, s)
        | Test (_, t1, t2) | Fork (t1, t2, _) | Sync (_ , t1, t2) ->
          aux (t1, stop, s) || aux (t2, stop, s)
        | Pause | Finish -> false
      in memo_rec (module Fg.FgEmitsTbl) aux (fg, stop, s)


    let extract_emits_tests_sets fg stop =
      let open SignalSet in
      let aux aux (fg, stop) =
        match fg with
        | fg when fg == stop -> empty, empty

        | Call (Emit s, t) ->
          let emits, tests = aux (t, stop) in
          add s.signal emits, tests
        | Call (_, t) -> aux (t, stop)

        | Test (Signal s, t1, t2) ->
          let emits1, tests1 = aux (t1, stop) in
          let emits2, tests2 = aux (t2, stop) in
          union emits1 emits2, add s (union tests1 tests2)
        | Test (_, t1, t2) | Fork (t1, t2, _) | Sync (_ , t1, t2) ->
          let emits1, tests1 = aux (t1, stop) in
          let emits2, tests2 = aux (t2, stop) in
          union emits1 emits2, union tests1 tests2

        | Pause | Finish -> empty, empty
      in memo_rec (module Fg.Fgtbl2) aux (fg, stop)


    let children fg t1 t2 =
      let children _ (fg, t1, t2) =
        let newfg = match fg with
          | Sync (ids, _, _) -> Sync (ids, t1, t2)
          | Test (tv, _, _) -> Test (tv, t1, t2)
          | Fork (_, _, sync) -> Fork (t1, t2, sync)
          | Call (a, _) -> Call(a, t1)
          | Pause | Finish -> fg
        in newfg
      in memo_rec (module Fgtbl3) children (fg, t1, t2)


    let find nofinish fg t =
      let aux aux (fg, elt) =
        if fg == elt then
          if nofinish && (fg == Pause || fg == Finish) then None 
          else Some fg
        else match fg with
          | Call (a, t) -> aux (t, elt)
          | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->
            Option.mapn (aux (t1, elt)) (fun () -> aux (t2, elt))
          | Pause | Finish -> None
      in memo_rec (module Fgtbl2) aux (fg, t)


    let find_and_replace replf fg elt =
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
      in memo_rec (module Fgtbl2) aux (fg, elt)


    let rec find_join nopause fg1 fg2 =
      Option.mapn (find nopause fg2 fg1) begin fun () ->
        match fg1 with
        | Call(a, t) -> find_join nopause fg2 t
        | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->

          begin match (find_join nopause fg2 t1), (find_join nopause fg2 t2) with
          | Some v1, Some v2 when v1 == v2 && v1 <> Pause && v1 <> Finish -> Some v1
          | _ -> None
          end
        | Pause | Finish -> None
      end

    let leads_to_end stop fg =
      let aux aux (stop, fg) = match fg with
        | fg when fg == stop -> None
        | Call (Exit n, t) -> Option.map (max n) (aux (stop, t))
        | Call (a, t) -> aux (stop, t)
        | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) -> None
        | Pause | Finish -> Some 0
      in memo_rec (module Fgtbl2) aux (stop, fg)

    let rec replace_join fg1 fg2 replf =
      let rec aux aux (fg1, fg2) =
        let res, fg2' = find_and_replace replf fg2 fg1 in
        if res then replf fg1, fg2'
        else
          match fg1 with
          | Call(a, t) ->
            let t, fg2' = aux (t, fg2) in
            let fg1' = Call (a, t) in
            fg1', fg2'
          | Sync(_ , t1, t2) | Test (_, t1, t2) | Fork (t1, t2, _) ->
            let fg2_r = ref fg2 in
            let t1, t2 = replace_join t1 t2 (fun x ->
                let x', fg2' = aux (x, fg2) in
                fg2_r := fg2'; x')
            in
            let fg1' = children fg1 t1 t2 in
            fg1', !fg2_r
          | Pause | Finish -> fg1, fg2
      in memo_rec (module Fgtbl2) aux (fg1, fg2)


    let fork_id = function
      | Fg.Sync (c, _, _) -> c
      | _ -> 0, 0

    let rec interleave fg =
      let fork_tbl = Fgtbl2.create 17 in
      let visit_tbl = Fgtbl.create 17 in
      let rec sequence_of_fork (stop: Fg.t) fg1 fg2 =
        try Fgtbl2.find fork_tbl (fg1, fg2) with | Not_found ->
          try Fgtbl2.find fork_tbl (fg2, fg1) with | Not_found ->
            let fg = match fg1, fg2 with
              | fg1, fg2 when fg1 == fg2 -> fg1
              | fg1, fg2 when fork_id fg1 = fork_id fg2 &&
                              fork_id fg1 = fork_id stop
                -> fg1

              | fg1, fg2 when fork_id fg1 = fork_id stop
                -> sequence_of_fork stop fg2 fg1

              | fg1, fg2 when leads_to_end stop fg1 != None
                              || leads_to_end stop fg2 != None ->
                begin match leads_to_end stop fg1, leads_to_end stop fg2 with
                  | Some n1, Some n2 when n1 >= n2 -> fg1
                  | Some n1, Some n2 -> fg2
                  | Some _, None -> fg1
                  | None, Some _ -> fg2
                  | None, None -> assert false
                end

              | (Finish | Pause), fg
              | fg, (Finish | Pause) ->
                Fg.error ~loc:Ast.dummy_loc (Par_leads_to_finish fg2)

              | Test (Signal s, t1, t2), fg2 ->
                if emits fg2 stop s then
                  match fg2 with
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

              | Sync (_, t1, t2), fg2
              | Test (_, t1, t2), fg2 ->
                let fg1_emits, fg1_tests = extract_emits_tests_sets fg1 stop in
                let fg2_emits, fg2_tests = extract_emits_tests_sets fg2 stop in

                let open SignalSet in
                if inter fg2_emits fg1_tests <> empty then
                  if inter fg1_emits fg2_tests <> empty then
                    assert false
                  else
                    sequence_of_fork stop fg2 fg1
                else
                  let t1, t2 =
                    replace_join t1 t2 (sequence_of_fork stop fg2)
                  in children fg1 t1 t2


            in Fgtbl2.add fork_tbl (fg1, fg2) fg; fg
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

end
