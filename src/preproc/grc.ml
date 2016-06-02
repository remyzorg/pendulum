
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
      | Instantiate_run of Ast.ident * Ast.signal Ast.run_param list * Ast.loc

    type test_value =
      | Signal of Ast.signal * Ast.atom option
      | Selection of int
      | Sync of (int * int)
      | Is_paused of Ast.ident * Ast.signal Ast.run_param list * Ast.loc
      | Finished

    type t =
      | Call of action * t
      | Test of test_value * t * t * t option (* then * else * end *)
      | Fork of t * t * t (* left * right * sync *)
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
    val pp_dot : Format.formatter -> t -> unit
    val pp_test_value : Format.formatter -> test_value -> unit
    val pp_action: Format.formatter -> action -> unit
    val test_node : test_value -> t * t * t option -> t
    val sync_node : (int * int) -> (t * t * t option) -> t

    val (>>) : action -> t -> t
    val exit_node : Ast.Tagged.t -> t -> t
    val enter_node : Ast.Tagged.t -> t -> t


    type error =
      | Unbound_label of string
      | Cyclic_causality of t * Ast.signal list
      | Par_leads_to_finish of t

    val error : loc:Ast.loc -> error -> 'a

    exception Error of Ast.loc * error
    val print_error : Format.formatter -> error -> unit
  end

  module Make (Ast : Ast.S) = struct

    module Ast = Ast
    open Ast


    type action =
      | Emit of valued_signal
      | Atom of atom
      | Enter of int
      | Exit of int
      | Local_signal of valued_signal
      | Instantiate_run of ident * signal run_param list * loc

    let pp_action_dot fmt a =
      Format.(fprintf fmt "%s" begin
          match a with
          | Emit vs -> "emit <B>" ^ vs.signal.ident.content ^ "</B>"
          | Atom e -> "atom"
          | Enter i -> sprintf "enter %d" i
          | Exit i -> sprintf "exit %d" i
          | Instantiate_run (id, _, _) -> sprintf "instantiate %s" id.content
          | Local_signal vs ->
            asprintf "signal %s" vs.signal.ident.content
        end)

    let pp_action fmt a =
      Format.(fprintf fmt "%s" begin
          match a with
          | Emit vs -> "Emit " ^ vs.signal.ident.content
          | Atom e -> asprintf "Atom" 
          | Enter i -> sprintf "Enter %d" i
          | Exit i -> sprintf "Exit %d" i
          | Instantiate_run (id, _, _) -> sprintf "Instantiate_run %s" id.content
          | Local_signal vs -> asprintf "Local_signal %s" vs.signal.ident.content
        end)

    type test_value =
      | Signal of signal * atom option
      | Selection of int
      | Sync of (int * int)
      | Is_paused of ident * signal run_param list * loc
      | Finished

    type t =
      | Call of action * t
      | Test of test_value * t * t * t option (* then * else * join_value *)
      | Fork of t * t * t (* left * right * sync *)
      | Pause
      | Finish

    let pp_test_value_dot fmt tv =
      Format.(begin
          match tv with
          | Signal (s, exp) -> fprintf fmt "%s ?" s.ident.content
          | Selection i -> fprintf fmt "%d ?" i
          | Finished -> fprintf fmt "finished ?"
          | Sync (i1, i2) -> fprintf fmt "sync(%d, %d)" i1 i2
          | Is_paused (id, _, _) -> fprintf fmt "paused %s ?" id.content
        end)

    let pp_test_value fmt tv =
      Format.(begin
          match tv with
          | Signal (s, None) -> fprintf fmt "Signal (%s) " s.ident.content
          | Signal (s, Some at) -> fprintf fmt "Signal (%s, %a) " s.ident.content printexp at.exp
          | Selection i -> fprintf fmt "Selection %d" i
          | Sync (i1, i2) -> fprintf fmt "Sync(%d, %d)" i1 i2
          | Finished -> fprintf fmt "Finished"
          | Is_paused (id, _, _) -> fprintf fmt "Is_paused %s" id.content
        end)

    let rec pp fmt t =
      let rec aux lvl fmt t =
        let indent = String.init lvl (fun _ -> ' ') in
        Format.(begin
            match t with
            | Call (a, t) ->
              fprintf fmt "%sCall(%a,\n%a)"
                indent pp_action a (aux (lvl + 1)) t
            | Test(Sync (i1, i2), t1, t2, _) ->
              fprintf fmt "%sSync((%d, %d),\n%a,\n%a)" indent
                i1 i2 (aux @@ lvl + 1) t1 (aux @@ lvl + 1) t2
            | Test (tv, t1, t2, _) ->
              fprintf fmt "%sTest(%a,\n%a,\n%a) "
                indent pp_test_value tv (aux @@ lvl + 1) t1 (aux @@ lvl + 1) t2
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
          | Test(Sync (i1, i2), _, _, _) -> fprintf fmt "sync(%d, %d)" i1 i2
          | Test (tv, _, _, _) -> fprintf fmt "test <B>%a</B>  " pp_test_value_dot tv
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

    let test_node t (c1, c2, endt) = if c1 == c2 then c1 else
        Test (t, c1, c2, endt)

    let (>>) s c = Call (s, c)
    let exit_node p next = Exit p.Tagged.id >> next
    let enter_node p next = Enter p.Tagged.id >> next

    let sync_node c (t1, t2, endt) = test_node (Sync (c)) (t1, t2, endt)

    let style_of_node = function
      | Call (a, _) -> "shape=oval" ^ begin match a with
          | Emit _ | Local_signal _ ->", fontcolor=blue, "
          | Atom _ -> ", "
          | Enter i -> ", fontcolor=darkgreen, "
          | Exit i -> ", fontcolor=red, "
          | Instantiate_run _ -> ", fontcolor=darkgreen, "
        end
      | Test (Sync _, _, _, _) -> "shape=invtrapezium"
      | Test _ -> "shape=box, "
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

            | Test (_, t1, t2, _) | Fork (t1, t2, _) ->
              let my_id = id () in
              let shape = match fg with Test _ -> "[style = dashed]" | _ -> "" in
              fprintf fmt "N%d [%s label=<%a>]; @\n" my_id (style_of_node fg) pp_dot fg;
              let fg1_id, fg2_id = visit t1, visit t2 in
              fprintf fmt "N%d -> N%d;@\n" my_id fg1_id;
              fprintf fmt "N%d -> N%d %s;@\n" my_id fg2_id shape;
              my_id

            | Pause | Finish ->
              let my_id = id () in
              fprintf fmt "N%d [shape = none, label=<%a>]; @\n" my_id pp_dot fg;
              my_id

          end in Fgtbl.add h fg id; id
      in
      fprintf fmt "@[<hov 2>digraph flowgraph {@\n";
      ignore(visit fg);
      fprintf fmt "}@]@\n"


    type error =
      | Unbound_label of string
      | Cyclic_causality of t * Ast.signal list
      | Par_leads_to_finish of t

    exception Error of Ast.loc * error

    let print_error fmt e =
      let open Format in
      fprintf fmt "Error: %s @\n"
        begin match e with
          | Unbound_label s -> "unbound label " ^ s
          | Cyclic_causality (fg, sigs) ->
            Format.sprintf "Cyclic causality on [%s]"
              (String.concat "; " @@ List.map Ast.(fun s -> s.ident.content) sigs)
          | Par_leads_to_finish fg -> "Parallel leads to pause or exit"
        end

    let error ~loc e = raise (Error (loc, e))

  end
end


module Of_ast = struct

  module type S = sig

    module Ast : Ast.S
    module Fg : Flowgraph.S
    module St : Selection_tree.S
    open Utils

    val flowgraph : Ast.Tagged.env -> Options.t -> Ast.Tagged.t -> Fg.t
    val construct : Ast.Tagged.env -> Options.t -> Ast.Tagged.t -> St.t * Fg.t
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
      (* A Sync is the same flow, both in S and D,
         so there is a special table for this *)
    }


    (* Both surface and depth use a hashconsing function memo_rec : The result
       of S(p) and D(p) is stored in a hashtbl indexed by p where p is the
       integer identifier of the statement in the Ast. Thus, if S/D(p) is
       required twice or more, the result is already in the table.
    *)

    let memo_rec h f =
      let open Tagged in
      let rec g env x p e =
        try Hashtbl.find h (x.id, p, e) with
        | Not_found ->
          let y = f g env x p e in
          Hashtbl.add h (x.id, p, e) y; y
      in g


    (** See the compiling rules in documentation *)
    let surface options h =
      let open Tagged in let open Fg in
      let surface surface env p pause endp =
        match p.st.content with
        | Pause -> enter_node p pause

        | Await (s, atopt) ->
          enter_node p @@
          test_node (Signal (s, atopt)) (
            exit_node p endp,
            pause,
            None
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
          enter_node p
          @@ surface env q pause
          @@ surf_r

        | Loop q ->
          enter_node p
          @@ surface env q pause
          (* @@ if Ast.Analysis.blocking q then endp else *)
            pause

        | Present ((s, atopt), q, r) ->
          let end_pres = exit_node p endp in
          enter_node p
          @@ test_node (Signal (s, atopt)) (
            surface env q pause end_pres,
            surface env r pause end_pres,
            None
          )

        | Run (id, sigs, loc) ->
          let endrun = exit_node p endp in
          enter_node p (
            Instantiate_run (id, sigs, loc)
            >> test_node (Is_paused (id, sigs, loc)) (pause, endrun, None))

        | Par (q, r) ->
          let syn = try Hashtbl.find env.synctbl (q.id, r.id) with
            | Not_found ->
              let n = sync_node (q.id, r.id) (pause, exit_node p endp, None)
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


    let depth options h surface =
      let open Tagged in let open Fg in
      let depth depth env p pause endp =
        match p.st.content with
        | Emit _ -> endp
        | Nothing -> endp
        | Atom _ -> endp
        | Exit _ -> endp

        | Pause -> Exit p.id >> endp

        | Await (s, atopt) ->
          test_node (Signal (s, atopt)) (
            exit_node p endp,
            pause,
            None)

        | Loop q ->
          depth env q pause @@ surface env q pause
          (* @@ if Ast.Analysis.blocking q then endp else *)
            pause

        | Seq (q, r) ->
          let end_seq = exit_node p endp in
          let depth_r = depth env r pause endp in
          if Ast.Analysis.blocking q then begin
            let surf_r = surface env r pause end_seq in
            let depth_q = depth env q pause surf_r in
            test_node (Selection q.id) (depth_q, depth_r, None)
          end
          else depth_r

        | Par (q, r) ->
          let syn = try Hashtbl.find env.synctbl (q.id, r.id) with
            | Not_found ->
              let n = sync_node (q.id, r.id) (pause, exit_node p endp, None)
              in Hashtbl.add env.synctbl (q.id, r.id) n; n
          in
          Fork (
            test_node (Selection q.id) (
              depth env q syn syn,
              syn,
              None
            ),
            test_node (Selection r.id) (
              depth env r syn syn,
              syn,
              None
            ), syn)

        | Present (s, q, r) ->
          let end_pres = exit_node p endp in
          test_node (Selection q.id) (
            depth env q pause end_pres,
            depth env r pause end_pres,
            None
          )

        | Run (id, sigs, loc) ->
          let endrun = exit_node p endp in
          test_node (Is_paused (id, sigs, loc)) (pause, endrun, None)

        | Signal (s,q) ->
          depth env q pause @@ exit_node p endp
        | Suspend (q, (s, atopt)) ->
          test_node (Signal (s, atopt)) (
            pause,
            depth env q pause (Exit p.id >> endp),
            None
          )

        | Trap (Label s, q) ->
          let end_trap = exit_node p endp in
          depth {env with exits = StringMap.add s.content end_trap env.exits} q pause end_trap
      in memo_rec h depth


    let flowgraph astenv options p =
      let open Fg in
      let open Tagged in
      let env = {
        under_suspend = false;
        exits = StringMap.empty;
        synctbl = Hashtbl.create 17;
      } in

      let depthtbl, surftbl = Hashtbl.create 30, Hashtbl.create 30 in

      (* creates the surface function with the table, to be passed to depth *)
      let surface = surface options surftbl in

      let s = surface env p Pause Finish in
      let d = depth options depthtbl surface env p Pause Finish in

      let endsync = match d with
        | Fork (_ , _, sync) -> Some sync
        | _ -> Some Finish
      in
      (* the init part of the flowgraph *)
      test_node Finished (
        Finish,
        (* Tests if the first stmt is selection : is the the first
        execution or not *)
        test_node (Selection p.id) (d, s, endsync),
        Some Finish
      )
    let construct env options p =
      St.of_ast p, flowgraph env options p

  end
end


module Schedule = struct
  module type S = sig

    module Ast : Ast.S
    module Fg : Flowgraph.S
    module St : Selection_tree.S

    (** The scheduling and cycle checking only happens here
        when the flowgraph has been already built. 
    *)

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
    val interleave: Fg.Ast.Tagged.env -> Fg.t -> Fg.t

    module Stats : sig
      val size : Fg.t -> int
      val pp : Format.formatter -> Fg.t -> unit
    end

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
        | Test (Signal (s, _), t1, t2, _) ->
          let prev = try find s m with Not_found -> [] in
          let m' = add s (fg :: prev) m in
          let m1 = visit m t1 in
          let m2 = visit m' t2 in
          merge (fun k v1 v2 ->
              match v1, v2 with
              | Some v1, Some v2 -> Some (v1 @ v2)
              | Some v, None | None, Some v -> Some v
              | _ -> None
            ) m1 m2

        | Fork (t1, t2, _) | Test (Sync _, t1, t2, _) ->
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
            | h :: fgs ->
              Fg.(error ~loc:s.signal.ident.loc @@ Cyclic_causality (h, [s.signal]))
            | [] -> m
            | exception Not_found -> m
          end
        | Call (Instantiate_run _, _) -> assert false (* TODO *)
        | Test (Is_paused _, t1, t2, _) -> assert false (* TODO *)
        | _ -> m
      in
      visit empty fg

    let memo_rec (type a) (module H : Hashtbl.S with type key = a) =
      let h = H.create 87 in
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
        | Test (Selection i, t1, t2, _) ->
          lr := i :: !lr
        | Test (Sync (i1, i2) , t1, t2, _) ->
          lr := i1 :: i2 :: !lr
        | Test (_, t1, t2, _) | Fork (t1, t2, _)  ->
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


        | Call (Instantiate_run (_, sigs, _) , t) ->
          List.exists (fun s' -> s.ident.content = s'.ident.content)
          @@ Ast.filter_param (fun x -> x) sigs
          || aux (t, stop, s)
        | Test (Is_paused (_, sigs, _), t1, t2, _) ->
          List.exists (fun s' -> s.ident.content = s'.ident.content)
          @@ Ast.filter_param (fun x -> x) sigs
          || aux (t1, stop, s)
          || aux (t2, stop, s)


        | Call (_, t) -> aux (t, stop, s)
        | Test (_, t1, t2, _) | Fork (t1, t2, _) ->
          aux (t1, stop, s) || aux (t2, stop, s)
        | Pause | Finish -> false
      in memo_rec (module Fg.FgEmitsTbl) aux (fg, stop, s)


    let extract_emits_tests_sets fg stop =
      let open SignalSet in
      let fold_set = List.fold_left (fun acc x -> add x acc) in
      let aux aux (fg, stop) =
        match fg with
        | fg when fg == stop -> empty, empty

        | Call (Emit s, t) ->
          let emits, tests = aux (t, stop) in
          add s.signal emits, tests

        | Call (Instantiate_run (_, sigs, _), t) ->
          let emits, tests = aux (t, stop) in
          let sigs = Ast.filter_param (fun x -> x) sigs in
          fold_set emits sigs, fold_set tests sigs
        | Test (Is_paused (_, sigs, _), t1, t2, _) ->
          let emits1, tests1 = aux (t1, stop) in
          let emits2, tests2 = aux (t2, stop) in
          let sigs = Ast.filter_param (fun x -> x) sigs in
          fold_set (union emits1 emits2) sigs, fold_set (union tests1 tests2) sigs

        | Call (_, t) -> aux (t, stop)
        | Test (Signal (s, atopt), t1, t2, _) ->
          let emits1, tests1 = aux (t1, stop) in
          let emits2, tests2 = aux (t2, stop) in
          union emits1 emits2, add s (union tests1 tests2)
        | Test (_, t1, t2, _) | Fork (t1, t2, _) ->
          let emits1, tests1 = aux (t1, stop) in
          let emits2, tests2 = aux (t2, stop) in
          union emits1 emits2, union tests1 tests2

        | Pause | Finish -> empty, empty
      in memo_rec (module Fg.Fgtbl2) aux (fg, stop)


    let children fg t1 t2 =
      let children _ (fg, t1, t2) =
        let newfg = match fg with
          | Test (tv, _, _, tend) -> Test (tv, t1, t2, tend)
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
          | Test (_, t1, t2, _) | Fork (t1, t2, _) ->
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
          | Test (_, t1, t2, _) | Fork (t1, t2, _)->
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
        | Test (_, t1, t2, _) | Fork (t1, t2, _) ->

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
        | Test (_, t1, t2, _) | Fork (t1, t2, _) -> None
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
          | Test (_, t1, t2, _) | Fork (t1, t2, _) ->
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
      | (Fg.Test (Fg.Sync c, _, _, _)) -> c
      | _ -> 0, 0

    let rec interleave env fg =
      let fork_tbl = Fgtbl2.create 17 in
      let visit_tbl = Fgtbl.create 17 in
      let rec sequence_of_fork (stop: Fg.t) fg1 fg2 =
        try Fgtbl2.find fork_tbl (fg1, fg2) with | Not_found ->
          try Fgtbl2.find fork_tbl (fg2, fg1) with | Not_found ->
            let fg = match fg1, fg2 with
              | fg1, fg2 when fg1 == fg2 -> fg1

              | fg1, fg2 when fork_id fg1 = fork_id stop -> fg2
              | fg1, fg2 when fork_id fg2 = fork_id stop -> fg1

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
                Fg.error ~loc:Ast.Tagged.(env.pname.loc) (Par_leads_to_finish fg2)

              | Test (Signal (s, atopt), t1, t2, _), fg2 ->
                if emits fg2 stop s then
                  match fg2 with
                  | Call (a, t) ->
                    Call (a, sequence_of_fork stop t fg1)
                  | Pause | Finish -> assert false (* TODO: Raise exn *)
                  | Test (test, t1, t2, joinfg2) ->
                    let t1, t2 = replace_join t1 t2 (fun x ->
                        sequence_of_fork stop fg1 x
                      )
                    in
                    Test(test, t1, t2, joinfg2)
                  | Fork (t1, t2, sync) ->
                    let fg2 = sequence_of_fork stop t1 t2 in
                    sequence_of_fork sync fg1 fg2
                else (
                  let t1, t2 = replace_join t1 t2 (fun x -> sequence_of_fork stop x fg2) in
                  children fg1 t1 t2
                )

              | (Call (Instantiate_run _, _) as fg1), Call (action, t2)
              | Call (action, t2), (Call (Instantiate_run _, _) as fg1) ->
                Call (action, sequence_of_fork stop fg1 t2)

              | Call (action, t), fg2 ->
                Call (action, sequence_of_fork stop fg2 t)

              | (Fork (t1, t2, sync)), (_ as fg2)
              | (_ as fg2), (Fork (t1, t2, sync)) ->
                let fg1 = sequence_of_fork sync t1 t2 in
                sequence_of_fork stop fg1 fg2

              | Test (test, t1, t2, joinfg1) as fg1, fg2 ->

                let open SignalSet in
                let fg1_emits, fg1_tests = extract_emits_tests_sets fg1 stop in
                let fg2_emits, fg2_tests = extract_emits_tests_sets fg2 stop in
                let inter1 = inter fg2_emits fg1_tests in
                let inter2 = inter fg1_emits fg2_tests in

                if inter1 <> empty then
                  if inter2 <> empty then
                    Fg.(error ~loc:Ast.Tagged.(env.pname.loc)
                        @@ Cyclic_causality
                          (fg1, (SignalSet.fold List.cons (union inter1 inter2) [])))
                  else
                    sequence_of_fork stop fg2 fg1
                else
                  let t1, t2 = replace_join t1 t2 (fun x ->
                      sequence_of_fork stop fg2 x
                    )
                  in Test(test, t1, t2, joinfg1)

            in Fgtbl2.add fork_tbl (fg1, fg2) fg; fg
      in

      let rec visit fg =
        try
          Fgtbl.find visit_tbl fg
        with Not_found ->
          let fg' = match fg with
            | Call (a, t) -> Call (a, visit t)
            | Test (tv, t1, t2, tend) ->
              Test (tv, visit t1, visit t2, Option.map visit tend)
            | Fork (t1, t2, sync) -> sequence_of_fork sync (visit t1) (visit t2)
            | Pause -> Pause
            | Finish -> Finish
          in
          Fgtbl.add visit_tbl fg fg'; fg'
      in
      visit fg

    module Stats = struct

      let size fg =
        let h = Fgtbl.create 119 in
        let rec aux fg =
          try Fgtbl.find h fg with
          | Not_found ->
            Fgtbl.add h fg ();
            begin match fg with
              | Call (Exit n, t) -> aux t
              | Call (a, t) -> aux t
              | Test (_, t1, t2, _) | Fork (t1, t2, _) ->
                aux t1; aux t2
              | Pause | Finish -> ()
            end
        in
        aux fg; Fgtbl.length h

      let pp fmt fg =
        Format.fprintf fmt "size: %d" (size fg)

    end


  end

end
