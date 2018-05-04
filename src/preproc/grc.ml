
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

        | Par l -> mk_tree (Par (List.map visit l)) tagged.id
        | Seq (t1, t2) -> mk_tree (Excl [visit t1; visit t2]) tagged.id
        | Present (_, st1, st2) -> mk_tree (Excl [visit st1; visit st2]) tagged.id

        | Loop st -> mk_tree (Ref (visit st)) tagged.id
        | Suspend (st, _) -> mk_tree (Ref (visit st)) tagged.id
        | Signal (_, st) -> mk_tree (Ref (visit st)) tagged.id
        | Trap (_, st) -> mk_tree (Ref (visit st)) tagged.id
        | Await _ -> mk_tree Pause tagged.id
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
      | Compressed of action * action

    type test_value =
      | Signal of Ast.signal * Ast.atom option
      | Selection of int
      | Sync of (int list * t list Utils.IntMap.t)
      | Is_paused of Ast.ident * Ast.signal Ast.run_param list * Ast.loc
      | Finished

    and t =
      | Call of action * t
      | Test of test_value * t * t * t option (* then * else * end *)
      | Fork of t list * t (* n branches * sync *)
      | Pause
      | Finish

    type flowgraph = t

    module Fgtbl : Hashtbl.S with type key = flowgraph
    module Synctbl : Hashtbl.S with type key = int list
    module FgEmitsTbl : Hashtbl.S with type key = flowgraph * flowgraph * Ast.signal
    module Fgtbl2 : Hashtbl.S with type key = flowgraph * flowgraph
    module Fgtblid : Hashtbl.S with type key = int * flowgraph
    module Grctbl : Hashtbl.S with type key = Ast.Tagged.t * flowgraph * flowgraph
    module Fgtbl3 : Hashtbl.S with type key = flowgraph * flowgraph * flowgraph
    module Fgstbl : Hashtbl.S with type key = flowgraph list
    module Acttbl : Hashtbl.S with type key = action
    module TestValueSet : Set.S with type elt = test_value

    type env = {
      exits : flowgraph list Utils.IntMap.t;
      exit_nodes : flowgraph Fgtblid.t;
      under_suspend : bool;
      synctbl : flowgraph Synctbl.t;
      (* A Sync is the same flow, both in S and D,
         so there is a special table for this *)
      runtbl : (string, flowgraph) Hashtbl.t;
      awaittbl : (int, flowgraph) Hashtbl.t;
      parents : (flowgraph list) Fgtbl.t;
    }

    val init_grcenv : unit -> env

    val memo_rec : (module Hashtbl.S with type key = 'a) ->
      (('a -> 'b) -> 'a -> 'b) -> 'a -> 'b

    val compress : ?env:(t list Fgtbl.t) -> t -> t

    val emits : Ast.signal -> action -> bool
    val test_eq : test_value -> test_value -> bool

    type error =
      | Unbound_label of string
      | Empty_exits
      | Cyclic_causality of t * Ast.signal list
      | Par_leads_to_finish of t
      | Invariant_violation of t * string

    val error : loc:Ast.loc -> error -> 'a

    exception Error of Ast.loc * error
    val print_error : Format.formatter -> error -> unit

    val print_to_dot : Format.formatter -> t -> unit
    val pp : Format.formatter -> t -> unit
    val pp_head : Format.formatter -> t -> unit
    val pp_dot : Format.formatter -> t -> unit
    val pp_test_value : Format.formatter -> test_value -> unit
    val pp_test_value_dot : Format.formatter -> test_value -> unit
    val pp_test_value_short : Format.formatter -> test_value -> unit
    val pp_action: Format.formatter -> action -> unit
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
      | Compressed of action * action

    type test_value =
      | Signal of signal * atom option
      | Selection of int
      | Sync of (int list * t list IntMap.t)
      | Is_paused of ident * signal run_param list * loc
      | Finished

    and t =
      | Call of action * t
      | Test of test_value * t * t * t option (* then * else * join_value *)
      | Fork of t list * t (* left * right * sync *)
      | Pause
      | Finish


    type flowgraph = t

    module Fgtbl = Hashtbl.Make(struct
        type t = flowgraph
        let hash = Hashtbl.hash
        let equal = (==)
      end)

    module Synctbl = Hashtbl.Make(struct
        type t = int list
        let hash = Hashtbl.hash
        let equal x y = x = y
      end)

    module Fgtblid = Hashtbl.Make(struct
        type t = int * flowgraph
        let hash = Hashtbl.hash
        let equal (id, fg) (_, fg') = id = id && fg == fg'
      end)

    module Grctbl = Hashtbl.Make(struct
        open Tagged
        type t = Tagged.t * flowgraph * flowgraph

        let hash (ast, pausek, endk) =
          let open Hashtbl in
          (19 * (hash ast.id * 19 + hash pausek) +
           (if ast.st.content == Pause then hash pausek else hash endk)) land max_int

        let equal (ast, pausek, endk) (ast', pausek', endk') =
          ast.id = ast'.id
          && (pausek == pausek')
          && (ast.st.content = Pause || endk == endk')
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

    module Acttbl = Hashtbl.Make (struct
        type t = action
        let hash = Hashtbl.hash
        let equal = (==)
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


    module TestValueSet =
      Set.Make(struct
        type t = test_value
        let compare t1 t2 = if t1 == t2 then 0 else compare t1 t2
      end)

    type env = {
      exits : flowgraph list Utils.IntMap.t;
      exit_nodes : flowgraph Fgtblid.t;
      under_suspend : bool;
      synctbl : flowgraph Synctbl.t;
      (* A Sync is the same flow, both in S and D,
         so there is a special table for this *)
      runtbl : (string, flowgraph) Hashtbl.t;
      awaittbl : (int, flowgraph) Hashtbl.t;
      parents : (flowgraph list) Fgtbl.t;
    }

    let init_grcenv () = {
      under_suspend = false;
      exits = IntMap.empty;
      synctbl = Synctbl.create 17;
      runtbl = Hashtbl.create 17;
      awaittbl = Hashtbl.create 17;
      parents = Fgtbl.create 17;
      exit_nodes = Fgtblid.create 17;
    }

    let memo_rec (type a) (module H : Hashtbl.S with type key = a) =
      let h = H.create 87 in
      fun f ->
        let rec g x =
          try H.find h x with
          | Not_found ->
            let y = f g x in
            H.add h x y; y
        in g

    let get_parents parents fg =
      try Fgtbl.find parents fg with Not_found -> []

    let add_parent parents_tbl fg parent =
      let parents =
        try Fgtbl.find parents_tbl fg with Not_found -> []
      in
      Fgtbl.replace parents_tbl fg (parent :: parents)

    let parents ph fg =
      let parents parents fg = match fg with
        | Call (_, child) ->
          add_parent ph child fg; parents child
        | Test (_, then_br, else_br, _) ->
          add_parent ph then_br fg; add_parent ph else_br fg;
          parents then_br; parents else_br
        | Fork (l, _fg) ->
          List.iter (fun x -> add_parent ph x fg) l;
          List.iter parents l
        | Pause | Finish -> ()
      in memo_rec (module Fgtbl) parents fg

    let test_eq t1 t2 =
      match t1, t2 with
      | Signal (s1, a1), Signal (s2, a2) ->
        s1.ident.content = s2.ident.content && a1 = a2
      | Selection i1, Selection i2 -> i1 = i2
      | Sync l1, Sync l2 -> l1 = l2
      | Is_paused (id1, _, _), Is_paused (id2, _, _) -> id1.content = id2.content
      | _ -> false

    let rec emits s (act : action) =
      match act with
      | Emit vs when vs.signal.ident.content = s.ident.content -> true
      | Compressed (a1, a2) -> emits s a1 || emits s a2
      | Atom _ | Emit _ | Enter _ | Exit _
      | Local_signal _ | Instantiate_run _ -> false

    let compress ?env fg =
      let env = match env with
        | None ->
          let ph = Fgtbl.create 17 in
          parents ph fg; ph
        | Some env -> env
      in
      let compress compress fg = match fg with
        | Call (a, child) ->
          let child_comp = compress child in
          begin match child_comp with
          | Call (a', next_child) ->
            begin match get_parents env child with
              | [parent] when fg == parent ->
                Call (Compressed (a, a'), next_child)
              | _ ->
                Call (a, child_comp)
            end
          | _ -> Call (a, child_comp)
          end
        | Test (test_value, then_br, else_br, end_fg_opt) ->
          let then_br' = compress then_br in
          let else_br' = compress else_br in
          let end_fg_opt' = Option.map compress end_fg_opt in
          Test (test_value, then_br', else_br', end_fg_opt')
        | Fork (l, sync_fg) ->
          let l' = List.map compress l in
          let sync_fg' = compress sync_fg in
          Fork (l', sync_fg')
        | Pause -> Pause
        | Finish -> Finish
      in memo_rec (module Fgtbl) compress fg


    let pp_test_value_dot fmt tv =
      Format.(begin
          match tv with
          | Signal (s, _) ->
            fprintf fmt "%a"
            Dot_pp.(font blue (fun fmt x -> fprintf fmt "%s ?" x)) s.ident.content
          | Selection i -> fprintf fmt "%d ?" i
          | Finished -> fprintf fmt "finished ?"
          | Sync (lid, _) -> fprintf fmt "sync(%a)" (MList.pp_iter ~sep:", " Format.pp_print_int) lid
          | Is_paused (id, _, _) -> fprintf fmt "paused %s ?" id.content
        end)

    let rec pp_action_dot fmt a =
      let open Format in
      let open Dot_pp in
      match a with
      | Emit vs ->
        fprintf fmt "%a"
          (font blue (bold str)) vs.signal.ident.content
      | Atom _ -> fprintf fmt "atom"
      | Enter i -> fprintf fmt "%a" (bold @@ font darkgreen int) i
      | Exit i -> fprintf fmt "%a" (bold @@ font red int) i
      | Instantiate_run (id, _, _) ->
        fprintf fmt "instantiate %a" (font darkgreen str) id.content
      | Local_signal vs ->
        fprintf fmt "signal %a" (font darkgreen str) vs.signal.ident.content
      | Compressed (a, a') -> fprintf fmt "%a; %a" pp_action_dot a pp_action_dot a'

    let pp_dot fmt t =
      let open Format in
      let open Dot_pp in
      match t with
      | Call (a, _) -> fprintf fmt "%a" pp_action_dot a
      | Test (tv, _, _, _) -> fprintf fmt "%a " (bold pp_test_value_dot) tv
      | Fork (_, _) -> fprintf fmt "fork"
      | Pause -> fprintf fmt "pause"
      | Finish -> fprintf fmt "finish"

    let pp_test_value fmt tv =
      Format.(begin
          match tv with
          | Signal (s, None) -> fprintf fmt "Signal (%s) " s.ident.content
          | Signal (s, Some at) ->
            fprintf fmt "Signal (%s, %a) " s.ident.content printexp at.exp
          | Selection i -> fprintf fmt "Selection %d" i
          | Sync (lid, _) -> fprintf fmt "Sync(%a)" (MList.pp_iter ~sep:", " Format.pp_print_int) lid
          | Finished -> fprintf fmt "Finished"
          | Is_paused (id, _, _) -> fprintf fmt "Is_paused %s" id.content
        end)

    let pp_test_value_short fmt tv =
      Format.(begin
          match tv with
          | Signal (s, None) -> fprintf fmt "%s?" s.ident.content
          | Signal (s, Some at) ->
            fprintf fmt "(%s, %a)?" s.ident.content printexp at.exp
          | Selection i -> fprintf fmt "%d" i
          | Sync (lid, _) -> fprintf fmt "(%a)" (MList.pp_iter ~sep:", " Format.pp_print_int) lid
          | Finished -> fprintf fmt "f"
          | Is_paused (id, _, _) -> fprintf fmt "p(%s)?" id.content
        end)

    let rec pp_action fmt a =
      Format.(
          match a with
          | Emit vs -> fprintf fmt "Emit %s" vs.signal.ident.content
          | Atom _ -> fprintf fmt "Atom"
          | Enter i -> fprintf fmt "Enter %d" i
          | Exit i -> fprintf fmt "Exit %d" i
          | Instantiate_run (id, _, _) -> fprintf fmt "Instantiate_run %s" id.content
          | Local_signal vs -> fprintf fmt "Local_signal %s" vs.signal.ident.content
          | Compressed (a, a') -> fprintf fmt "%a; %a" pp_action a pp_action a'
      )

    let pp fmt t =
      let rec aux lvl fmt t =
        let indent = String.init lvl (fun _ -> ' ') in
        let open Format in
        match t with
        | Call (a, t) ->
          fprintf fmt "%sCall([%a],\n%a)"
            indent pp_action a (aux (lvl + 1)) t
        | Test(Sync (lid, _), t1, t2, _) ->
          fprintf fmt "%sSync((%a),\n%a,\n%a)" indent
            (MList.pp_iter ~sep:", " Format.pp_print_int) lid
            (aux @@ lvl + 1) t1 (aux @@ lvl + 1) t2
        | Test (tv, t1, t2, _) ->
          fprintf fmt "%sTest(%a,\n%a,\n%a) "
            indent pp_test_value tv (aux @@ lvl + 1) t1 (aux @@ lvl + 1) t2
        | Fork (l, _) ->
          fprintf fmt "%sFork(\n%a)" indent (MList.pp_iter ~sep:"\n" (aux @@ lvl + 1)) l
        | Pause -> fprintf fmt "%sPause" indent
        | Finish -> fprintf fmt "%sFinish" indent
      in aux 0 fmt t

    let rec pp_head fmt t =
      let open Format in
      match t with
      | Call (a, _) ->
        fprintf fmt "Call([%a],_)" pp_action a
      | Test(Sync (lid, _), _, _, _) ->
        fprintf fmt "Sync((%a),_,_)" (MList.pp_iter ~sep:", " Format.pp_print_int) lid
      | Test (tv, _, _, _) ->
        fprintf fmt "Test(%a,_,_) " pp_test_value tv
      | Fork (l, _) ->
        fprintf fmt "Fork(%a)" (MList.pp_iter ~sep:"\n" pp_head) l
      | Pause -> fprintf fmt "Pause"
      | Finish -> fprintf fmt "Finish"

    let style_of_node = function
      | Call (_, _) -> "shape=oval "
      | Test (Sync _, _, _, _) -> "shape=invtrapezium"
      | Test _ -> "shape=box, "
      | Finish -> ", fontcolor=red, "
      | Pause -> ", fontcolor=darkgreen, "
      | Fork _ -> ""

    let print_to_dot fmt fg =
      let open Format in
      let id = let id = ref 0 in fun () -> incr id;!id in
      let h = Fgtbl.create 17 in
      let rec visit fg =
        try Fgtbl.find h fg with
        | Not_found ->
          let id = begin match fg with
            | Call (_, t) ->
              let my_id = id () in
              fprintf fmt "N%d [%slabel=<%a>]; @\n" my_id (style_of_node fg) pp_dot fg;
              let fg_id = visit t in
              fprintf fmt "N%d -> N%d ;@\n" my_id fg_id;
              my_id

            | Test (_, t1, t2, end_branch) ->
              print_branches_to_dot fmt "[style = dashed]" fg t1 t2 end_branch

            | Fork (l, _) ->
              let my_id = id () in
              fprintf fmt "N%d [%s label=<%a>]; @\n" my_id (style_of_node fg) pp_dot fg;
              List.iter (fun x ->
                  let fg_id = visit x in
                  fprintf fmt "N%d -> N%d;@\n" my_id fg_id;
                ) l;
              my_id

            | Pause | Finish ->
              let my_id = id () in
              fprintf fmt "N%d [shape = none, label=<%a>]; @\n" my_id pp_dot fg;
              my_id
          end in Fgtbl.add h fg id; id

      and print_branches_to_dot fmt shape fg t1 t2 _ =
        let open Format in
        let my_id = id () in
        fprintf fmt "N%d [%s label=<%a>]; @\n" my_id (style_of_node fg) pp_dot fg;
        let fg1_id, fg2_id = visit t1, visit t2 in
        fprintf fmt "N%d -> N%d;@\n" my_id fg1_id;
        fprintf fmt "N%d -> N%d %s;@\n" my_id fg2_id shape;
        my_id
      in
      fprintf fmt "@[<hov 2>digraph flowgraph {@\n";
      ignore(visit fg);
      fprintf fmt "}@]@\n"

    type error =
      | Unbound_label of string
      | Empty_exits
      | Cyclic_causality of t * Ast.signal list
      | Par_leads_to_finish of t
      | Invariant_violation of t * string

    exception Error of Ast.loc * error

    let print_error fmt e =
      let open Format in
      fprintf fmt "Error: %a @\n" (fun fmt e ->
        begin match e with
          | Unbound_label s -> fprintf fmt "unbound label %s" s
          | Empty_exits -> fprintf fmt "empty exits"
          | Cyclic_causality (_, sigs) ->
            fprintf fmt "Cyclic causality on [%a]"
              (MList.pp_iter ~sep:"; "
                 (fun fmt s -> fprintf fmt "%s" s.ident.content)) sigs
          | Par_leads_to_finish _ ->
            fprintf fmt "Parallel leads to pause or exit"
          | Invariant_violation (fg, err) ->
            fprintf fmt "An invariant is broken. %s. \n%a" err pp fg
        end
        ) e

    let error ~loc e = raise (Error (loc, e))


  end
end


module Of_ast = struct

  module type S = sig

    module Ast : Ast.S
    module Fg : Flowgraph.S
    module St : Selection_tree.S
    open Utils


    val flowgraph : Ast.Tagged.env -> Fg.env -> Options.t -> Ast.Tagged.t -> Fg.t
    val construct : Ast.Tagged.env -> Fg.env -> Options.t -> Ast.Tagged.t -> St.t * Fg.t


  end

  module Make (Fg : Flowgraph.S) (St : Selection_tree.S with module Ast = Fg.Ast) = struct

    module Fg = Fg
    module St = St
    module Ast = Fg.Ast
    open Ast

    let add_parent parents_tbl fg parent =
      let parents =
        try Fg.Fgtbl.find parents_tbl fg with Not_found -> []
      in
      Fg.Fgtbl.replace parents_tbl fg (parent :: parents)

    let test_node env t (c1, c2, endt) =
      let open Fg in
      if c1 == c2 then c1 else
        let test = Test (t, c1, c2, endt) in
        add_parent env.parents c1 test; add_parent env.parents c2 test;
        test

    let call env s fg =
      let open Fg in
      let c = Call (s, fg) in
      add_parent env.parents fg c; c

    let exit_node env p next =
      let open Tagged in
      let open Fg in
      try Fgtblid.find env.exit_nodes (p.id, next) with
      | Not_found ->
        let ex = call env (Exit p.id) next in
        Fgtblid.add env.exit_nodes (p.id, next) ex; ex


    let enter_node env p next = call env (Fg.Enter p.Tagged.id) next
    let sync_node env c (t1, t2, endt) = test_node env (Fg.Sync (c)) (t1, t2, endt)


    (* Both surface and depth use a hashconsing function memo_rec : The result
       of S(p) and D(p) is stored in a hashtbl indexed by p where p is the
       integer identifier of the statement in the Ast. Thus, if S/D(p) is
       required twice or more, the result is already in the table.
    *)

    let memo_rec_build h f =
      let rec g env x p e =
        try Fg.Grctbl.find h (x, p, e) with
        | Not_found ->
          let y = f g env x p e in
          Fg.Grctbl.add h (x, p, e) y; y
      in g


    (** See the compiling rules in documentation *)
    let surface _o h =
      let open Tagged in let open Fg in
      let surface surface env p pause endp =
        match p.st.content with
        | Pause -> enter_node env p pause

        | Await (s, atopt) ->
          let aw = test_node env (Signal (s, atopt)) (
            exit_node env p endp,
            pause,
            Some pause
          )
          in
          Hashtbl.add env.awaittbl p.id aw;
          enter_node env p @@ aw

        | Emit s -> call env (Emit s) endp
        | Nothing -> endp

        | Atom f -> call env (Atom f) endp

        | Suspend (q, _) ->
          enter_node env p
          @@ surface env q pause
          @@ exit_node env p endp

        | Signal (s, q) ->
          enter_node env p (
            call env (Local_signal s)
            @@ surface env q pause
            @@ exit_node env p endp
          )

        | Seq (q,r) ->
          let surf_r = (surface env r pause @@ exit_node env p endp) in
          enter_node env p @@ surface env q pause @@ surf_r

        | Loop q ->
          enter_node env p
          @@ surface env q pause
            pause

        | Present ((s, atopt), q, r) ->
          let end_pres = exit_node env p endp in
          let t = (Signal (s, atopt)) in
          enter_node env p
          @@ test_node env t (
            surface env q pause end_pres,
            surface env r pause end_pres,
            None
          )

        | Run (id, sigs, loc) ->
          let endrun = exit_node env p endp in

          let ip = try Hashtbl.find env.runtbl id.content with
          | Not_found ->
            let ip = test_node env (Is_paused (id, sigs, loc)) (pause, endrun, None) in
            Hashtbl.add env.runtbl id.content ip; ip
          in
          enter_node env p (call env (Instantiate_run (id, sigs, loc)) ip)

        | Par l ->
          let lid = List.map (fun x -> x.id) l in
          let syn = try
              Synctbl.find env.synctbl lid
            with
            | Not_found ->
              let n = sync_node env (lid, env.exits) (pause, exit_node env p endp, None) in
              Synctbl.add env.synctbl lid n; n
          in
          let exits = IntMap.(fold (fun k v m ->
              add k (syn :: v) m
            ) empty) env.exits
          in
          let env = { env with exits } in
          enter_node env p @@
          Fork (List.map (fun x -> surface env x syn syn) l, syn)
        | Exit (TLabel (s, n)) ->
          begin try
              List.hd (IntMap.find n env.exits)
            with
            | Not_found -> error ~loc:(Ast.dummy_loc ()) @@ Unbound_label s.content
            | Failure _ -> error ~loc:(Ast.dummy_loc ()) @@ Empty_exits
          end
        | Trap (TLabel (_, n), q) ->
          let end_trap = exit_node env p endp in
          enter_node env p
          @@ surface {env with exits =
              (IntMap.add n [end_trap] env.exits)} q pause end_trap
      in
      memo_rec_build h surface

    let depth _o h surface =
      let open Tagged in let open Fg in
      let depth depth env p pause endp =
        match p.st.content with
        | Emit _ -> endp
        | Nothing -> endp
        | Atom _ -> endp
        | Exit _ -> endp

        | Pause -> call env (Exit p.id) endp

        | Await (_, _) ->
          begin try Hashtbl.find env.awaittbl p.id with
          | Not_found ->
            match surface env p pause endp with
              | Call (_, fg) -> fg
              | _ -> assert false
          end

        | Loop q ->
          depth env q pause @@ surface env q pause
          (* @@ if Ast.Analysis.blocking q then endp else *)
            pause

        | Seq (q, r) ->
          let end_seq = exit_node env p endp in
          let depth_r = depth env r pause end_seq in
          if Ast.Analysis.non_blocking q then depth_r
          else
            let surf_r = surface env r pause end_seq in
            let depth_q = depth env q pause surf_r in
            test_node env (Selection q.id) (depth_q, depth_r, Some pause)

        | Par l ->
          let lid = List.map (fun x -> x.id) l in
          let syn = try Synctbl.find env.synctbl lid with
            | Not_found ->
              let n = sync_node env (lid, env.exits) (pause, exit_node env p endp, None) in
              Synctbl.add env.synctbl lid n; n
          in
          let exits = IntMap.(fold (fun k v m ->
              add k (syn :: v) m
            ) empty) env.exits
          in
          let env = { env with exits } in
          Fork (List.map (fun x ->
              let t = Selection x.id in
              test_node env t (
                depth env x syn syn,
                syn,
                None
              )) l, syn)

        | Present (_, q, r) ->
          let end_pres = exit_node env p endp in
          let t = (Selection q.id) in
          if Ast.Analysis.non_blocking q then
            depth env r pause end_pres
          else
            test_node env t (
              depth env q pause end_pres,
              depth env r pause end_pres,
              None
            )

        | Run (id, sigs, loc) ->
          let endrun = exit_node env p endp in
          begin try Hashtbl.find env.runtbl id.content with
          | Not_found ->
            let ip = test_node env (Is_paused (id, sigs, loc)) (pause, endrun, None) in
            Hashtbl.add env.runtbl id.content ip;
            ip
          end

        | Signal (_, q) ->
          depth env q pause @@ exit_node env p endp
        | Suspend (q, (s, atopt)) ->
          test_node env (Signal (s, atopt)) (
            pause,
            depth env q pause (call env (Exit p.id) endp),
            None
          )

        | Trap (TLabel (_, n), q) ->
          let end_trap = exit_node env p endp in
          let exits = IntMap.add n [end_trap] env.exits in
          depth { env with exits } q pause end_trap
      in memo_rec_build h depth



    let flowgraph _astenv grcenv options p =
      let open Fg in
      let open Tagged in

      let depthtbl, surftbl = Grctbl.create 30, Grctbl.create 30 in
      (* creates the surface function with the table, to be passed to depth *)
      let surf = surface options surftbl in
      let dep = depth options depthtbl in
      let s = surf grcenv p Pause Finish in
      let d = dep surf grcenv p Pause Finish in

      let endsync = match d with
        | Fork (_, sync) -> Some sync
        | _ -> Some Finish
      in
      (* the init part of the flowgraph *)
      let result_fg = test_node grcenv Finished (
          Finish,
          (* Tests if the first stmt is selection : is the the first
             execution or not *)
          test_node grcenv (Selection p.id) (d, s, endsync),
          Some Finish
        )
      in
      result_fg

    let construct _astenv grcenv options p =
      St.of_ast p, flowgraph grcenv grcenv options p

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

    val tag_tested_stmts : St.t -> Fg.t -> unit
    val find : ?stop:Fg.t -> bool -> Fg.t -> Fg.t -> Fg.t option

    val find_join : Fg.t -> Fg.t -> Fg.t option
    val children: Fg.t -> Fg.t -> Fg.t -> Fg.t
    val interleave: StringSet.t -> Fg.Ast.Tagged.env -> Fg.env -> Fg.t -> Fg.t

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

    let tag_tested_stmts sel fg =
      let open Fg in
      let open St in
      let lr = ref [] in
      let rec aux fg =
        match fg with
        | Call (_, t) -> aux t
        | Test (Selection i, _, _, _) ->
          lr := i :: !lr
        | Test (Sync (lid, _), _, _, _) -> lr := List.(fold_left (fun acc x -> x :: acc) !lr lid)
        | Test (_, t1, t2, _) -> aux t1; aux t2
        | Fork (l, _)  -> List.iter aux l
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

    let children fg t1 t2 =
      let children _ (fg, t1, t2) =
        let newfg = match fg with
          | Test (tv, _, _, tend) -> Test (tv, t1, t2, tend)
          | Fork (l, sync) -> Fork (l, sync)
          | Call (a, _) -> Call(a, t1)
          | Pause | Finish -> fg
        in newfg
      in memo_rec (module Fgtbl3) children (fg, t1, t2)

    let fork_id = function
      | (Fg.Test (Fg.Sync (lid, _), _, _, _)) -> lid
      | _ -> []

    let eq_fork_id fg1 fg2 = fork_id fg1 = fork_id fg2

    let find ?stop nofinish fg t =
      let aux aux (fg, elt) =
        if fg == elt then
          if nofinish && (fg == Pause || fg == Finish) then None
          else Some fg
        else match fg with
          | Call (_, t) -> aux (t, elt)

          | Test (Sync _, t1, t2, _)  ->
            begin match stop with
            | Some stop when eq_fork_id stop fg -> Some stop
            | _ -> Option.mapn (aux (t1, elt)) (fun () -> aux (t2, elt))
            end
          | Test (_, t1, t2, _) ->
            Option.mapn (aux (t1, elt)) (fun () -> aux (t2, elt))
          | Fork (l, _) ->
            let rec f = function
              | [] -> None
              | h :: t ->
                Option.mapn (aux (h, elt)) (fun () -> f t)
            in f l
          | Pause | Finish -> None
      in memo_rec (module Fgtbl2) aux (fg, t)


    let rec find_join fg1 fg2 =
      let tbl1 = Fgtbl.create 17 in
      let tbl2 = Fgtbl.create 17 in
      let explore tbl1 tbl2 fg =
        let rec explore fg =
          if Fgtbl.mem tbl2 fg then Some fg
          else if Fgtbl.mem tbl1 fg then None
          else begin match fg with
            | Call (_, t) ->
              Fgtbl.add tbl1 fg ();
              explore t
            | Test (_, t1, t2, _) ->
              Fgtbl.add tbl1 fg ();
              begin match find_join t1 t2 with
                | None -> None
                | Some j' -> explore j'
              end
            | Pause | Finish -> None
            | Fork _ ->
              Fg.error ~loc:(Ast.dummy_loc ())
                (Invariant_violation (fg1, "Finished reached from interleaving : find_join"))
          end;
        in explore fg
      in
      ignore @@ explore tbl1 tbl2 fg1;
      explore tbl2 tbl1 fg2

    type test_op = F of test_value | T of test_value

    module DTbl = Hashtbl.Make (struct
        type t = (Fg.action * (test_op list)) list
        let hash = Hashtbl.hash
        let equal = (==)
      end)

    let pp_test fmt t =
      let op, t= match t with F t -> "F", t | T t -> "T", t in
      Format.fprintf fmt "%s(%a)" op pp_test_value_short t
    let pp_tests fmt = MList.pp_iter ~sep:" " pp_test fmt
    let pp_signals fmt set =
      MList.pp_iter ~sep:"," (fun fmt x -> Format.fprintf fmt "%s" x.ident.content) fmt
        (SignalSet.elements set)

    let pp_dline ?wr () fmt (a, ts) =
      match wr with
      | Some (w, r) ->
        Format.fprintf fmt "%a \t\t(%a | %a) :\t\t %a"
          pp_action a pp_signals r pp_signals w pp_tests ts
      | None ->
        Format.fprintf fmt "%a :\t\t %a" pp_action a pp_tests ts

    let extract_emits_tests_sets =
      let open SignalSet in
      let h = DTbl.create 19 in
      let rec extract_mem l =
        try DTbl.find h l with | Not_found ->
          match l with
          | [] -> SignalSet.empty, SignalSet.empty
          | (a, tests) :: l' ->
            let writes, _reads = extract_mem l' in
            let sig_tests = List.fold_left (fun acc test ->
                match test with
                | F (Signal (s, _)) | T (Signal (s, _)) -> add s acc
                | _ -> acc
              ) empty tests
            in
            let emits = match a with Emit sv -> add sv.signal writes | _ -> writes in
            let r = emits, sig_tests in
            DTbl.add h l r;r
      in extract_mem

    let pp_destruct b fmt l =
      let rec f fmt l =
        match l with
        | [] -> ()
        | (a, ts) :: l' ->
          Format.fprintf fmt "%a\n"
            (if b then pp_dline ~wr:(extract_emits_tests_sets l) () else pp_dline ()) (a, ts) ;
          f fmt l'
      in f fmt l
    let _pp_destruct = pp_destruct

(* L ← Empty list that will contain the sorted elements *)
(* S ← Set of all nodes with no incoming edge *)
(* while S is non-empty do *)
(*     remove a node n from S *)
(*     add n to tail of L *)
(*     for each node m with an edge e from n to m do *)
(*         remove edge e from the graph *)
(*         if m has no other incoming edges then *)
(*             insert m into S *)
(* if graph has edges then *)
(*     return error (graph has at least one cycle) *)
(* else  *)
(*     return L (a topologically sorted order) *)


    (* let actions_graph stop acc fg = *)
    (*   let visited = Fgtbl.create 17 in *)
    (*   let rec build_rel acc fg = *)
    (*     try Fgtbl.find visited fg @ acc with | Not_found -> *)
    (*       let r = begin match fg with *)
    (*         | fg when fg  == stop -> [] *)
    (*         | Call (act, fg') -> *)
    (*           let acts = build_rel [] fg' in *)
    (*           List.iter (Acttbl.add rels act) acts; *)
    (*           act :: acc *)
    (*         | Test (_, tb, fb, _) -> *)
    (*           build_rel (build_rel acc tb) fb *)
    (*         | Fork _ | Pause | Finish -> *)
    (*           Fg.error ~loc:Ast.Tagged.(env.pname.loc) *)
    (*             (Invariant_violation (fg, "Fork or end reached while interleaving")) *)
    (*       end in *)
    (*       Fgtbl.add visited fg r; *)
    (*       r *)
    (*   in *)
    (*   ignore @@ build_rel [] fg *)

    (* let action_topological_sort env stop fg = *)
    (*   let open Fg in *)
    (*   let rels = ActTbl.create 17 in *)



    let destruct env stop fg =
      let rec destruct (acc : 'a list) stop tests fg =
        match fg with
        | fg when fg == stop -> acc
        | Call (a, t) ->
          destruct ((a, tests) :: acc) stop tests t
        | Test (test, l, r, _) ->
          let stop' =
            match find_join l r with
            | None ->
              Fg.error ~loc:Ast.Tagged.(env.pname.loc)
                (Invariant_violation (fg, "No join after test"))
            | Some s -> s
          in
          let dl = destruct acc stop' (T test :: tests) l in
          let dr = destruct dl stop' (F test :: tests) r in
          destruct dr stop tests stop'
        | Fork _ ->
          (* Impossible by construction *)
          Fg.error ~loc:Ast.Tagged.(env.pname.loc)
            (Invariant_violation (fg, "Fork reached while interleaving"))
        | Finish | Pause ->
          (* Impossible by construction *)
          Fg.error ~loc:Ast.Tagged.(env.pname.loc)
            (Invariant_violation (fg, "Parallel leads to pause or exit"))
      in destruct [] stop [] fg

    (* emit, read *)

    (*
       Idea to avoid the quadratic complexity in general case :
       - annonce when a list may emit eventually in a Hashtbl indiced by signals
       - Read the lists with the same rules as two lists
       - if there is a signal test, check the table to see which list is needed, put it in first

     *)

    let interleave_lists env d1 d2 =
      let rec inter acc d1 d2 =
        match d1, d2 with
        (* cas de fin *)
        | [], [] -> acc
        (* une des liste est vide *)
        | [], d | d, [] ->
          List.fold_left (fun acc (a, t) -> (a, List.rev t) :: acc) acc d
        (* aucune branche ne dépend d'un test*)
        | (_, [] as at1) :: d1, (_, [] as at2) :: d2 -> inter (at1 :: at2 :: acc) d1 d2
        (* Une branche n'a pas de test *)
        | d1, ((_, [] as at) :: d2) | ((_, [] as at) :: d2), d1 -> inter (at :: acc) d1 d2
        (* Les deux ont un test *)
        | (a1, t1) :: d1', (a2, t2) :: d2' ->
          let d1_writes, d1_reads = extract_emits_tests_sets d1 in
          let d2_writes, d2_reads = extract_emits_tests_sets d2 in
          let inter_d1r_d2w = SignalSet.inter d1_reads d2_writes in
          if inter_d1r_d2w = SignalSet.empty then
            inter ((a1, List.rev t1) :: acc) d1' d2
          else
            let inter_d2r_d1w = SignalSet.inter d2_reads d1_writes in
            if inter_d2r_d1w = SignalSet.empty then
              inter ((a2, List.rev t2) :: acc) d1 d2'
            else
              let signals = SignalSet.(elements (union inter_d1r_d2w inter_d2r_d1w)) in
              Fg.(error ~loc:Ast.Tagged.(env.pname.loc) @@ Cyclic_causality (Finish, signals))
      in List.rev @@ inter [] d1 d2



    let rebuild_destruct env fg l =
      let gen_tests acc (a, ts) =
        let old_acc = acc in
        List.fold_left (fun acc x -> match x with
            | T test -> Test (test, acc, old_acc, Some old_acc)
            | F test -> Test (test, old_acc, acc, Some old_acc)
          ) (Call (a, acc)) (List.rev ts)
      in
      let rec visit_tests (a, ts) acc t t_path f_path endt =

        (* List. *)

        match ts with
        | [] -> assert false
        | T t' :: ts' ->
          if Fg.test_eq t t' then
            Test (t, rebuild t_path [a, ts'], f_path, endt)
          else gen_tests acc (a, ts)
        | F t' :: ts' ->
          if Fg.test_eq t' t then
            Test (t, t_path, rebuild f_path [a,ts'], endt)
          else gen_tests acc (a, ts)
      and rebuild acc l =
        match l with
        | [] -> acc
        | (a, []) :: l ->
          begin match acc with
            | Call (a', fg) ->
              (rebuild (Call (Compressed (a, a'), fg)) l)
            | _ -> rebuild (Call (a, acc)) l
          end
        | (a, ts) :: l ->
          begin match acc with
            | Call _ -> rebuild (gen_tests acc (a, ts)) l
            | Test (t, t_path, f_path, endt) ->
              let visited = visit_tests (a, ts) acc t t_path f_path endt in
              rebuild visited l
            | _ -> Fg.error ~loc:Ast.Tagged.(env.pname.loc)
            (Invariant_violation (fg, "Fork reached while interleaving"))
          end
      in
      rebuild fg @@ List.rev l


    let interleave _options env _grcenv fg =
      let visit_tbl = Fgtbl.create 17 in
      let rec visit fg =
        try
          Fgtbl.find visit_tbl fg
        with Not_found ->
          let fg' = match fg with
            | Call (a, t) -> Call (a, visit t)
            | Test (tv, t1, t2, tend) ->
              let tv = match tv with
                | Sync (lid, exits) ->
                  let map = IntMap.(
                      fold (fun k v m -> add k (List.map visit v) m) IntMap.empty exits
                    )
                  in Sync (lid, map)
                | tv -> tv
              in
              Test (tv, visit t1, visit t2, Option.map visit tend)
            | Fork (l, sync) ->
              let visited = List.map visit l in
              let sync = visit sync in

              (* action_topological_sort env sync (List.hd visited); *)

              (* let t0 = Unix.gettimeofday () in
               * Format.printf "Init @\n";
               * 
               * Format.printf "stop : %a\n" Fg.pp_head sync;
               * Format.printf "start : %a\n" Fg.pp_head (List.hd visited); *)

              let destruct d = List.rev @@ destruct env sync d in


              let destructed = List.map destruct visited in

              (* let t1 = Unix.gettimeofday () in
               * Format.printf "Destruct %f@\n" (t1 -. t0); *)
              let inter_r_l = List.fold_left (interleave_lists env) [] destructed in


              (* let t2 = Unix.gettimeofday () in
               * Format.printf "Interleave %f@\n" (t2 -. t1);
               * 
               * Format.printf "%a" (MList.pp_iter ~sep:"---\n" (pp_destruct true)) destructed; *)

              (* Format.printf "----------\n%a#########\n\n" (pp_destruct false) inter_r_l; *)

              let r = rebuild_destruct env sync inter_r_l in
              (* let t3 = Unix.gettimeofday () in
               * Format.printf "Rebuild %f@\n" (t3 -. t2); *)
              r

            | Pause -> Pause
            | Finish -> Finish
          in
          Fgtbl.add visit_tbl fg fg'; fg'
      in
      Fg.compress @@ visit fg



    module Stats = struct

      let size fg =
        let h = Fgtbl.create 119 in
        let rec aux fg =
          try Fgtbl.find h fg with
          | Not_found ->
            Fgtbl.add h fg ();
            begin match fg with
              | Call (Exit _, t) -> aux t
              | Call (_, t) -> aux t
              | Test (_, t1, t2, _) ->
                aux t1; aux t2
              | Fork (l, _) ->
                List.iter aux l
              | Pause | Finish -> ()
            end
        in
        aux fg; Fgtbl.length h

      let pp fmt fg =
        Format.fprintf fmt "size: %d" (size fg)

    end


  end

end
