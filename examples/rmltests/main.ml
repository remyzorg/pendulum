
open Implem_lco_ctrl_tree_record


open Lco_ctrl_tree_record



(* let s = rml_global_signal_combine [] (fun x y -> x :: y) *)
  (* let react = rml_make (my_loop s) in *) (* object *) (*   method set_s str = rml_hard_emit s str; () *)
  (*   method get_s = rml_pre_value s *)
  (*   method react = ignore @@ react () *)
  (* end *)


(* let react = Lco_ctrl_tree_record.rml_make (my_loop s) *)
(* let _ = *)
(*   Lco_ctrl_tree_record.rml_expr_emit_val s "emit 0"; *)
(*   react (); *)
(*   react (); *)
(*   react (); *)
(*   Lco_ctrl_tree_record.rml_expr_emit_val s "emit 1"; *)
(*   react (); *)
(*   react (); *)
(*   react (); *)
(*   Lco_ctrl_tree_record.rml_expr_emit_val s "emit 2"; *) (*   react (); *)
(*   react () *)


(* open Implem_lco_ctrl_tree_record;; *)
(* let s = *)
(*   Lco_ctrl_tree_record.rml_global_signal_combine [] (fun x y -> x::y) *)

(* let p () = *)
(*   Lco_ctrl_tree_record.rml_par *)
(*     (Lco_ctrl_tree_record.rml_loop *)
(*        (Lco_ctrl_tree_record.rml_await_immediate_one' s *)
(*           (fun x -> *)
(*             Lco_ctrl_tree_record.rml_seq *)
(*               (Lco_ctrl_tree_record.rml_compute (fun () -> print_endline x)) *)
(*               Lco_ctrl_tree_record.rml_pause *)
(*           ))) *)
(*     (Lco_ctrl_tree_record.rml_def *)
(*        (fun () -> ref 0) *)
(*        (fun cpt -> *)
(*          Lco_ctrl_tree_record.rml_loop *)
(*            (Lco_ctrl_tree_record.rml_seq *)
(*               (Lco_ctrl_tree_record.rml_compute *)
(*                  (fun () -> *)
(*                    incr cpt; *)
(*                    print_int !cpt; *)
(*                    print_newline ())) *)
(*               Lco_ctrl_tree_record.rml_pause))) *)


(* let react = Lco_ctrl_tree_record.rml_make p *)
(* let _ = *)
(*   Lco_ctrl_tree_record.rml_expr_emit_val s "emit 0"; *)
(*   react (); *)
(*   react (); *)
(*   react (); *)
(*   react (); *)
(*   react (); *)
(*   Lco_ctrl_tree_record.rml_expr_emit_val s "emit 1"; *)
(*   react (); *)
(*   react (); *)
(*   react (); *)
(*   Lco_ctrl_tree_record.rml_expr_emit_val s "emit 2"; *)
(*   react (); *)
(*   react () *)


let (!!) = rml_pre_value

let%rml p s ~dsource =
  loop begin
    await s;
    !(print_endline "hello");
    pause
  end
  ||
  let cpt = 0 in
  loop begin
    emit cpt (!!cpt + 1);
    !(print_int !!cpt);
    !(print_newline ());
    pause
  end

(* let p s ~dsource = *)
(*   let create_local s = *)
(*     let p_react = *)
(*       let open Lco_ctrl_tree_record in *)
(*       rml_make *)
(*         (fun ()  -> *)
(*            rml_par *)
(*              (rml_loop *)
(*                 (rml_seq (rml_await (fun ()  -> s)) *)
(*                    (rml_seq *)
(*                       (rml_compute (fun ()  -> print_endline "hello"; ())) *)
(*                       rml_nothing))) *)
(*              (rml_signal_combine (fun ()  -> 0) *)
(*                 (fun ()  -> fun acc  -> fun x  -> x) *)
(*                 (fun cpt  -> *)
(*                    rml_loop *)
(*                      (rml_seq (rml_emit_val' cpt (fun ()  -> (!! cpt) + 1)) *)
(*                         (rml_seq *)
(*                            (rml_compute (fun ()  -> print_int (!! cpt); ())) *)
(*                            (rml_seq *)
(*                               (rml_compute (fun ()  -> print_newline (); ())) *)
(*                               rml_nothing)))))) *)
(*     in *)
(*     object method s = rml_expr_emit_val s method react = p_react () end  in *)
(*   object *)
(*     method create s = *)
(*       create_local *)
(*         (Lco_ctrl_tree_record.rml_global_signal_combine *)
(*            create *)
(*            s *)
(*            (fun acc  -> fun x  -> x)) *)
(*   end *)
