(* THIS FILE IS GENERATED. *)
(* rmlc test.rml  *)

open Implem_lco_ctrl_tree_record;;
let main =
      ((function
         | ()  ->
             Lco_ctrl_tree_record.rml_compute
               (function | ()  -> Pervasives.print_endline "Hello world!" )
         ):
        (_) Lco_ctrl_tree_record.process) 
;;
let main2 =
      (function
        | s__val_rml_3  ->
            (function
              | w__val_rml_4  ->
                  ((function
                     | ()  ->
                         Lco_ctrl_tree_record.rml_signal
                           (function
                             | x__sig_5  ->
                                 Lco_ctrl_tree_record.rml_signal
                                   (function
                                     | y__sig_6  ->
                                         Lco_ctrl_tree_record.rml_par
                                           (Lco_ctrl_tree_record.rml_loop
                                             (Lco_ctrl_tree_record.rml_seq
                                               (Lco_ctrl_tree_record.rml_emit_val'
                                                 s__val_rml_3
                                                 (function | ()  -> 5 ))
                                               Lco_ctrl_tree_record.rml_pause))
                                           (Lco_ctrl_tree_record.rml_control'
                                             w__val_rml_4
                                             (Lco_ctrl_tree_record.rml_loop
                                               (Lco_ctrl_tree_record.rml_seq
                                                 (Lco_ctrl_tree_record.rml_present'
                                                   s__val_rml_3
                                                   (Lco_ctrl_tree_record.rml_compute
                                                     (function
                                                       | ()  ->
                                                           Lco_ctrl_tree_record.rml_expr_emit
                                                             x__sig_5
                                                       ))
                                                   (Lco_ctrl_tree_record.rml_compute
                                                     (function
                                                       | ()  ->
                                                           Lco_ctrl_tree_record.rml_expr_emit
                                                             y__sig_6
                                                       )))
                                                 Lco_ctrl_tree_record.rml_pause)))
                                     )
                             )
                     ):
                    (_) Lco_ctrl_tree_record.process)
              )
        ) 
;;
let my_loop =
      (function
        | input__val_rml_8  ->
            ((function
               | ()  ->
                   Lco_ctrl_tree_record.rml_loop
                     (Lco_ctrl_tree_record.rml_seq
                       (Lco_ctrl_tree_record.rml_present'
                         input__val_rml_8
                         (Lco_ctrl_tree_record.rml_compute
                           (function
                             | ()  -> Pervasives.print_endline "Hello world"
                             ))
                         Lco_ctrl_tree_record.rml_nothing)
                       Lco_ctrl_tree_record.rml_pause)
               ):
              (_) Lco_ctrl_tree_record.process)
        )
;;
(* Rml_machine.rml_exec *)
(*   ([]) *)
(*   ((function *)
(*      | ()  -> *)
(*          Lco_ctrl_tree_record.rml_signal *)
(*            (function *)
(*              | s__sig_9  -> *)
(*                  Lco_ctrl_tree_record.rml_run *)
(*                    (function | ()  -> my_loop s__sig_9 ) *)
(*              ) *)
(*      ): *)
(*     (_) Lco_ctrl_tree_record.process);; *)



let _ =
  let open Lco_ctrl_tree_record in
  let s = rml_global_signal_combine "" (fun acc x -> x) in
  let react = rml_make (my_loop s) in
  ignore @@ react ();
  ignore @@ (rml_make (fun () -> rml_emit (fun () -> s))) ();
  ignore @@ react ();
  ignore @@ react ()
