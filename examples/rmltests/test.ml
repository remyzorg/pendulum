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
let () =
      Rml_machine.rml_exec
        ([])
        ((function
           | ()  -> Lco_ctrl_tree_record.rml_run (function | ()  -> main ) ):
          (_) Lco_ctrl_tree_record.process) 
;;
let main2 =
      ((function
         | ()  ->
             Lco_ctrl_tree_record.rml_signal
               (function
                 | s__sig_3  ->
                     Lco_ctrl_tree_record.rml_signal
                       (function
                         | x__sig_4  ->
                             Lco_ctrl_tree_record.rml_signal
                               (function
                                 | y__sig_5  ->
                                     Lco_ctrl_tree_record.rml_signal
                                       (function
                                         | w__sig_6  ->
                                             Lco_ctrl_tree_record.rml_par
                                               (Lco_ctrl_tree_record.rml_loop
                                                 (Lco_ctrl_tree_record.rml_seq
                                                   (Lco_ctrl_tree_record.rml_emit_val'
                                                     s__sig_3
                                                     (function | ()  -> 5 ))
                                                   Lco_ctrl_tree_record.rml_pause))
                                               (Lco_ctrl_tree_record.rml_control'
                                                 w__sig_6
                                                 (Lco_ctrl_tree_record.rml_loop
                                                   (Lco_ctrl_tree_record.rml_seq
                                                     (Lco_ctrl_tree_record.rml_present'
                                                       s__sig_3
                                                       (Lco_ctrl_tree_record.rml_compute
                                                         (function
                                                           | ()  ->
                                                               Lco_ctrl_tree_record.rml_expr_emit
                                                                 x__sig_4
                                                           ))
                                                       (Lco_ctrl_tree_record.rml_compute
                                                         (function
                                                           | ()  ->
                                                               Lco_ctrl_tree_record.rml_expr_emit
                                                                 y__sig_5
                                                           )))
                                                     Lco_ctrl_tree_record.rml_pause)))
                                         )
                                 )
                         )
                 )
         ):
        (_) Lco_ctrl_tree_record.process) 
;;
