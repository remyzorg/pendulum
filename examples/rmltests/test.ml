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
                       (Lco_ctrl_tree_record.rml_compute
                         (function
                           | ()  ->
                               Pervasives.print_endline "Hello world";
                                 Lco_ctrl_tree_record.rml_expr_emit
                                   input__val_rml_8
                           ))
                       Lco_ctrl_tree_record.rml_pause)
               ):
              (_) Lco_ctrl_tree_record.process)
        )
;;
let my_loop' = ((function
     | ()  ->
         Lco_ctrl_tree_record.rml_signal
           (function
             | s__sig_9  ->
                 Lco_ctrl_tree_record.rml_run
                   (function | ()  -> my_loop s__sig_9 )
             )
     ):
    (_) Lco_ctrl_tree_record.process);;

let _ =
  let react = Lco_ctrl_tree_record.rml_make my_loop' in
  for i = 0 to 9 do
    ignore @@ react ()
  done
