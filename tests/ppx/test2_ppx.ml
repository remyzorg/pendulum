
open OUnit
open Pendulum.Runtime_ast

(* let%sync mouse_machine1 = *)
(*   input s; *)
(*   let s' = !!s + 1 in *)
(*   let s'' = !!s' + 1 in *)
(*   (loop (pause)) *)


(* let%sync incr = *)
(*   input s; *)
(*   input s2; *)
(*   emit s (!!s + 1 + !!s2); *)
(*   atom (Format.printf "%d" !!s) *)

(* let%sync m_loop_incr = *)
(*   input zz; *)
(*   let s1 = 5 in *)
(*   let s2 = 5 in *)
(*   loop ( *)
(*     run incr (s2, s1); *)
(*     pause *)
(*   ) *)
(*   || *)
(*   loop ( *)
(*     run incr (s1, s2); *)
(*     pause *)
(*   ) *)

let%to_dot_grc many_par =
  loop pause
  || loop pause
  || loop pause


(* let%to_dot_grc reactive_player = *)
(*   input play; *)
(*   input pause; *)
(*   input start_slide; *)
(*   input stop_slide; *)
(*   input media_time; *)
(*   input media; *)
(*   input slider; *)

(*   let cant_update = () in *)
(*   loop begin *)
(*     trap t' (loop ( *)
(*         present stop_slide ( *)
(*           atom (print_string "stop_slide"); *)
(*           exit t'); *)
(*         pause) *)
(*       ); *)
(*     pause *)
(*   end *)

