
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

(* let%sync many_par = *)
(*   loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)
(*   || loop pause *)

(* let%sync reactive_player = *)
(*   input play; *)
(*   input pause; *)
(*   input start_slide; *)
(*   input stop_slide; *)
(*   input media_time; *)
(*   input media; *)
(*   input slider; *)

(*   let cant_update = () in *)
(*   loop begin *)
(*     present play (atom (print_string "play")); *)
(*     present pause (atom (print_string "pause")); *)
(*     pause *)
(*   end *)
(*   || loop begin *)
(*     await start_slide; *)
(*     trap t' (loop ( *)
(*         emit cant_update (); *)
(*         present stop_slide ( *)
(*           atom (print_string "stop_slide"); *)
(*           exit t'); *)
(*         pause) *)
(*       ); *)
(*     pause *)
(*   end *)
(*   || *)
(*   loop begin present cant_update nothing *)
(*       (present media_time (atom( *)
(*            print_string "media_time" *)
(*          ))); pause *)
(*   end *)


let%sync reactive_player =
  input play;
  input start_slide;
  input stop_slide;

  loop begin
    present play (emit play ());
    pause
  end
  ||
  loop begin
    present start_slide
      pause;
    pause
  end

