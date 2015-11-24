
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


let%to_dot_grc reactive_player =
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

let player_lol =
  let open Pendulum.Runtime_misc in
  let open Pendulum.Machine in
  fun (play,start_slide,stop_slide)  ->
    let pendulum_state = Bitset.make 15 in
    let play = make_signal play in
    let start_slide = make_signal start_slide in
    let stop_slide = make_signal stop_slide in
    let set_absent () =
      set_absent play; set_absent start_slide; set_absent stop_slide; () in


    let f () =
      try
        (Bitset.remove pendulum_state 3;
         if !? start_slide
         then
           (Bitset.add pendulum_state 8;
            Bitset.add pendulum_state 4;
            if
              (Bitset.mem pendulum_state 6) ||
              (Bitset.mem pendulum_state 12)
            then
              raise Pause_exc
            else
              (Bitset.remove pendulum_state 13; raise Finish_exc))
         else
           (Bitset.remove pendulum_state 9;
            Bitset.add pendulum_state 10;
            Bitset.add pendulum_state 4;
            if
              (Bitset.mem pendulum_state 6) ||
              (Bitset.mem pendulum_state 12)
            then raise Pause_exc
            else
              (Bitset.remove pendulum_state 13; raise Finish_exc));
         if
           (Bitset.mem pendulum_state 6) ||
           (Bitset.mem pendulum_state 12)
         then raise Pause_exc
         else (Bitset.remove pendulum_state 13; raise Finish_exc))
      with | Pause_exc  -> Pause
           | Finish_exc  -> Finish
    in
    f ()

