
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
  (* || loop pause *)
  (* || loop pause *)
  (* || loop pause *)



let many_par_lol =
let open Pendulum.Runtime_misc in
  let open Pendulum.Machine in
    fun ()  ->
      let pendulum_state = Bitset.make 7 in
      let set_absent () = () in
      fun ()  ->
        fun ()  ->
          try
            if Bitset.mem pendulum_state 0
            then raise Finish_exc
            else
              if Bitset.mem pendulum_state 5
              then
                (if Bitset.mem pendulum_state 2
                 then
                   (Bitset.remove pendulum_state 1;
                    Bitset.add pendulum_state 1;
                    if Bitset.mem pendulum_state 4
                    then
                      (Bitset.remove pendulum_state 3;
                       Bitset.add pendulum_state 3))
                 else
                   if Bitset.mem pendulum_state 4
                   then
                     (Bitset.remove pendulum_state 3;
                      Bitset.add pendulum_state 3);
                 if
                   (Bitset.mem pendulum_state 2) ||
                     (Bitset.mem pendulum_state 4)
                 then raise Pause_exc
                 else (Bitset.remove pendulum_state 5; raise Finish_exc))
              else
                (Bitset.add pendulum_state 5;
                 Bitset.add pendulum_state 2;
                 Bitset.add pendulum_state 4;
                 Bitset.add pendulum_state 1;
                 Bitset.add pendulum_state 3;
                 if
                   (Bitset.mem pendulum_state 2) ||
                     (Bitset.mem pendulum_state 4)
                 then raise Pause_exc
                 else (Bitset.remove pendulum_state 5; raise Finish_exc))
          with | Pause_exc  -> (set_absent (); Pause)
          | Finish_exc  ->
              (set_absent (); Bitset.add pendulum_state 0; Finish)


let many_par_lol =
  let open Pendulum.Runtime_misc in
  let open Pendulum.Machine in
  fun ()  ->
    let pendulum_state = Bitset.make 7 in
    let set_absent () = () in
    fun ()  ->
    fun ()  ->
      try
        if Bitset.mem pendulum_state 0
        then raise Finish_exc
        else
        if Bitset.mem pendulum_state 5
        then
          (if Bitset.mem pendulum_state 2
           then
             (Bitset.remove pendulum_state 1;
              Bitset.add pendulum_state 1;)
           else ();
           if Bitset.mem pendulum_state 4
           then
             (Bitset.remove pendulum_state 3;
              Bitset.add pendulum_state 3))
        else
          (Bitset.add pendulum_state 5;
           Bitset.add pendulum_state 2;
           Bitset.add pendulum_state 4;
           Bitset.add pendulum_state 1;
           Bitset.add pendulum_state 3);
        if
          (Bitset.mem pendulum_state 2) ||
          (Bitset.mem pendulum_state 4)
        then raise Pause_exc
        else (Bitset.remove pendulum_state 5; raise Finish_exc)
      with | Pause_exc  -> (set_absent (); Pause)
           | Finish_exc  ->
             (set_absent (); Bitset.add pendulum_state 0; Finish)
