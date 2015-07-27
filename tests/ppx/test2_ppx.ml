
open OUnit
open Pendulum.Runtime_ast

(* let%to_dot_grc mouse_machine = *)
(*   input s; *)
(*   trap t (loop ( *)
(*       present s (exit t); *)
(*       pause) *)
(*     ); *)
(*   atom () || atom () *)

let%to_dot_grc mouse_machine =
  loop (pause) ;
  atom () || atom ()


let mouse_machine =
  let open Pendulum.Runtime_misc in
  let open Pendulum.Machine in
  fun ()  ->
    let pendulum_state = Bitset.make 8 in
    let set_absent () = () in
    fun ()  ->
      if Bitset.mem pendulum_state 0
      then (set_absent (); Bitset.add pendulum_state 0)
      else
        (if Bitset.mem pendulum_state 6
         then
           (if Bitset.mem pendulum_state 2
            then Bitset.remove pendulum_state 1
            else
            if
              (Bitset.mem pendulum_state 3) ||
              (Bitset.mem pendulum_state 4)
            then (set_absent ())
            else
              (Bitset.remove pendulum_state 5;
               Bitset.remove pendulum_state 6;
               set_absent ();
               Bitset.add pendulum_state 0
               ))
         else (Bitset.add pendulum_state 6; Bitset.add pendulum_state 2);
         Bitset.add pendulum_state 1;
         set_absent ();
         )
