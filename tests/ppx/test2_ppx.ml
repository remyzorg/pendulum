
open OUnit
open Pendulum.Runtime_ast

let%to_dot_grc mouse_machine =
  input s;
  trap t (loop (
      present s (exit t);
      pause)
    );
  atom () || atom ()

exception Pause_exc
exception Finish_exc

let m =
let open Pendulum.Runtime_misc in
  let open Pendulum.Machine in
    fun s  ->
      let pendulum_state = Bitset.make 13 in
      let s = make_signal s in
      let set_absent () = set_absent s; () in
      ((fun set_arg  -> set_present_value s set_arg),
        (fun ()  ->
          try
           if Bitset.mem pendulum_state 0
           then (set_absent (); Bitset.add pendulum_state 0; raise Finish_exc)
           else
             if Bitset.mem pendulum_state 11
             then (
               if Bitset.mem pendulum_state 7
               then
                 (Bitset.remove pendulum_state 4;
                  Bitset.remove pendulum_state 5;
                  Bitset.add pendulum_state 5;
                  Bitset.add pendulum_state 3;
                  if !? s
                  then
                    (Bitset.remove pendulum_state 7;
                     Bitset.add pendulum_state 10;
                     (let () = () in ());
                     (let () = () in ());
                     if
                       (Bitset.mem pendulum_state 8) ||
                       (Bitset.mem pendulum_state 9)
                     then (set_absent (); raise Pause_exc)
                     else
                       (Bitset.remove pendulum_state 10;
                        Bitset.remove pendulum_state 11;
                        set_absent ();
                        Bitset.add pendulum_state 0;
                        raise Finish_exc))
                  else
                    (Bitset.remove pendulum_state 3;
                     Bitset.add pendulum_state 4;
                     set_absent ();
                     raise Pause_exc))
               ;
               if
                 (Bitset.mem pendulum_state 8) ||
                 (Bitset.mem pendulum_state 9)
               then (set_absent (); raise Pause_exc)
               else
                 (Bitset.remove pendulum_state 10;
                  Bitset.remove pendulum_state 11;
                  set_absent ();
                  Bitset.add pendulum_state 0;
                  raise Finish_exc))
             else
               (Bitset.add pendulum_state 11;
                Bitset.add pendulum_state 7;
                Bitset.add pendulum_state 6;
                Bitset.add pendulum_state 5;
                Bitset.add pendulum_state 3;
                if !? s
                then
                  (Bitset.remove pendulum_state 7;
                   Bitset.add pendulum_state 10;
                   (let () = () in ());
                   (let () = () in ());
                   if
                     (Bitset.mem pendulum_state 8) ||
                     (Bitset.mem pendulum_state 9)
                   then (set_absent (); raise Pause_exc)
                   else
                     (Bitset.remove pendulum_state 10;
                      Bitset.remove pendulum_state 11;
                      set_absent ();
                      Bitset.add pendulum_state 0;
                      raise Finish_exc))
                else
                  (Bitset.remove pendulum_state 3;
                   Bitset.add pendulum_state 4;
                   set_absent ();
                   raise Pause_exc))
          with
          | Pause_exc -> Pause
          | Finish_exc -> Finish
        ))


(* let%sync mouse_machine = *)
(*   loop (pause) ; *)
(*   atom () || atom () *)

