
open OUnit
open Pendulum.Runtime_ast

let%sync mouse_machine1 =
  input s;
  let s' = !!s + 1 in
  let s'' = !!s' + 1 in
  (loop (pause))


let%sync incr =
  input s;
  emit s (!!s + 1)


let%to_dot_grc m_loop_incr =
  input zz;
  let s = 5 in
  loop (
    run incr s;
    pause
  )

(* let m = *)
(*   let open Pendulum.Runtime_misc in *)
(*   let open Pendulum.Machine in *)
(*   fun ()  -> *)
(*     let pendulum_state = Bitset.make 7 in *)
(*     let s = ref (make_signal 5) in *)
(*     let set_absent () = set_absent (!s); () in *)
(*     fun ()  -> *)
(*     fun ()  -> *)
(*       try *)
(*         if Bitset.mem pendulum_state 0 *)
(*         then raise Finish_exc *)
(*         else *)
(*         if Bitset.mem pendulum_state 5 *)
(*         then *)
(*           (if Bitset.mem pendulum_state 1 *)
(*            then *)
(*              (if (incr (!! (!s))) == Pause *)
(*               then raise Pause_exc *)
(*               else *)
(*                 (Bitset.remove pendulum_state 1; *)
(*                  Bitset.add pendulum_state 2; *)
(*                  raise Pause_exc)) *)
(*            else *)
(*              (Bitset.remove pendulum_state 2; *)
(*               Bitset.remove pendulum_state 3; *)
(*               Bitset.add pendulum_state 3; *)
(*               Bitset.add pendulum_state 1; *)
(*               incr := (make_signal (incr (!! (!s)))); *)
(*               if (incr (!! (!s))) == Pause *)
(*               then raise Pause_exc *)
(*               else *)
(*                 (Bitset.remove pendulum_state 1; *)
(*                  Bitset.add pendulum_state 2; *)
(*                  raise Pause_exc))) *)
(*         else *)
(*           (Bitset.add pendulum_state 5; *)
(*            s := (make_signal 5); *)
(*            Bitset.add pendulum_state 4; *)
(*            Bitset.add pendulum_state 3; *)
(*            Bitset.add pendulum_state 1; *)
(*            incr := (make_signal (incr (!! (!s)))); *)
(*            if (incr (!! (!s))) == Pause *)
(*            then raise Pause_exc *)
(*            else *)
(*              (Bitset.remove pendulum_state 1; *)
(*               Bitset.add pendulum_state 2; *)
(*               raise Pause_exc)) *)
(*       with | Pause_exc  -> (set_absent (); Pause) *)
(*            | Finish_exc  -> *)
(*              (set_absent (); Bitset.add pendulum_state 0; Finish) *)
