
open OUnit
open Pendulum.Runtime_ast

let%sync mouse_machine =
  input s;
  trap t (loop (
      present s (exit t);
      pause)
    );
  atom () || atom ()


(* let%sync mouse_machine = *)
(*   loop (pause) ; *)
(*   atom () || atom () *)

