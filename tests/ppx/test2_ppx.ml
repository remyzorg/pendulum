
open OUnit
open Pendulum.Runtime_ast

let%sync mouse_machine =
  input s;
  let s' = !!s + 1 in
  let s'' = !!s' + 1 in
  (loop (pause))

