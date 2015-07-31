
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

let%sync m_loop_incr =
  let s = 5 in
  loop (
    run incr (s, s, s, s);
    pause
  )
