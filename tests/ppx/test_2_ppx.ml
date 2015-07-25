
open OUnit
open Pendulum.Runtime_ast

let%to_dot_grc mouse_machine =
  input ctx;
  input move;
  input quit1;
  input quit2;

  let circle  = () in
  trap ex1 (
    loop (
      present quit1 (exit ex1);
      present move (
        atom ();
        emit circle ()
      ); pause)
  );
  atom (erase_circle !!ctx !!circle)
  ||
  let circle = () in
  begin
    trap ex2 (
      loop (
        present quit2 (exit ex2);
        present move (
          atom ();
          emit circle ()
        ); pause));
    atom (erase_circle !!ctx !!circle)
  end
