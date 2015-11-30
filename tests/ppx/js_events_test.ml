
open Pendulum.Runtime_ast


let%to_dot_grc basic =
  input elt;
  input elt2;

  loop begin
    present elt2##mousedown
      (atom (Format.printf "click\n"))
  end
  ||
  loop begin
    present elt##click
      (atom (Format.printf "click\n"))
  end


