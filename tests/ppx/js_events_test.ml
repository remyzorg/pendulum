
open Pendulum.Runtime_ast


let%sync basic =
  input elt;
  input elt2;

  loop begin
    present elt2##onmouseover
      (atom (Format.printf "mouseover\n"))
  end
  ||
  loop begin
    present elt##onclick
      (atom (Format.printf "click\n"))
  end



let%sync emit_basic ~dsource =
  input elt;
  input elt2;

  loop begin
    present elt##onmouseover
      (emit elt2##.textContent (Js.string "lol"));
    pause
  end

