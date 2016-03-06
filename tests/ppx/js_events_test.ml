
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

let%sync emit_basic elt elt2 =
  input (elt3 : int);
  loop begin
    present elt##onmouseover
      (emit elt2##.textContent (Js.string "lol"));
    pause
  end
  ||
  loop begin
    present elt##onmouseover
      (emit elt2##.textContent (Js.string "lol"));
    pause
  end


let%sync emit_basic0 elt0 elt1 elt2 ~dsource =
  input (elt3 : int), (elt4 : int), elt5;
  input (elt6 : int), (elt7 : int), elt8;

  loop pause
