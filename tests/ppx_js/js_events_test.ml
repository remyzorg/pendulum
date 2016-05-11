
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



let%sync emit_basic =
  input elt;
  input elt2;

  loop begin
    present elt2##onmouseover
      (emit elt2##.value##.textcontent (Js.string "lol"));
    pause
  end


let%sync instant_abs =
  input a;
  signal b begin
    signal w begin
      loop begin
        present a##click begin
          present b nothing begin
            emit w
          end
        end;
        pause
      end
      ||
      loop begin
        present w
          (atom (write state)) (* side effect *)
      end
    end
  end






