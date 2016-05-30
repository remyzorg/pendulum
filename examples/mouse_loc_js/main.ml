open Dom_html
open Format

let%sync mouse =
  input s;
  input w {
    onmousemove =
      "",
      (fun x ev -> sprintf "%d,%d" ev##.clientX ev##.clientY);
  };

  loop begin
    present w##onmousemove begin
      emit s##.textContent (Js.(some (string !!(w##onmousemove))))
    end;
    pause
  end
;;

let onload _ =
  let s = createSpan document in
  Dom.appendChild document##.body s;
  mouse (s, window);
  Js._false
;;

window##.onload := handler onload;;

