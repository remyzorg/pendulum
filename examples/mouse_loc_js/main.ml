open Dom_html
open Format


let%sync print_lol s =
  let n = 0 in
  loop begin
    emit n (!!n + 1)
    ; emit s##.textContent Js.(some (string @@ string_of_int !!n))
    ; pause
  end

let%sync print_lol s =
  let n = 0 in
  loop begin
    emit n (!!n + 1)
    ; emit s##.textContent Js.(some (string @@ string_of_int !!n))
    ; pause
  end

let%sync mouse =
  input s;
  element s2;
  input w {
    onmousemove =
      "",
      (fun x ev -> sprintf "%d,%d" ev##.clientX ev##.clientY);
  };


  run print_lol s2
  ||
  loop begin
    present w##onmousemove begin
      emit s##.textContent (Js.(some (string !!(w##onmousemove))))
    end;
    pause ;
  end
;;

let onload _ =
  let s = createSpan document in
  let s2 = createSpan document in
  Dom.appendChild document##.body s;
  Dom.appendChild document##.body (createBr document);
  Dom.appendChild document##.body s2;
  mouse#create (s, s2, window);
  Js._false
;;

window##.onload := handler onload;;

