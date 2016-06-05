open Dom_html
open Format

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let%sync debug s n = !(debug "%s : %d" !!s !!n)

let%sync mouse =
  input s;
  element s2;

  input w {
    onmousemove =
      "",
      (fun x ev -> sprintf "%d,%d" ev##.clientX ev##.clientY);
  };

  let n = 0 in
  loop begin
    present w##onmousemove begin
      emit s##.textContent (Js.(some (string !!(w##onmousemove))))
    ; emit n (!!n + 1)
    end
    ; pause
  end
  (* || loop begin run debug (w##onmousemove, n); pause end *)
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

