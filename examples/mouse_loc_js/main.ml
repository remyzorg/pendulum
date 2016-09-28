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
  element w {
    onmousemove = "", (fun x ev -> sprintf "%d,%d" ev##.clientX ev##.clientY);
  };
  output write;
  loop begin
    present w##onmousemove
      (emit write !!(w##onmousemove))
  ; pause
  end

let onload _ =
  let sp = createSpan document in
  Dom.appendChild document##.body sp;
  let write_f v = sp##.textContent := Js.(some (string v)) in
  let _m = mouse#create window ("", write_f) in
  Js._false
;;

window##.onload := handler onload;;


