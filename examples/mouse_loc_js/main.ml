open Dom_html
open Format

open Tyxml_js

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let%sync debug s n = !(debug "%s : %d" !!s !!n)

let react = (^)

let%sync mouse =
  element w {
    onmousemove = "", (fun x ev -> sprintf "%d,%d" ev##.clientX ev##.clientY);
  };
  react write;
  loop begin
    present w##onmousemove
      (emit write !!(w##onmousemove))
  ; pause
  end

let onload _ =
  (* let sp = createSpan document in *)
  (* let write_f v = sp##.textContent := Js.(some (string v)) in *)

  (* let mysig, setme = React.S.create "" in *)

  let _m = mouse#create window ("") in

  let sp5 = R.Html5.(span @@ ReactiveData.RList.from_signal (
      React.S.map (fun (x : string) -> [Html5.pcdata x]) _m#write
    ))
  in
  Dom.appendChild document##.body (Tyxml_js.To_dom.of_span sp5);
  Js._false
;;

window##.onload := handler onload;;


