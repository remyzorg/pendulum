open Dom_html
open Format

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let%sync debug s n = !(debug "%s : %d" !!s !!n)

let%sync mouse ~print:(dot, pdf) =
  element sp;
  element w {
    onmousemove =
      "",
      (fun x ev -> sprintf "%d,%d" ev##.clientX ev##.clientY);
  };
  loop begin
    present w##onmousemove !begin sp##.textContent := Js.(some (string !!(w##onmousemove))) end;
    pause
  end




(*
   val mouse :
     < create : element Js.t * window Js.t ->
       < react : machine_state >
     >
   *)

let onload _ =
  let s = createSpan document in
  Dom.appendChild document##.body s;
  mouse#create (s, window);
  Js._false
;;

window##.onload := handler onload;;

