
open Firebug

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let (@>) s coerce =
  Js.Opt.get (coerce @@ Dom_html.getElementById s)
    (fun () -> error "can't find element %s" s)

let iter opt f =
  match opt with
  | None -> ()
  | Some o -> f o



let%sync mouse_react =
  input span;
  input w {
    onmousemove = "", (fun x ev ->
        Format.sprintf "%d,%d" ev##.clientX ev##.clientY);
  };

  loop begin
    present w##onmousemove (
      emit span##.textContent
        (Js.(some (string !!(w##onmousemove))))
    ); pause
  end



let _ =
  let open Dom_html in

  window##.onkeydown := handler (fun _ ->
      debug "lol";
      Js._false
    );

  window##.onload := handler (fun _ ->
      let area = "tarea" @> CoerceTo.a in
      let _step = mouse_react (area, window) in
      Js._false
    )

