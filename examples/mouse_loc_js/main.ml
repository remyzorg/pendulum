
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

let iter f opt =
  match opt with
  | None -> ()
  | Some o -> f o

let update_field tarea ev =
  tarea##.textContent :=
    Js.some (Js.string @@ Format.sprintf "%dx%d" (ev##.clientX) (ev##.clientY))

let%sync mouse_machine =
  input tarea;
  input window;
  loop begin
    present window##onmousemove !(
      iter (update_field !!tarea)
        !!(window##onmousemove))
    pause
  end

let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->
      let area = "tarea" @> CoerceTo.a in
      let _set_tarea, _step = mouse_machine (area, window) in
      Js._false
    )

