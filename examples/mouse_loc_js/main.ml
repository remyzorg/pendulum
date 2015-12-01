
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

let%sync mouse_machine =
  input tarea;
  input move;

  loop begin
    present move (
      atom (
        (!!tarea)##.textContent := Js.some (Js.string @@
          Format.sprintf "%dx%d" (fst !!move) (snd !!move))
      ));
    pause
  end

let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->
      let area = "tarea" @> CoerceTo.a in
      let set_tarea, set_move, step =
        mouse_machine (area, (0,0))
      in

      window##.onmousemove := handler (fun e ->
          set_move (e##.clientX, e##.clientY);
          ignore @@ step ();
          Js._false);

      Js._false
    )

