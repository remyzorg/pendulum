
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





let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->
      let area = "tarea" @> CoerceTo.a in
      (* let c = "canvas" @> CoerceTo.canvas in *)
      (* let ctx = canvas##getContext (Dom_html._2d_) in *)

      let photos = Flickr.Method.People.getPhotos in

      Js._false
    )

