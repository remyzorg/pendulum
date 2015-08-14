
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





let e =
  let open Dom_html in
  let open XmlHttpRequest in
  Lwt_js_events.(async (fun () -> 

      let _ = onload () in
      let area = "tarea" @> CoerceTo.a in
      (* let c = "canvas" @> CoerceTo.canvas in *)
      (* let ctx = canvas##getContext (Dom_html._2d_) in *)

      let username = "Nevor" in
      let user_id = "48404998@N08" in

      let%lwt user_id = Flickr.Method.People.(
          let%lwt frame = findByUsername username in
          debug "%s" frame.content;
          Lwt.return @@ extract_user_id frame.content
        ) in

      (* let%lwt photos = Flickr.Method.People.( *)
      (*     let%lwt frame = getPhotos user_id in *)
      (*     debug "%s" frame.content; *)
      (*     Lwt.return @@ extract_photos frame.content *)
      (*   ) in *)

      List.iter (debug "%s") user_id;

      Lwt.return Js._false

    ))

