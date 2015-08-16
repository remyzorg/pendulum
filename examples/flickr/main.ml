
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


let jsopt_string s = Dom_html.(Js.some @@ Js.string s)


let e =
  let open Dom_html in
  let open XmlHttpRequest in
  Lwt_js_events.(async (fun () ->

      let _ = onload () in
      let area = "tarea" @> CoerceTo.a in
      let photos_area = "photos_area" @> CoerceTo.a in
      let username_input = "username_input" @> CoerceTo.input in

      let _ = async (fun () ->
          inputs username_input (fun _ ev ->
              Flickr.Method.People.(
                let username = Js.to_string @@ username_input##.value in
                if username <> "" then
                  let%lwt id_frame = findByUsername username in
                  let id = extract_user_id id_frame.content in
                  area##.textContent := Js.some @@ Js.string id;

                  let%lwt photos_frame = getPhotos id in
                  let photo_ids = extract_photos photos_frame.content in
                  photos_area##.textContent := jsopt_string @@ String.concat "\n" photo_ids;



                  Lwt.return ()
                else Lwt.return ()
              ))) in

      Lwt.return Js._false

    ))

