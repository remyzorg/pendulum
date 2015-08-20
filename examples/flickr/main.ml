
open Firebug

open Lwt

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
  let open Flickr.Method in
  Lwt_js_events.(async (fun () ->
      let area = "tarea" @> CoerceTo.a in
      let photos_area = "photos_area" @> CoerceTo.div in
      let username_input = "username_input" @> CoerceTo.input in
      let set_gallery = (fun _ ev ->
          let%lwt _ = Lwt_js.sleep 0.5 in
          let username = Js.to_string @@ username_input##.value in
          if username <> "" then
            let%lwt id = People.(findByUsername username >|= extract_user_id) in
            area##.textContent := Js.some @@ Js.string id;

            let%lwt photo_ids = People.(getPhotos id >|= extract_photos) in

            Js.Opt.iter (photos_area##.firstChild) (Dom.removeChild photos_area);
            let gallery_div = createDiv document in
            Dom.appendChild photos_area gallery_div;

            let%lwt () = Lwt_list.iter_p (fun phid ->
                let%lwt sizes = Photos.(getSizes phid >|= extract_url Square) in
                let photo_img = createImg document in
                (match sizes with [] -> () | e::_ -> photo_img##.src := Js.string e);
                Lwt.return @@ Dom.appendChild gallery_div photo_img
              ) photo_ids
            in
            Lwt.return () else Lwt.return ()
        )
      in
      Lwt.return (changes username_input set_gallery,
                  inputs username_input set_gallery,
                  keyups username_input set_gallery)
    ))













