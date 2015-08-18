
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
  let open Flickr.Method in
  Lwt_js_events.(async (fun () ->

      let _ = onload () in
      let area = "tarea" @> CoerceTo.a in
      let photos_area = "photos_area" @> CoerceTo.div in
      let username_input = "username_input" @> CoerceTo.input in

      let _ = async (fun () ->
          let set_gallery = (fun _ ev ->
              People.(
                let username = Js.to_string @@ username_input##.value in
                if username <> "" then
                  let%lwt id_frame = findByUsername username in
                  let id = extract_user_id id_frame.content in
                  area##.textContent := Js.some @@ Js.string id;

                  let%lwt photos_frame = getPhotos id in
                  let photo_ids = extract_photos photos_frame.content in

                  Js.Opt.iter (photos_area##.firstChild) (Dom.removeChild photos_area);
                  let gallery_div = createDiv document in
                  Dom.appendChild photos_area gallery_div;

                  let%lwt urls = Lwt_list.iter_p (fun phid ->
                      let%lwt frm = Photos.getSizes phid in
                      let thumb = match Photos.extract_url Photos.Square frm.content with
                        | [] -> Js.null | l -> Js.some (List.hd l)
                      in
                      let photo_img = createImg document in
                      Js.Opt.iter thumb (fun x -> photo_img##.src := Js.string x);
                      Lwt.return @@ Dom.appendChild gallery_div photo_img
                    ) photo_ids
                  in

                  Lwt.return ()
                else Lwt.return ()
              ))
          in
          Lwt.pick (changes username_input set_gallery,
                      keyups username_input set_gallery)
        )
      in
      Lwt.return Js._false
    ))

