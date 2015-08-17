
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
      let photos_area = "photos_area" @> CoerceTo.div in
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
                  (* photos_area##.textContent := jsopt_string @@ String.concat "\n" photo_ids; *)


                  Js.Opt.iter (photos_area##.firstChild) (Dom.removeChild photos_area);

                  let gallery_div = createDiv document in
                  Dom.appendChild photos_area gallery_div;

                  ignore @@ List.fold_left (fun (n, div) phid ->
                      let curr_div =
                        if n mod 5 = 0 then
                          let photos_div = createDiv document in
                          Dom.appendChild photos_area photos_div;
                          photos_div
                        else div
                      in
                      let photo_a = createA document in
                      photo_a##.textContent := jsopt_string (phid ^ "   ");
                      Dom.appendChild curr_div photo_a;
                      (n + 1, curr_div)
                  ) (0, createDiv document) photo_ids;
                  Lwt.return ()
                else Lwt.return ()
              ))) in

      Lwt.return Js._false

    ))

