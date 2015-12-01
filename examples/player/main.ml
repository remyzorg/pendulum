
open Firebug

module Dom_html = struct
  include Dom_html
  open Js
  class type _audioElement = object
    inherit mediaElement
    method ontimeupdate : (_audioElement t, mouseEvent t) event_listener writeonly_prop
  end
  module Coerce = struct
    include CoerceTo
    let unsafeCoerce tag (e : #element t) = Js.some (Js.Unsafe.coerce e)
    let audio :  #element t -> _audioElement t opt = fun e -> unsafeCoerce "audio" e
  end
end

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let (@>) s coerce =
  Js.Opt.get (coerce @@ Dom_html.getElementById s)
    (fun () -> error "can't find element %s" s)








let update_slider slider media =
  slider##.value := Js.string @@ Format.sprintf "%0.f"
      (if media##.duration = 0. then 0. else media##.currentTime /. media##.duration *. 100.)

let update_media media slider =
  media##.currentTime := (Js.parseFloat slider##.value) /. 100. *. media##.duration

let update_state state media button =
  if state then begin
    button##.textContent := Js.some @@ Js.string "Pause";
    media##play
  end else begin
    button##.textContent := Js.some @@ Js.string "Play";
    media##pause
  end

let%sync reactive_player =
  input play_pause;
  input progress_bar;
  input media;

  let no_update = () in
  let state = false in
  loop begin
    present play_pause##onclick
      (emit state (not !!state));
    pause
  end
  ||
  loop begin
    present state (
      atom (update_state (!!state) media play_pause)
    );
    pause
  end
  ||
  loop begin
    await progress_bar##onmousedown;
    trap t' (loop (
        emit no_update ();
        present progress_bar##onmouseup (
          atom (update_media media progress_bar);
          exit t');
        pause)
      ); pause
  end
  ||
  loop begin
    present no_update nothing
      (present media##ontimeupdate (atom(
           update_slider progress_bar media
         ))); pause
  end


let wrapper react f p = Dom_html.handler (fun _ -> f p;  react (); Js._true)

let main _ =
  let open Dom_html in
  let play_button = "play" @> CoerceTo.button in
  let progress_bar = "progress" @> CoerceTo.input in
  let media = "media" @> Coerce.audio in
  let _react = reactive_player (play_button, progress_bar, media) in
  Js._false


let () = Dom_html.(window##.onload := handler main)
