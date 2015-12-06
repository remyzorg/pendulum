
open Firebug

module Dom_html = struct
  include Dom_html
  open Js
  class type _mediaElement = object
    inherit mediaElement
    method onprogress : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
    method ontimeupdate : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
    method onplay : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
    method onloadeddata : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
  end
  module Coerce = struct
    include CoerceTo
    let unsafeCoerce tag (e : #element t) = Js.some (Js.Unsafe.coerce e)
    let media :  #element t -> _mediaElement t opt = fun e -> unsafeCoerce "media" e
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






let str s = Js.some @@ Js.string s

let update_slider slider media =
  slider##.value := Js.string @@ Format.sprintf "%0.f"
      (if media##.duration = 0. then 0. else media##.currentTime /. media##.duration *. 800.)

let update_media media slider =
  media##.currentTime := (Js.parseFloat slider##.value) /. 800. *. media##.duration

let update_state state media button =
  if state then begin
    button##.textContent := Js.some @@ Js.string "Pause";
    media##play
  end else begin
    button##.textContent := Js.some @@ Js.string "Play ";
    media##pause
  end

let ftime_to_min_sec t =
  let sec = int_of_float t in
  let min = sec / 60 in
  let sec = sec mod 60 in
  (min, sec)

let update_time_a media time_a =
  let cmin, csec = ftime_to_min_sec media##.currentTime in
  let tmin, tsec = ftime_to_min_sec media##.duration in
  time_a##.textContent := str @@ Format.sprintf "%2d:%0d / %0d:%0d" cmin csec tmin tsec


let%sync reactive_player =
  input play_pause;
  input progress_bar;
  input media;
  input time_a;

  let no_update = () in
  let state = Js.to_bool media##.paused in
  loop (
    present media##onplay
      !(play_pause##.textContent := Js.some @@ Js.string "Pause"); pause)
  || loop (present play_pause##onclick (emit state (not !!state)); pause)
  || loop (present state !(update_state (!!state) media play_pause); pause)
  || loop (
    await progress_bar##onmousedown;
    trap t' (loop (
        emit no_update ();
        present progress_bar##onmouseup
          (!(update_media media progress_bar); exit t');
        pause)
      ); pause)
  || loop (
    present media##onprogress (
      present no_update nothing !(update_slider progress_bar media)
      ||
      !(update_time_a media (!!time_a))
    ); pause)
  || loop pause
  || loop pause


let wrapper react f p = Dom_html.handler (fun _ -> f p;  react (); Js._true)

let main _ =
  let open Dom_html in
  let play_button = "play" @> Coerce.button in
  let progress_bar = "progress" @> Coerce.input in
  let media = "media" @> Coerce.media in
  let time = "timetxt" @> Coerce.a in
  let _set_time, _react = reactive_player (play_button, progress_bar, media, time) in
  Js._false


let () = Dom_html.(window##.onload := handler main)
