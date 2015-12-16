
open Firebug

module Dom_html = struct
  include Dom_html
  open Js
  class type _mediaElement = object
    inherit mediaElement
    method onprogress : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
    method ontimeupdate : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
    method onplay : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
    method onpause : (_mediaElement t, mouseEvent t) event_listener writeonly_prop
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

let ftime_to_min_sec t =
  let sec = int_of_float t in
  let min = sec / 60 in
  let sec = sec mod 60 in
  (min, sec)

let max_slide = 1000.

let set_visible b elt =
  let visibility = if b then "visible" else "hidden"
  in elt##.style##.visibility := Js.string visibility

let update_slider slider media =
  slider##.value := Js.string @@ Format.sprintf "%0.f" (
      if media##.duration = 0. then 0.
      else media##.currentTime /. media##.duration *. max_slide)

let update_slider_value slider_value media slider =
  let min, sec = ftime_to_min_sec @@
    ((Js.parseFloat slider##.value) /. max_slide *. media##.duration)
  in
  set_visible true slider_value;
  let padding =
    Format.sprintf "%0.fpx" (Js.parseFloat slider##.value /. max_slide *. (float_of_int slider##.scrollWidth))
  in
  slider_value##.style##.marginLeft := Js.string padding;
  slider_value##.textContent := Js.some @@ Js.string @@ Format.sprintf "%2d:%2d" min sec

let update_media media slider =
  media##.currentTime := (Js.parseFloat slider##.value) /. max_slide *. media##.duration

let update_state state media button =
  if state then media##play else media##pause


let update_time_a media time_a =
  let cmin, csec = ftime_to_min_sec media##.currentTime in
  let tmin, tsec = ftime_to_min_sec media##.duration in
  time_a##.textContent := str @@ Format.sprintf "%2d:%0d / %0d:%0d" cmin csec tmin tsec

let update_content elt b =
  elt##.textContent := Js.some @@ Js.string @@ if b then "Pause" else "Play"


let%sync reactive_player ~animate =
  input play_pause; (* the button *)
  input progress_bar; (* the progress element *)
  input media; (* the video element *)
  input time_a; (* the a elt displaying time*)
  input slider_value; (* the a elt displaying time*)

  let no_update = () in
  let state = Js.to_bool media##.autoplay in
  !(update_content play_pause (pre state));

  loop (
    present media##onplay !(
      update_content play_pause (pre state))

    || present media##onpause !(
      update_content play_pause (pre state))

    || present play_pause##onclick (
      emit state (not (pre state)))

    || present state !(
      update_state (pre state) media play_pause)
  ; pause)

  || loop (
    present progress_bar##oninput
      !(update_slider_value (pre slider_value) media progress_bar);
    pause
  )
  || loop (
    await progress_bar##onmousedown;
    trap t' (
      loop (
        emit no_update ();
        present progress_bar##onmouseup (
          !(set_visible false (pre slider_value);
           update_media media progress_bar);
          exit t'
          ); pause)); pause)

  || loop (
    present media##ontimeupdate (
      present no_update nothing !(update_slider progress_bar media)
      ||
      !(update_time_a media (pre time_a))
    ); pause)


let wrapper react f p = Dom_html.handler (fun _ -> f p;  react (); Js._true)

let main _ =
  let open Dom_html in
  let play_button = "reactiveplayer_play" @> Coerce.button in
  let progress_bar = "reactiveplayer_progress" @> Coerce.input in
  let media = "reactiveplayer_media" @> Coerce.media in
  let time = "reactiveplayer_timetxt" @> Coerce.a in
  let range_value = "reactiveplayer_range_value" @> Coerce.a in
  let _set_time, _set_range_value, _react =
    reactive_player (play_button, progress_bar, media, time, range_value)
  in
  Js._false


let () = Dom_html.(window##.onload := handler main)
