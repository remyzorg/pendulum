
open Firebug



(* Jsoo boilerplate code *)

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

(* ======================== *)



let ftime_to_min_sec t =
  let sec = int_of_float t in
  let min = sec / 60 in
  let sec = sec mod 60 in
  (min, sec)

let max_slide = 1000.

let set_visible b elt =
  let visibility = if b then "visible" else "hidden"
  in elt##.style##.visibility := Js.string visibility

(* Updates the progress bar value proportionnaly to current time *)
let update_slider slider media =
  slider##.value := Js.string @@ Format.sprintf "%0.f" (
      if media##.duration = 0. then 0.
      else media##.currentTime /. media##.duration *. max_slide)


(* Updates the time of the current position of the cursor over it *)
let update_slider_value slider_value media slider =
  let min, sec = ftime_to_min_sec @@
    ((Js.parseFloat slider##.value) /. max_slide *. media##.duration)
  in
  set_visible true slider_value;
  let padding =
    Format.sprintf "%0.fpx" (Js.parseFloat slider##.value /. max_slide *.
                             (float_of_int slider##.scrollWidth))
  in
  slider_value##.style##.marginLeft := Js.string padding;
  slider_value##.textContent := Js.some @@ Js.string @@ Format.sprintf "%2dmin%2ds" min sec


(* Sets the current time of the media tag in proportion *)
let update_media media slider =
  media##.currentTime := (Js.parseFloat slider##.value) /. max_slide *. media##.duration


(* Apply the state switching to the video by calling pause/play action *)
let update_state state media button =
  if state then media##play else media##pause


(* Update the displayed current time *)
let update_time_a media time_a =
  let cmin, csec = ftime_to_min_sec media##.currentTime in
  let tmin, tsec = ftime_to_min_sec media##.duration in
  time_a##.textContent := str @@ Format.sprintf "%2d:%0d / %0d:%0d" cmin csec tmin tsec

(* Switch the text on the button *)
let update_content elt b =
  elt##.textContent := Js.some @@ Js.string @@ if b then "Pause" else "Play"


open Dom_html

(* The reactive program of the player *)
let%sync reactive_player ~animate
    (play_pause : buttonElement Js.t)
    (progress_bar : inputElement Js.t)
    (media : _mediaElement Js.t)
    (time_a : anchorElement Js.t)
    (slider_value : anchorElement Js.t)(* the a elt displaying time*)
  =

  let no_update = () in
  let state = Js.to_bool media##.autoplay in (* carry the state of the video (true if playing) *)

  !(update_content play_pause (pre state)); (* update the button at start-up *)

  loop ( (* handle the different possibles actions  : *)
       present media##onplay !(update_content play_pause !!state)
    || present media##onpause !(update_content play_pause !!state)
    || present play_pause##onclick (emit state (not !!state))
    || present state !(update_state !!state media play_pause)
  ; pause)

  || loop ( (* while the user moves the cursor, display the time over it *)
    present progress_bar##oninput
      !(update_slider_value !!slider_value media progress_bar);
    pause
  )

  || loop (
    await progress_bar##onmousedown;              (* When mouse button is down *)
    trap t' (                                       (* open an escape block with exception t' *)
      loop (                                          (* every instant,  *)
        emit no_update;                                 (* emits the blocking signal *)
        present progress_bar##onmouseup (               (* if the button is released*)
          !(set_visible false (pre slider_value);         (* stop displaying the time over the cursor *)
            update_media media progress_bar);             (* set the current time of the video *)
          exit t'                                         (* leave the escape block and stop this behavior *)
          ); pause)); pause)

  || loop (
    present media##ontimeupdate (                 (* everying instants the video updates *)
      present no_update nothing                     (* if the blocking signal is present, do nothing *)
        !(update_slider progress_bar media)         (* else, update the progress bar with the current time of the video*)
      ||
      !(update_time_a media (pre time_a))           (* update the display of the current time *)
    ); pause)


let wrapper react f p = Dom_html.handler (fun _ -> f p;  react (); Js._true)

let main _ =
  let open Dom_html in
  let play_button = "reactiveplayer_play" @> Coerce.button in
  let progress_bar = "reactiveplayer_progress" @> Coerce.input in
  let media = "reactiveplayer_media" @> Coerce.media in
  let time = "reactiveplayer_timetxt" @> Coerce.a in
  let range_value = "reactiveplayer_range_value" @> Coerce.a in

  (* Initialize the player program, with the js elements as inputs
     setters function have '_' to avoid the unused warning, as long
     as we don't use them :
     Signals and _react function are implicitely triggered by the generated code
     of the program
  *)
  let _set_time, _set_range_value, _react =
    reactive_player (play_button, progress_bar, media, time, range_value)
  in
  Js._false


let () = Dom_html.(window##.onload := handler main)
