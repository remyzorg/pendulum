
open Firebug

open Tyxml_js

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let (@>) s coerce =
  Js.Opt.get (coerce @@ Dom_html.getElementById s)
    (fun () -> error "can't find element %s" s)

let update_slider slider (duration, time) =
  slider##.value := Js.string @@ Format.sprintf "%0.f"
      (if duration = 0. then 0. else
      time /. duration *. 100.)

let update_media media f =
  media##.currentTime := f /. 100. *. media##.duration

let%sync reactive_player =
  input play;
  input pause;
  input start_slide;
  input stop_slide;
  input media_time;
  input media;
  input slider;

  let cant_update = () in
  loop begin present play (atom ((!!media)##play));
    present pause (atom ((!!media)##pause));
    pause
  end
  (* || loop begin *)
  (*   await start_slide; *)
  (*   trap t' (loop ( *)
  (*       emit cant_update (); *)
  (*       present stop_slide ( *)
  (*         atom (update_media !!media !!stop_slide); *)
  (*         exit t'); *)
  (*       pause) *)
  (*     ); *)
  (*   pause *)
  (* end *)
  ||
  loop begin present cant_update nothing
      (present media_time (atom(
           debug "%f" (snd !!media_time);
           update_slider !!slider !!media_time
         ))); pause
  end


let wrapper react f p = Dom_html.handler (fun _ -> f p;  react (); Js._true)

let main _ =
  let open Dom_html in
  let play_button = "play" @> CoerceTo.button in
  let pause_button = "pause" @> CoerceTo.button in
  let progress_bar = "progress" @> CoerceTo.input in
  let media = "media" @> CoerceTo.audio in
  let
    set_play,
    set_pause,
    set_start_slide,
    set_stop_slide,
    set_media_time,
    _, _, react
    = reactive_player ((), (), (), 0., (media##.duration, 0.0), media, progress_bar)
  in

  let wrapper f p = wrapper react f p  in

  play_button##.onclick := wrapper set_play ();

  pause_button##.onclick := wrapper set_pause ();

  media##.ontimeupdate := handler (fun _ ->
      set_media_time (media##.duration, media##.currentTime);
      react (); Js._true);
  progress_bar##.onmousedown := wrapper set_start_slide ();
  progress_bar##.onmouseup := handler (fun _ ->
      set_stop_slide (Js.parseFloat progress_bar##.value); react (); Js._true);

  Js._false


let () = Dom_html.(window##.onload := handler main)
