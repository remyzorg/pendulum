
open Pendulum.Runtime_ast

let dummyatom () = Format.printf "Hello\n"

let%sync mouse_machine1 =
  input s;
  let s' = !!s + 1 in
  let s'' = !!s' + 1 in
  (loop (pause))


let%sync incr =
  input s;
  input s2;
  emit s (!!s + 1 + !!s2);
  atom (Format.printf "%d" !!s)

let%sync m_loop_incr =
  input zz;
  let s1 = 5 in
  let s2 = 5 in
  loop (
    run incr (s2, s1);
    pause
  )
  ||
  loop (
    run incr (s1, s2);
    pause
  )

let%sync many_par =
  loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause
  || loop pause



let%sync reactive_player =
  input play;
  input pause;
  input start_slide;
  input stop_slide;
  input media_time;
  input media;
  input slider;

  let cant_update = () in
  loop begin
    present play (atom (print_string "play"));
    present pause (atom (print_string "pause"));
    pause
  end
  || loop begin
    await start_slide;
    trap t' (loop (
        emit cant_update ();
        present stop_slide (
          atom (print_string "stop_slide");
          exit t');
        pause)
      );
    pause
  end
  ||
  loop begin present cant_update nothing
      (present media_time (atom(
           print_string "media_time"
         ))); pause
  end



let%sync reactive_player =
  loop begin
    trap t (
      loop (pause); pause
    );
  end


let%sync basic =
  input elt;
  input elt2;

  loop begin
    present elt2##onmouseover
      !(Format.printf "mouseover\n");
    pause
  end
  ||
  loop begin
    present elt##onclick
      !(Format.printf "click\n");
    pause
  end


let%sync bang =
  loop begin
    !(Format.printf "lol");
    pause
  end

let%sync testexpr =
  input iinp;

  loop begin
    present (iinp & !!iinp)
      !(Format.printf "lol");
    pause
  end

let%sync reactive_player =
  input play_pause;
  input progress_bar;
  input media;
  input time_a;

  let no_update = () in
  let state = Js.to_bool media##.paused in
  loop (
    present media##onplay !(dummyatom ()); pause)
  || loop (present play_pause##onclick (emit state (not !!state)); pause)
  || loop (present state !(dummyatom ()); pause)
  || loop (
    await progress_bar##onmousedown;
    trap t' (loop (
        emit no_update ();
        present progress_bar##onmouseup
          (!(dummyatom ()); exit t');
        pause)
      ); pause)
  || loop (
    present media##onprogress (
      present no_update nothing !(dummyatom ())
      ||
      !(dummyatom ())
    ); pause)


let%sync reactive_player =
  input a;
  input b;
  loop (
    present a !(dummyatom ());
    pause
  )
  ||
  loop (
    present b (emit a (not !!a));
    pause
  )
  ||
  loop pause

let%sync test_animate ~animate ~dsource =
  input btn;
  let loca = Dom_html.(createDiv document) in
  let localol = 0 in
  loop begin
    present btn##onclick !(
      let newitem = Dom_html.(createButton document) in
      (newitem##.onclick) :=
        (Dom_html.handler
           (fun ev  ->
              set_present_value localol (!!localol + 1);
              animate ();
              Js._true));
    );
    pause
  end


