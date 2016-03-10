
open Pendulum.Runtime_ast

let dummyatom () = Format.printf "Hello\n"

let%sync basic =
  input elt;
  input elt2;

  loop begin
    present elt2##onmouseover
      (atom (Format.printf "mouseover\n"))
  end
  ||
  loop begin
    present elt##onclick
      (atom (Format.printf "click\n"))
  end


let%sync emit_basic elt elt2 =
  input (elt3 : int);
  loop begin
    present elt##onmouseover
      (emit elt2##.textContent (Js.string "lol"));
    pause
  end
  ||
  loop begin
    present elt##onmouseover
      (emit elt2##.textContent (Js.string "lol"));
    pause
  end

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

let%sync test_animate ~animate ~print:(png, pdf, dot) =
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


let%sync mouse_react  =
  input span {
    onclick = 0, (fun acc ev -> acc + 1);
  };
  input w {
    onmousemove = "", fun x ev ->
        Format.sprintf "%d,%d" ev##.clientX ev##.clientY;
  };

  loop begin
    present w##onmousemove (
      emit span##.textContent
        Js.(some (string !!(w##onmousemove)))
    ); pause
  end


let%sync mouse_react =
  input span {
    onclick = 0, (fun acc ev -> acc + 1);
  };

  loop begin
    pause
  end

