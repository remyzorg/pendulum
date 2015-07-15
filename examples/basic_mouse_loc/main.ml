
let%sync m =
  input btn_up;
  input move;
  input ex;

  loop begin
    present btn_up (
      atom (
        Graphics.draw_string (Format.sprintf "%dx%d" (fst !!move) (snd !!move));
      ));
    pause
  end
  ||
  loop begin
    present move (
      atom (
        Graphics.moveto (fst !!move) (snd !!move);
      ));
    pause
  end
  ||
  loop begin
    present ex (atom (Graphics.close_graph ()));
    pause
  end

open Graphics

let () =
  let open Pendulum in
  let open Machine in
  Graphics.open_graph " 300x300";

  let btn_up = Machine.make_signal () in
  let move = Machine.make_signal (0,0) in
  (* let btn_up = Machine.make_signal () in *)
  (* let circle = Machine.make_signal (100, 100) in *)
  let ext = Machine.make_signal () in
  let step = m.instantiate (btn_up, move, ext) in
  while true do
    let status =
      Graphics.(wait_next_event [Mouse_motion; Button_down; Key_pressed])
    in
    Machine.set_present_value move (status.mouse_x, status.mouse_y);
    if status.button then Machine.set_present_value btn_up ();
    if status.keypressed && status.key ='q' then Machine.set_present_value ext ();
    ignore(step ())
  done




