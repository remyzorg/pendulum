

let%sync m =
  input btn_up;
  input move;
  input ex;

  atom (Graphics.moveto (fst !!move) (snd !!move));

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


let () =
  let open Pendulum in
  let open Machine in
  let open Graphics in
  Graphics.open_graph " 300x300";

  let (set_btn_up,
       set_move,
       set_ex),
      step_m = m ((), Graphics.mouse_pos (), ())
  in
  while true do
    let status =
      Graphics.(wait_next_event [Mouse_motion; Button_down; Key_pressed])
    in
    set_move (status.mouse_x, status.mouse_y);
    if status.button then set_btn_up ();
    if status.keypressed && status.key ='q' then set_ex ();
    ignore(step_m ())
  done
