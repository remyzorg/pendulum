
let%sync m =
  input a 0;
  output b false;

  loop begin
    present b (atom (Printf.printf "%s %d\n" !!b !!a));
    pause
  end
  ||
  loop begin
    present b (atom (Printf.printf "%s %d\n" !!b !!a));
    pause
  end
  ||
  loop begin
    present a (emit b (if !!a mod 2 = 0 then "even" else "odd"));
    pause
  end

let () =
  let open Pendulum.Machine in
  let a = make_signal 0 in
  let b = make_signal "" in
  let step = m.instantiate (a, b) in
  for i = 1 to 10 do
    set_present_value a i;
    ignore (step ());
  done
