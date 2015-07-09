
let%sync m =
  input a 0;
  output b false;

  loop begin
    present b
      (atom (Printf.printf "even %B\n" !!b))
      (atom (Printf.printf "odd %B\n" !!b));
    pause
  end
  ||
  loop begin
    present a (emit b (!!a mod 2 = 0));
    pause
  end

let () =
  let open Pendulum.Machine in
  let a = make_signal 0 in
  let b = make_signal false in
  let step = m.instantiate (a, b) in
  for i = 1 to 10 do
    setval a i;
    ignore (step ());
  done
