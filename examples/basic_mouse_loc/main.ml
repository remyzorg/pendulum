


let%sync m =
  input a;
  output b;
  loop begin
    present a begin
      atom (Printf.printf "even %B\n" !b)
    end begin
      atom (Printf.printf "odd %B\n" !b)
    end;
    pause
  end
  ||
  loop begin
    emit b;
    pause
  end

let () =
  let open Pendulum.Machine in
  let step = m.instantiate () in
  for i = 1 to 10 do
    ignore (step ((i mod 2 = 0), false));
  done
