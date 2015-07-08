


let%sync m =
  input a;
  loop begin
    present a begin
      atom (Printf.printf "even\n")
    end begin
      atom (Printf.printf "odd\n")
    end;
    pause
  end

let () =
  let open Pendulum.Machine in
  let step = m.instantiate () in
  for i = 1 to 10 do
    ignore (step (i mod 2 = 0));
  done
