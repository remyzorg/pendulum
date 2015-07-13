
let%sync m =
  input a 0;
  output b "";

  loop begin
    present b (atom (
        Printf.printf "%s %d\n" !!b !!a
      ));
    pause
  end
  ||
  loop begin
    present a (emit b (if !!a mod 2 = 0 then "even" else "odd"));
    pause
  end

let%sync m2 =
  input a ();
  loop begin
    present a begin
      atom (print_string "hello\n");
      pause;
      atom (print_string "world\n");
      pause;
    end begin
      atom (print_string "bonjour\n");
      pause;
      atom (print_string "le monde\n");
      pause;
    end
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

let () = Format.printf "@."

let () =
  let open Pendulum.Machine in
  let a = make_signal () in
  let step = m2.instantiate a in
  for i = 0 to 9 do
    if i mod 3 = 0 then set_present a;
    ignore (step ());
  done
