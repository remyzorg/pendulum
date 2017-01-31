
open Pendulum.Runtime_ast

let dummyatom () = Format.printf "Hello\n"

let%sync p1 =
  input s;
  let s' = !!s + 1 in
  let s'' = !!s' + 1 in
  (loop (pause))

let%sync p2 a b =
  loop begin
    present a##onclick (
      emit a##.textContent Js.(some @@ string "lol")
    ) ; pause
  end
  || loop (emit b; pause)

let%sync incr =
  input s;
  input s2;
  emit s (!!s + 1 + !!s2);
  atom (Format.printf "%d" !!s)

let%sync crazy s x =
  loop (
    !(Format.printf "%d" !!x)
    ; pause
  )

(* let%sync ptest s = *)
(*   run crazy (s, !5) *)

(* let%sync m_loop_incr = *)
(*   input zz; *)
(*   let s1 = 5 in *)
(*   let s2 = 5 in *)
(*   loop ( *)
(*     run incr (s2, s1); *)
(*     pause *)
(*   ) *)
(*   || *)
(*   loop ( *)
(*     run incr (s1, s2); *)
(*     pause *)
(*   ) *)


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

let%sync parall_present_pause =
  let s = () in
  (present s (pause) nothing;
   !(print_string "42-1"))
  ||
  (present s (pause) nothing;
   !(print_string "42-2"))

let%sync emit_basic0 elt0 elt1 elt2 =
  input (elt3 : int), (elt4 : int), elt5;
  input (elt6 : int), (elt7 : int), elt8;
  loop pause


let%sync reincarnation1 o1 o2 =
  loop begin
    let s = () in
    present s (emit o1) (emit o2);
    pause;
    emit s;
    !(print_string "ok");
  end

(* Check never returns *)
let%sync reincarnation2 o1 o2 =
  loop (
    let s = () in
    trap t (
      (pause;
       emit s;
       exit t)
      ||
      loop (
        present s (emit o1) (emit o2);
        pause)
    )
  )

let%sync loop_no_pause o1 o2 =
  loop begin
    !(print_string "ok");
  end

let%sync loop_no_pause2 o1 o2 =
  loop (
    (!(Format.printf "OK1"); pause)
    ||
    (!(Format.printf "OK2"); pause)
  )

let%sync loop_pause_pause =
  loop (
    pause;
    pause;
  )

let%sync gatherer_0 =
  input s (fun acc s -> s + acc);
  input s2 (fun acc s -> s :: acc) ;
  loop begin
    emit s 1;
    emit s2 1;
    pause
  end
  ||
  loop begin
    present s !(Format.printf "s %d" !!s);
    pause
  end


let () =
  let p = gatherer_0#create (0, []) in
  p#s 1;
  p#s 1;
  p#s2 1;
  ignore @@ p#react


let%sync trap_remove_interleaving =
  input a;
  input b;

  loop begin
    present b
      !(Format.printf "b");
    pause
  end
  ||
  loop begin
    trap reset (
      loop (present a (exit reset) ; pause)
    );
    pause
  end
  ||
  loop begin
    present b !(Format.printf "b"); pause
  end


let%sync elements =
  element e1, e2;
  element e3;
  loop pause

let%sync example_edwards_paper =
  input a;
  output b, c, d, e;
  trap t begin
    present a begin
      emit b;
      present c (emit d);
      present e (exit t);
    end;
    pause;
    emit b
  end
  || present b (emit c)
  || present d (emit e)

let%sync test_await ~new_feature ~print_only ~print:pdf s =
  loop begin
    await s;
    !(print_endline "hello");
    pause;
  end


let%sync test_await ~new_feature ~print:(pdf, dot) s1 s2 s3 =
  begin
    present s1 !(print_endline "hello1");
    emit s2;
    present s3 !(print_endline "hello3")
  end
  ||
  loop begin
    present s2 !(print_endline "hello2");
    emit s3;
  end









