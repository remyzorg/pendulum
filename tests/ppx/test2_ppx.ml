
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

let%sync ptest s =
  run crazy (s, !5)

let%sync m_loop_incr =
  input zz;
  let s1 = 5 in
  let s2 = 5 in
  loop (
    run incr (s2, s1);
    pause
  )
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

let%sync reincarnation1 o1 o2 =
  loop begin
    let s = () in
    present s (emit o1) (emit o2);
    pause;
    emit s;
    !(print_string "ok");
  end

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

let%sync loop_pause_pause ~print:pdf =
  loop (
    pause;
    pause;
  )


let () =
  let p = gatherer_0#create (0, []) in
  p#s 1;
  p#s 1;
  p#s2 1;
  ignore @@ p#react



let p =
  let%sync react_obj a =
    loop begin
      !(Format.printf "%d\n" !!a)
    ; pause
    end in react_obj#create 0

let () =
  p#a 10;
  ignore @@ p#react

open Pendulum
open Program
open Signal
(* 'a -> < react : Pendulum.Program.state; x : 'a -> unit > *)

let%sync p2 = loop pause

let _ : < react : state > = p2#create
let _ : < react : state > = p2#create_run
let%sync p2' s = loop begin
    run p2
  ; pause
  end
let _ : 'a -> < react : state; s : 'a -> unit > = p2'#create
let _ : ('a, 'b) signal -> < react : state; s : 'a -> unit > = p2'#create_run

let%sync p3 ~dsource s = loop begin run p2' !("test" ^ !!s); pause end
let _ : string -> < react : state; s : string -> unit > = p2'#create
let _ : (string, string) signal -> < react : state; s : string -> unit > = p2'#create_run
