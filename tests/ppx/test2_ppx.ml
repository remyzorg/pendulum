
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

let lol =
  let open Pendulum.Runtime_misc in
  let open Pendulum.Program in
  let open Pendulum.Signal in
  let create_local (o1,o2) () =
    let pendulum_state = Bitset.make 16  in
    let s = ref (make_signal ())  in
    let set_absent () = set_absent o1; set_absent o2; set_absent (!s); ()
    in
    let p_react () =
      try
        if Bitset.mem pendulum_state 0
        then raise Finish_exc
        else
          (if Bitset.mem pendulum_state 15
           then
             (if Bitset.mem pendulum_state 5
              then
                (if Bitset.mem pendulum_state 1
                 then
                   ((Bitset.inter_union pendulum_state [|2147483645|] [|16|];
                     set_present_value (!s) ());
                    if Bitset.mem pendulum_state 11
                    then
                      (Bitset.inter_union pendulum_state [|2147483135|]
                         [|1280|];
                       if !? (!s)
                       then set_present_value o1 ()
                       else set_present_value o2 ();
                       Bitset.inter_union pendulum_state [|2147483391|]
                         [|512|]);
                    Bitset.inter pendulum_state [|2147459071|];
                    raise Pause_exc
                   )
                 else if (Bitset.mem pendulum_state 5) ||
                         (Bitset.mem pendulum_state 11) then
                   raise Pause_exc
                 else
                   (Bitset.inter pendulum_state [|2147479551|];
                    if Bitset.mem pendulum_state 11
                    then
                      (Bitset.inter_union pendulum_state [|2147483135|]
                         [|1280|];
                       if !? (!s)
                       then set_present_value o1 ()
                       else set_present_value o2 ();
                       Bitset.inter_union pendulum_state [|2147483391|]
                         [|512|]);
                    Bitset.inter pendulum_state [|2147459071|];
                    raise Pause_exc)
                    ; if (Bitset.mem pendulum_state 5) ||
                         (Bitset.mem pendulum_state 11)
                      then raise Pause_exc
                      else
                        (Bitset.inter pendulum_state [|2147454975|];
                         raise Pause_exc)
                )
              else
              if
                (Bitset.mem pendulum_state 5) ||
                (Bitset.mem pendulum_state 11)
              then raise Pause_exc
              else
                (Bitset.inter pendulum_state [|2147479551|];
                 if Bitset.mem pendulum_state 11
                 then
                   (Bitset.inter_union pendulum_state [|2147483135|]
                      [|1280|];
                    if !? (!s)
                    then set_present_value o1 ()
                    else set_present_value o2 ();
                    Bitset.inter_union pendulum_state [|2147483391|]
                      [|512|]);
                 Bitset.inter pendulum_state [|2147459071|];
                 raise Pause_exc))
           else
             ((Bitset.union pendulum_state [|49152|];
               s := (make_signal ());
               Bitset.union pendulum_state [|15650|]);
              if !? (!s)
              then set_present_value o1 ()
              else set_present_value o2 ();
              Bitset.inter_union pendulum_state [|2147483391|] [|512|]);
           if
             (Bitset.mem pendulum_state 5) ||
             (Bitset.mem pendulum_state 11)
           then raise Pause_exc
           else
             (Bitset.inter pendulum_state [|2147454975|]; raise Pause_exc))
      with | Pause_exc  -> (set_absent (); Pause)
           | Finish_exc  ->
             (set_absent (); Bitset.add pendulum_state 0; Finish)
    in
    object
      method o1 = set_present_value o1
      method o2 = set_present_value o2
      method react = p_react ()
    end  in
  object
    method create (o1,o2) =
      create_local ((make_signal o1), (make_signal o2)) ()
    method create_run ins = create_local ins ()
  end

(* TODO : Check never returns *)
let%sync reincarnation2 o1 o2 ~dsource =
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









