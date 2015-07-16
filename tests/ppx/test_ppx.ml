
open OUnit


open Pendulum.Runtime_ast



let test_loop_pause ctx =
  let%sync_ast ast = loop begin pause end
  in assert_equal ast (loop [pause])


let test_seq ctx =
  let%sync_ast ast = pause; pause; emit a () in
  assert_equal (Seq(Pause, Seq(Pause, Emit "a"))) ast


let test_par ctx = assert_equal
    (let%sync_ast ast = emit a () || emit b () || emit c () in ast)
    (Par (Emit "a", Par (Emit "b", Emit "c")))


let test_loop_par_emit_await ctx = assert_equal
    (Signal ("a", Loop (!! [Pause; emit "a"; Signal ("a", emit "a")])
                  // Loop (!! [await "a";])))
    (let%sync_ast ast = signal a () begin loop begin pause;
               emit a (); signal a () (emit a ()) end ||
                          loop (await a) end in ast)


let test_halt_exit_trap ctx = assert_equal
  (let%sync_ast ast = trap a begin halt || exit a end in ast)
  (Trap (Label "a", Par (Halt, Exit (Label "a"))))


let test_presentthen ctx = assert_equal
  (let%sync_ast ast = trap a (loop (present out (exit a); pause)) in ast)
  (trap "a" (loop [Present_then ("out", exit_l "a"); pause]))

let test_abro ctx = assert_equal
    (let%sync_ast ast = loopeach (await a || await b; emit o ();) r in ast)
    (loop_each "r" @@ !![await "a" // await "b"; emit "o"])


let test_every ctx = assert_equal
    (let%sync_ast ast = every (await a || await b; emit o ();) r in ast)
    (Every ("r", !![await "a" // await "b"; emit "o"]))


let cyclic_grc ctx = assert_equal
  (let%sync_ast ast =
    input i, s;
    (nothing
     ||
     present i begin
       present s pause pause
     end
    );
    emit s () in ast)
  (Seq(Par (Nothing, Present_then ("i", Present ("s", Pause, Pause ))), Emit "s"))

let par_deps ctx = assert_equal
    (let%sync_ast ast =
      input s1, s2;
      present s1 (emit s2 ())
      ||
      present s2 (atom begin
          print_string "42";
        end);
     in ast) (Par (Present_then ("S1", emit "S2"), Present_then ("S2", Atom)))

let par_deps ctx = assert_equal
    (let%sync_ast ast =
      input s1, s2;
      present s1 (emit s2 ())
      ||
      present s2 begin
        atom (print_string "42");
        pause
      end;
     in ast) (Par (Present_then ("S1", emit "S2"), Present_then ("S2", Atom)))


let par_deps ctx = assert_equal
    (let%sync_ast ast =
      input s1, s2;
      present s1 (emit s2 ())
      ||
      present s2 begin
        signal s1 () (
          emit s1 ();
          atom (print_string "42");
          pause
        )
      end;
     in ast)
    (Par (Present_then ("S1", emit "S2"), Present_then ("S2", Atom)))

let%sync_ast evenodd =
  input a;
  output b;
  loop begin
    present b (atom (Printf.printf "%s %d\n" !!b !!a));
    pause
  end
  || loop begin present a (emit b (if !!a mod 2 = 0 then "even" else "odd"));
    pause
  end
  || loop begin
    present a (emit b (if !!a mod 2 = 0 then "even" else "odd"));
    pause
  end

let%to_dot_grc loop_pause_atom = (* Bad grc generation : loop that doesn't loop *)
  input click;
  input move;
  loop begin
    pause;
    atom (Format.printf "step@\n");
  end

let%to_dot_grc m = (* Grc.Error  (_, _) *)
  input btn_up;
  input move;
  input ex;

  trap t (
    loop (
      present ex (exit t);
      pause))
  ; atom (ignore 2)


let par_deps ctx = assert_equal
    (let%sync_ast ast =
      input s1, s2;
      present s1 (emit s2 ())
      ||
      present s2 begin
        signal s1 () (
          emit s1 ();
          atom (print_string "42");
          pause
        )
      end;
     in ast) (Par (Present_then ("S1", emit "S2"), Present_then ("S2", Atom)))

let suite =
  "Test_ppx_pendulum_syntax">::: [
    "every">:: test_every;
    "abro">:: test_abro;
    "halt_exit_trap">:: test_halt_exit_trap;
    "loop_par_emit_await">:: test_loop_par_emit_await;
    "loop_pause">:: test_loop_pause;
    "par">:: test_par;
    "seq">:: test_seq;
    "cyclic">:: cyclic_grc;
  ] |> run_test_tt_main





(* let abro a b r o = *)
(*   loop_each r @@ *)
(*     await a *)
(*     // !![await b; emit o] *)


(* let trap_par_loop = *)
(*   Ast.normalize @@ *)

(*   trap "T" ( *)
(*     loop [ *)
(*       Present_then ("out", exit_l "T"); *)
(*       pause; *)
(*     ] // *)
(*     loop [ *)
(*       Present_then ("hop", emit "out"); *)
(*       pause; *)
(*     ] *)
(*   ) *)


(*
   trap T in
     loop
       present OUT then exit T;
       pause
     end
     loop
       present HOP then emit OUT;
       pause
     end
   end

*)
