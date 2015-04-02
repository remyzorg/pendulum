
open OUnit


open Pendulum.Runtime_ast
open Grc



let test_loop_pause ctx = assert_equal [%sync_ast loop begin pause end]
    (loop [pause])


let test_seq ctx = assert_equal [%sync_ast pause; pause; emit a]
    (Seq(Pause, Seq(Pause, Emit "a")))


let test_par ctx = assert_equal
    [%sync_ast emit a || emit b || emit c]
    (Par (Emit "a", Par (Emit "b", Emit "c")))


let test_loop_par_emit_await ctx = assert_equal
    (Signal ("a", Loop (!! [Pause; emit "a"; Signal ("a", emit "a")])
                  // Loop (!! [await "a";])))
    [%sync_ast signal a begin loop begin pause;
               emit a; signal a (emit a) end ||
                          loop (await a) end]


let test_halt_exit_trap ctx = assert_equal
  [%sync_ast trap a begin halt || exit a end]
  (Trap (Label "a", Par (Halt, Exit (Label "a"))))


let test_presentthen ctx = assert_equal
  [%sync_ast trap a (loop (present OUT (exit a); pause))]
  (trap "a" (loop [Present_then ("OUT", exit_l "a"); pause]))

let test_abro ctx = assert_equal
    [%sync_ast loopeach (await a || await b; emit o;) r]
    (loop_each "r" @@ !![await "a" // await "b"; emit "o"])


let test_every ctx = assert_equal
    [%sync_ast every (await a || await b; emit o;) r]
    (Every ("r", !![await "a" // await "b"; emit "o"]))


let suite =
  "Test_ppx_pendulum_syntax">::: [
    "every">:: test_every;
    "abro">:: test_abro;
    "halt_exit_trap">:: test_halt_exit_trap;
    "loop_par_emit_await">:: test_loop_par_emit_await;
    "loop_pause">:: test_loop_pause;
    "par">:: test_seq;
    "seq">:: test_seq;
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
