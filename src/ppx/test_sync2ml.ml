

open Pendulum_preproc
open Grc.Flowgraph

let sync49 =
  Sync ((4, 9), Pause,
                      Call ((Exit 1), Finish))

let call a t = Call (a, sync49)
let callexsync n = Call ((Exit n), sync49)
let callex4sync = callexsync 4
let callex9sync = callexsync 9

let test_fg = Test (Finished, Finish,
                 Test ((Selection 1),
                       Fork (
                         Test ((Selection 4),
                               callex4sync,
                               sync49),
                         Test ((Selection 9),
                               Test ((Selection 6),
                                     Call ((Exit 8),
                                           Call ((Exit 6),
                                                 callex9sync)),
                                     callex9sync),
                               sync49), sync49),
                       Call ((Enter 1),
                             Fork (
                               Call ((Enter 4),
                                     Test ((Signal "S1"),
                                           Call ((Emit "S2"),
                                                 callex4sync),
                                           callex4sync)),
                               Call ((Enter 9),
                                     Test ((Signal "S2"),
                                           Call ((Enter 6),
                                                 Call ((Emit "RI1DUTOO"),
                                                       Call ((Enter 8), sync49))),
                                           callex9sync)),
                               sync49))))


let () =
  Format.printf "Test sync2ml";
  Pendulum_misc.print_to_dot_one "test_sync2mllolz" "_fg" Grc.Flowgraph.print_to_dot test_fg
(* ; print_to_dot_one name "_interfg" Grc.Flowgraph.print_to_dot (Sync2ml.interleave fg) *)


let () =
  let res, mod_test_fg = Sync2ml.find_and_replace test_fg callex9sync (fun x ->
      match x with
      | Call (a, t) -> Sync2ml.children (callexsync 100) t t
      | _ -> assert false
    ) in
  Format.printf "replaced: %B\n" res;
  Pendulum_misc.print_to_dot_one "test_sync2mllolz_mod" "_fg" Grc.Flowgraph.print_to_dot mod_test_fg
