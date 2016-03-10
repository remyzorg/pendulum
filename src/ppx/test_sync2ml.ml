

open Pendulum_compiler
open Grc.Flowgraph


let sync49 =
  Sync ((4, 9), Pause,
                      Call ((Exit 1), Finish))

let call a t = Call (a, sync49)
let callexsync n = Call ((Exit n), sync49)
let callex4sync = Call ((Exit 4), sync49)
let callex9sync = Call ((Exit 9), sync49)

let fork_br1 = Test ((Selection 4),
                               callex4sync,
                               sync49)

let callex8ex6 = Call ((Exit 8),
                       Call ((Exit 6),
                             callex9sync))

let () = assert (callex9sync == callex9sync)

let fork_br2 = Test ((Selection 9),
                     Test ((Selection 6),
                           callex8ex6,
                           callex9sync),
                     sync49)

let callex100sync = Call ((Exit 100), sync49)

let dumb = Ast_helper.Exp.constant (Asttypes.Const_int 0)
let ms = Ast.mk_loc
let msv s = Ast.mk_vsig (ms s) dumb


let fork_br2_2 =
  let br1, br2 = Grc.Schedule.replace_join callex8ex6 callex9sync (fun x ->
      match x with
      | Call (a, t) -> (* Sync2ml.children callex100sync t t *) callex100sync
      | t ->
        (* Format.eprintf "%a\n" Grc.Flowgraph.pp t; *)
        (* Format.eprintf "Omg je me fais jambonner@\n" ;*) assert false
    ) in
  Test ((Selection 9), Test ((Selection 6), br1, br2), sync49)


let test_fg = Test (Finished, Finish,
                 Test ((Selection 1),
                       Fork (fork_br1, fork_br2, sync49),
                       Call ((Enter 1),
                             Fork (
                               Call ((Enter 4),
                                     Test ((Signal (ms "S1")),
                                           Call ((Emit (msv "S2")),
                                                 callex4sync),
                                           callex4sync)),
                               Call ((Enter 9),
                                     Test ((Signal (ms "S2")),
                                           Call ((Enter 6),
                                                 Call ((Emit (msv "RI1DUTOO")),
                                                       Call ((Enter 8), sync49))),
                                           callex9sync)),
                               sync49))))


let () =
  Pendulum_misc.print_to_dot_one "test_sync2mllolz" "_fg" Grc.Flowgraph.print_to_dot test_fg
(* ; print_to_dot_one name "_interfg" Grc.Flowgraph.print_to_dot (Sync2ml.interleave fg) *)

let () =
  let res, mod_test_fg = Grc.Schedule.find_and_replace (fun x ->
      match x with
      | Call (a, t) -> callex100sync
      | _ -> assert false
    ) test_fg callex9sync  in
  Pendulum_misc.print_to_dot_one "test_sync2mllolz_mod" "_fg" Grc.Flowgraph.print_to_dot mod_test_fg

let sync126 =
  Sync((12, 6),
       Pause,
       Call(Exit 13,
            Finish))

let test12 =
Test(Selection 12,
 Call(Exit 7,
  Call(Exit 11,
   Call(Enter 11,
    Call(Enter 10,
     Test(Signal (ms "b"),
      Call(Atom (Ast.mk_atom dumb),
       Call(Exit 10,
        Call(Enter 7,
         sync126))),
      Call(Exit 10,
       Call(Enter 7,
        sync126))))))),
 sync126)

let test6 =
Test(Selection 6,
 Call(Exit 1,
  Call(Exit 5,
   Call(Enter 5,
    Call(Enter 4,
     Test(Signal (ms "a"),
      Call(Emit (msv "b"),
       Call(Exit 4,
        Call(Enter 1,
         sync126))),
      Call(Exit 4,
       Call(Enter 1,
        sync126))) )))),
 sync126)

let () =
  let f = (fun fg -> Call (Exit 10000000, Finish)) in
  let test6', test12' =
    Grc.Schedule.replace_join test12 test6 f
  in
  (* let _, test12' = Grc.Schedule.find_and_replace f test12 sync126 in *)
  Grc.Flowgraph.pp Format.std_formatter test6';
  Format.printf "\n===============\n";
  Grc.Flowgraph.pp Format.std_formatter test12'


let mod_test_fg_2 = Test (Finished, Finish,
                 Test ((Selection 1),
                       Fork (fork_br1, fork_br2_2, sync49),
                       Call ((Enter 1),
                             Fork (
                               Call ((Enter 4),
                                     Test ((Signal (ms "S1")),
                                           Call ((Emit (msv "S2")),
                                                 callex4sync),
                                           callex4sync)),
                               Call ((Enter 9),
                                     Test ((Signal (ms "S2")),
                                           Call ((Enter 6),
                                                 Call ((Emit (msv "RI1DUTOO")),
                                                       Call ((Enter 8), sync49))),
                                           callex9sync)),
                               sync49))))


let () =
  Pendulum_misc.print_to_dot_one "test_sync2mllolz_join"
    "_fg" Grc.Flowgraph.print_to_dot mod_test_fg_2

