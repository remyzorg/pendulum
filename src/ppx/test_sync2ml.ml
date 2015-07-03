

(* open Pendulum_preproc *)
(* open Grc.Flowgraph *)


(* let sync49 = *)
(*   Sync ((4, 9), Pause, *)
(*                       Call ((Exit 1), Finish)) *)

(* let call a t = Call (a, sync49) *)
(* let callexsync n = Call ((Exit n), sync49) *)
(* let callex4sync = Call ((Exit 4), sync49) *)
(* let callex9sync = Call ((Exit 9), sync49) *)

(* let fork_br1 = Test ((Selection 4), *)
(*                                callex4sync, *)
(*                                sync49) *)

(* let callex8ex6 = Call ((Exit 8), *)
(*                        Call ((Exit 6), *)
(*                              callex9sync)) *)

(* let () = assert (callex9sync == callex9sync) *)

(* let fork_br2 = Test ((Selection 9), *)
(*                      Test ((Selection 6), *)
(*                            callex8ex6, *)
(*                            callex9sync), *)
(*                      sync49) *)

(* let callex100sync = Call ((Exit 100), sync49) *)

(* let fork_br2_2 = *)
(*   let br1, br2 = Grc.Schedule.replace_join callex8ex6 callex9sync (fun x -> *)
(*       match x with *)
(*       | Call (a, t) -> (\* Sync2ml.children callex100sync t t *\) callex100sync *)
(*       | t -> *)
(*         (\* Format.eprintf "%a\n" Grc.Flowgraph.pp t; *\) *)
(*         (\* Format.eprintf "Omg je me fais jambonner@\n" ;*\) assert false *)
(*     ) in *)
(*   Test ((Selection 9), Test ((Selection 6), br1, br2), sync49) *)


(* let test_fg = Test (Finished, Finish, *)
(*                  Test ((Selection 1), *)
(*                        Fork (fork_br1, fork_br2, sync49), *)
(*                        Call ((Enter 1), *)
(*                              Fork ( *)
(*                                Call ((Enter 4), *)
(*                                      Test ((Signal "S1"), *)
(*                                            Call ((Emit "S2"), *)
(*                                                  callex4sync), *)
(*                                            callex4sync)), *)
(*                                Call ((Enter 9), *)
(*                                      Test ((Signal "S2"), *)
(*                                            Call ((Enter 6), *)
(*                                                  Call ((Emit "RI1DUTOO"), *)
(*                                                        Call ((Enter 8), sync49))), *)
(*                                            callex9sync)), *)
(*                                sync49)))) *)


(* let () = *)
(*   Pendulum_misc.print_to_dot_one "test_sync2mllolz" "_fg" Grc.Flowgraph.print_to_dot test_fg *)
(* (\* ; print_to_dot_one name "_interfg" Grc.Flowgraph.print_to_dot (Sync2ml.interleave fg) *\) *)

(* let () = *)
(*   let res, mod_test_fg = Grc.Schedule.find_and_replace (fun x -> *)
(*       match x with *)
(*       | Call (a, t) -> callex100sync *)
(*       | _ -> assert false *)
(*     ) test_fg callex9sync  in *)
(*   Pendulum_misc.print_to_dot_one "test_sync2mllolz_mod" "_fg" Grc.Flowgraph.print_to_dot mod_test_fg *)




(* let mod_test_fg_2 = Test (Finished, Finish, *)
(*                  Test ((Selection 1), *)
(*                        Fork (fork_br1, fork_br2_2, sync49), *)
(*                        Call ((Enter 1), *)
(*                              Fork ( *)
(*                                Call ((Enter 4), *)
(*                                      Test ((Signal "S1"), *)
(*                                            Call ((Emit "S2"), *)
(*                                                  callex4sync), *)
(*                                            callex4sync)), *)
(*                                Call ((Enter 9), *)
(*                                      Test ((Signal "S2"), *)
(*                                            Call ((Enter 6), *)
(*                                                  Call ((Emit "RI1DUTOO"), *)
(*                                                        Call ((Enter 8), sync49))), *)
(*                                            callex9sync)), *)
(*                                sync49)))) *)


(* let () = *)
(*   Pendulum_misc.print_to_dot_one "test_sync2mllolz_join" *)
(*     "_fg" Grc.Flowgraph.print_to_dot mod_test_fg_2 *)



(* let () = Format.printf "Test sync2ml: OK \n"; *)
