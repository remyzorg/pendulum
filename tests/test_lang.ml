
open OUnit


open Ast
open Ast.Derived
open Grc




let loop_pause () =
  Ast.normalize @@
  loop [
    pause
  ]


let loop_emit_pause () =
  Ast.normalize @@
  loop [
    emit "HOP";
    pause
  ]


let loop_par_emit () =
  Ast.normalize @@
  Signal ("a",
          Loop (!! [
              Pause;
              emit "a";
              Signal ("a", emit "a")
            ])
          //
          Loop (!! [
              await "a";
              Atom (fun _ -> Format.printf "Hello")
            ]))


let loop_present_trap () =
  Ast.normalize @@
  trap "a" (
    loop [
      Present_then ("OUT", exit_l "a");
      pause
    ]
  )



let abro a b r o =
  loop_each r @@
    await a
    // !![await b; emit o]


let trap_par_loop =
  Ast.normalize @@

  trap "T" (
    loop [
      Present_then ("out", exit_l "T");
      pause;
    ] //
    loop [
      Present_then ("hop", emit "out");
      pause;
    ]
  )


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
