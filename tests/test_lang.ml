
open OUnit


open Runtime_ast




let loop_pause () =

  loop [
    pause
  ]


let loop_emit_pause () =
  loop [
    emit "HOP";
    pause
  ]


let loop_par_emit () =
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
