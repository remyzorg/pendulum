
let simple () =
  Ast.normalize @@
  Signal ("a",
          Loop (Seq [
              Pause;
              emit "a";
              Signal ("a", emit "a")
            ])
          //
          Loop (Seq [
              await "a";
              Atom (fun _ -> Format.printf "Hello")
            ]))




let abro a b r o =
  loop_each r @@
    await a
    // await b ++ emit o
