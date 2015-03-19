

open Ast
open Ast.Derived
open Interp
open Grc





let abro a b r o =
  (* loop [ *)
  (*   await a; *)
  (* ] *)
  loop_each r @@
    !![await a // await b;
     emit o]

let simple () =
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

let gen_tagged_sel_fg p l =
  let abro = p
    |> Ast.normalize
    |> Tagged.of_ast ~env:l
  in
  abro, Grc.of_ast abro

let abro_tagged, (abro_sel, abro_fg) =
  gen_tagged_sel_fg (abro "a" "b" "r" "o") ["a"; "b"; "r"; "o"]



let simple_tagged, (simple_sel, simple_fg) =
  gen_tagged_sel_fg (simple ()) ["out"; "hop"]


let () =
  try
    let c_tagged = open_out "tagged.dot" in
    let c_flowgraph = open_out "flowgraph.dot" in
    let fmt_tagged = Format.formatter_of_out_channel c_tagged in
    let fmt_flowgraph = Format.formatter_of_out_channel c_flowgraph in

    Tagged.print_to_dot fmt_tagged abro_tagged ;
    Flowgraph.print_to_dot fmt_flowgraph abro_fg;

    close_out c_flowgraph; close_out c_tagged;

  with Ast.Error e -> Ast.print_error Format.std_formatter e
  (* Format.printf "%s@\n" (Ast.show_statement @@ normalize @@ loop [Nothing; Nothing; Pause]) *)
