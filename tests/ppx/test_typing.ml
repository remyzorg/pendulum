[@@@warning "-27"]
[@@@warning "-32"]
[@@@warning "-33"]

open Pendulum
open Program
open Signal

let p =
  let%sync react_obj a =
    loop begin
      !(Format.printf "%d\n" !!a)
    ; pause
    end in react_obj#create 0
let () = p#a 10; ignore @@ p#react
(* 'a -> < react : Pendulum.Program.state; x : 'a -> unit > *)

let%sync p2 = loop pause
let _ : < react : unit; status : state> = p2#create
let _ : < react : unit; status : state> = p2#create_run
let%sync p2' s = loop begin
    run p2
  ; pause
  end
let _ : 'a -> < react : unit; status : state; s : 'a -> unit > = p2'#create
let _ : ('a, 'b) signal -> < react : unit; status : state; s : 'a -> unit > = p2'#create_run
let%sync p3 s = loop begin run p2' !("test" ^ !!s); pause end
let _ : string -> < react : unit; status : state; s : string -> unit > = p2'#create
let _ : (string, string) signal -> < react : unit; status : state; s : string -> unit > = p2'#create_run


let%sync p_out = input s; output o;
  loop begin
    emit o
  ; pause
  end
let _ : 'a -> unit * ('b -> unit) -> < react : unit; status : state; s : 'a -> unit > = p_out#create
let _ :
  ('a, 'c) signal ->
  (unit, unit) signal * (unit -> unit) ->
  < react : unit; status : state; s : 'a -> unit >
  = p_out#create_run

let%sync mouse =
  input i;
  output write (^);
  loop begin
    present i
      (emit write "")
  ; pause
  end
