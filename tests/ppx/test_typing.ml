
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
let _ : < react : state > = p2#create
let _ : < react : state > = p2#create_run
let%sync p2' s = loop begin
    run p2
  ; pause
  end
let _ : 'a -> < react : state; s : 'a -> unit > = p2'#create
let _ : ('a, 'b) signal -> < react : state; s : 'a -> unit > = p2'#create_run
let%sync p3 s = loop begin run p2' !("test" ^ !!s); pause end
let _ : string -> < react : state; s : string -> unit > = p2'#create
let _ : (string, string) signal -> < react : state; s : string -> unit > = p2'#create_run


let%sync p_out = input s; output o;
  loop begin
    emit o
  ; pause
  end
let _ : 'a -> unit * ('b -> unit) -> < react : state; s : 'a -> unit > = p_out#create
let _ :
  ('a, 'c) signal ->
  (unit, unit) signal * (unit -> unit) ->
  < react : state; s : 'a -> unit >
  = p_out#create_run


let mouse =
  let open Pendulum.Runtime_misc in
  let open Pendulum.Program in
  let open Pendulum.Signal in
  let create_local i write =
    let pendulum_state = Bitset.make 7  in
    let (write,write_out) = ((fst write), (snd write))  in
    let set_absent () =
      set_absent i; set_absent write; write.value <- write.default; ()
    in
    let p_react () =
      try
        if Bitset.mem pendulum_state 0
        then raise Finish_exc
        else
        if Bitset.mem pendulum_state 6
        then
          (Bitset.inter_union pendulum_state [|2147483631|] [|40|];
           if !? i
           then (set_present_value write ""; write_out write.value);
           Bitset.inter_union pendulum_state [|2147483639|] [|16|];
           raise Pause_exc)
        else
          (Bitset.union pendulum_state [|104|];
           if !? i
           then (set_present_value write ""; write_out write.value);
           Bitset.inter_union pendulum_state [|2147483639|] [|16|];
           raise Pause_exc)
      with | Pause_exc  -> (set_absent (); Pause)
           | Finish_exc  ->
             (set_absent (); Bitset.add pendulum_state 0; Finish)
    in
    object method i = set_present_value i method react = p_react () end
  in
  object
    method create i write =
      create_local (make_signal i)
        ((make_signal_gather ((fst write), (^))), (snd write))
    method create_run = create_local
  end

let%sync mouse =
  input i;
  output write (^);
  loop begin
    present i
      (emit write "")
  ; pause
  end
