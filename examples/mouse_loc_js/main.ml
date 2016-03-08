
open Firebug

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

let (@>) s coerce =
  Js.Opt.get (coerce @@ Dom_html.getElementById s)
    (fun () -> error "can't find element %s" s)

let iter opt f =
  match opt with
  | None -> ()
  | Some o -> f o



(* let%sync mouse_react ~dsource = *)
(*   input span; *)
(*   input w { *)
(*     onmousemove = "", fun x ev -> Format.sprintf "%d,%d" ev##.clientX ev##.clientY; *)
(*   }; *)

(*   input (newit : Dom_html.inputElement Js.t) { *)
(*     onkeydown = [], fun acc ev -> *)
(*         if enter_pressed ev && newit##.value##.length > 0 *)
(*         then newit##.value :: acc *)
(*         else acc *)
(*   }; *)

(*   loop begin *)
(*     present w##onmousemove ( *)
(*       emit span##.textContent *)
(*         Js.(some (string !!(w##onmousemove))) *)
(*     ); pause *)
(*   end *)

let mouse_react =
let open Pendulum.Runtime_misc in
  let open Pendulum.Machine in
    fun (span,w,(newit : Dom_html.inputElement Js.t))  ->
      let pendulum_state = Bitset.make 7 in
      let w__onmousemove =
        let ((arg__init,arg__gather) : ('a * ('a -> 'b -> 'a)))=
          ("",
            (fun x  ->
               fun ev  ->
                 Format.sprintf "%d,%d" (ev##. clientX) (ev##. clientY))) in
        make_signal_gather arg__gather arg__init in
      let newit__onkeydown =
        let ((arg__init,arg__gather) : ('a * ('a -> 'b -> 'a)))=
          ([],
            (fun acc  ->
               fun ev  ->
                 if
                   (enter_pressed ev) && (((newit##.value)##.length) > 0)
                 then (newit ##. value) :: acc
                 else acc))
        in
        make_signal_gather arg__gather arg__init in
      let set_absent () =
        set_absent w__onmousemove; set_absent newit##onkeydown; () in
      let p_stepfun () =
        try
          if Bitset.mem pendulum_state 0
          then raise Finish_exc
          else
            if Bitset.mem pendulum_state 6
            then
              (Bitset.inter_union pendulum_state [|2147483631|] [|8|];
               if !? w__onmousemove
               then
                 (span ##. textContent) :=
                   (let open Js in some (string (!! w__onmousemove)));
               Bitset.inter_union pendulum_state [|2147483639|] [|16|];
               raise Pause_exc)
            else
              (Bitset.union pendulum_state [|104|];
               if !? w__onmousemove
               then
                 (span ##. textContent) :=
                   (let open Js in some (string (!! w__onmousemove)));
               Bitset.inter_union pendulum_state [|2147483639|] [|16|];
               raise Pause_exc)
        with | Pause_exc  -> (set_absent (); Pause)
        | Finish_exc  -> (set_absent (); Bitset.add pendulum_state 0; Finish) in
      (w ##. onmousemove) :=
        (Dom_html.handler
           (fun ev  ->
              set_present_value w__onmousemove ev;
              ignore @@ (p_stepfun ());
              Js._true));
      (newit ##. onkeydown) :=
        (Dom_html.handler
           (fun ev  ->
              set_present_value newit##onkeydown ev;
              ignore @@ (p_stepfun ());
              Js._true));
      p_stepfun


let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->
      let area = "tarea" @> CoerceTo.a in
      let lol = "lol" @> CoerceTo.input in
      let _step = mouse_react (area, window, lol) in
      Js._false
    )

