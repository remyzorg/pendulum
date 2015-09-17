
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

type circle = {x : float; y : float; radius : float; color : string}
let mk_circle ?(color="black") x y radius = {x; y; radius; color}


let draw_circle ctx c =
  let pi = 3.14159265 in
  ctx##.fillStyle := (Js.string c.color);
  ctx##beginPath;
  ctx##arc c.x c.y c.radius 0. (pi*.2.) Js._true;
  ctx##fill


let clear ctx h w = ctx##clearRect 0. 0. (float_of_int h) (float_of_int w)


let dist = 400.
let radius = 20.

let move_circle ctx c x y =
  draw_circle ctx {c with x; y}

let%sync planet =
  input circle, ctx, move, quit;
  begin
    trap ex (
      loop (
        present quit (exit ex);
        atom (move_circle !!ctx !!circle (fst !!move) !!circle.y);
        emit circle { !!circle with x = fst !!move; };
        pause
      ))
  end

let mouse_machine =
  let open Pendulum.Runtime_misc in
  let open Pendulum.Machine in
  fun (ctx,hw,move,quit1,quit2)  ->
    let pendulum_state = Bitset.make 9 in
    let ctx = make_signal ctx in
    let hw = make_signal hw in
    let move = make_signal move in
    let quit1 = make_signal quit1 in
    let quit2 = make_signal quit2 in
    let dist = ref (make_signal 400.) in
    let radius = let dist = !dist in ref (make_signal 20.) in
    let circle1 =
      let dist = !dist in
      let radius = !radius in
      ref
        (make_signal
           {
             x = (fst (!! move));
             y = (!! dist);
             radius = (!! radius);
             color = "green"
           }) in
    let circle2 =
      let dist = !dist in
      let radius = !radius in
      let circle1 = !circle1 in
      ref
        (make_signal
           {
             x = (!! dist);
             y = (snd (!! move));
             radius = (!! radius);
             color = "red"
           }) in
    let set_absent () =
      set_absent ctx;
      set_absent hw;
      set_absent move;
      set_absent quit1;
      set_absent quit2;
      set_absent (!dist);
      set_absent (!radius);
      set_absent (!circle1);
      set_absent (!circle2);
      () in
    let (planet_1_set_arg_0,planet_1_set_arg_1,planet_1_set_arg_2,planet_1_set_arg_3,planet_1_step)
      =
      let (planet_1_set_arg_0,planet_1_set_arg_1,planet_1_set_arg_2,planet_1_set_arg_3,planet_1_step)
        = planet ((!! (!circle2)), (!! ctx), (!! move), (!! quit2)) in
      ((ref planet_1_set_arg_0), (ref planet_1_set_arg_1),
       (ref planet_1_set_arg_2), (ref planet_1_set_arg_3),
       (ref planet_1_step)) in
    let (planet_set_arg_0,planet_set_arg_1,planet_set_arg_2,planet_set_arg_3,planet_step)
      =
      let (planet_set_arg_0,planet_set_arg_1,planet_set_arg_2,planet_set_arg_3,planet_step)
        = planet ((!! (!circle1)), (!! ctx), (!! move), (!! quit1)) in
      ((ref planet_set_arg_0), (ref planet_set_arg_1),
       (ref planet_set_arg_2), (ref planet_set_arg_3), (ref planet_step)) in
    (((fun set_arg  -> set_present_value ctx set_arg),
      (fun set_arg  -> set_present_value hw set_arg),
      (fun set_arg  -> set_present_value move set_arg),
      (fun set_arg  -> set_present_value quit1 set_arg),
      (fun set_arg  -> set_present_value quit2 set_arg)),
     (fun ()  ->
        try
          if Bitset.mem pendulum_state 0
          then raise Finish_exc
          else
            (if Bitset.mem pendulum_state 7
             then
               (if Bitset.mem pendulum_state 1
                then
                  (if
                    not
                      (if !? quit1 then (!planet_set_arg_3) (!! quit1);
                       if !? move then (!planet_set_arg_2) (!! move);
                       if !? ctx then (!planet_set_arg_1) (!! ctx);
                       if !? (!circle1)
                       then (!planet_set_arg_0) (!! (!circle1));
                       ((!planet_step) ()) == Pause)
                   then Bitset.remove pendulum_state 1);
                if Bitset.mem pendulum_state 2
                then
                  (if
                    not
                      (if !? quit2 then (!planet_1_set_arg_3) (!! quit2);
                       if !? move then (!planet_1_set_arg_2) (!! move);
                       if !? ctx then (!planet_1_set_arg_1) (!! ctx);
                       if !? (!circle2)
                       then (!planet_1_set_arg_0) (!! (!circle2));
                       ((!planet_1_step) ()) == Pause)
                   then Bitset.remove pendulum_state 2))
             else
               (Bitset.add pendulum_state 7;
                dist := (make_signal 400.);
                Bitset.add pendulum_state 6;
                radius := (make_signal (let dist = !dist in 20.));
                Bitset.add pendulum_state 5;
                circle1 :=
                  (make_signal
                     (let dist = !dist in
                      let radius = !radius in
                      {
                        x = (fst (!! move));
                        y = (!! dist);
                        radius = (!! radius);
                        color = "green"
                      }));
                Bitset.add pendulum_state 4;
                circle2 :=
                  (make_signal
                     (let dist = !dist in
                      let radius = !radius in
                      let circle1 = !circle1 in
                      {
                        x = (!! dist);
                        y = (snd (!! move));
                        radius = (!! radius);
                        color = "red"
                      }));
                Bitset.add pendulum_state 3;
                Bitset.add pendulum_state 1;
                Bitset.add pendulum_state 2;
                (let (planet_set_arg_0',planet_set_arg_1',planet_set_arg_2',planet_set_arg_3',planet_step')
                  =
                  planet
                    ((!! (!circle1)), (!! ctx), (!! move), (!! quit1)) in
                 planet_set_arg_0 := planet_set_arg_0';
                 planet_set_arg_1 := planet_set_arg_1';
                 planet_set_arg_2 := planet_set_arg_2';
                 planet_set_arg_3 := planet_set_arg_3';
                 planet_step := planet_step');
                (let (planet_1_set_arg_0',planet_1_set_arg_1',planet_1_set_arg_2',planet_1_set_arg_3',planet_1_step')
                  =
                  planet
                    ((!! (!circle2)), (!! ctx), (!! move), (!! quit2)) in
                 planet_1_set_arg_0 := planet_1_set_arg_0';
                 planet_1_set_arg_1 := planet_1_set_arg_1';
                 planet_1_set_arg_2 := planet_1_set_arg_2';
                 planet_1_set_arg_3 := planet_1_set_arg_3';
                 planet_1_step := planet_1_step');
                if
                  not
                    (if !? quit1 then (!planet_set_arg_3) (!! quit1);
                     if !? move then (!planet_set_arg_2) (!! move);
                     if !? ctx then (!planet_set_arg_1) (!! ctx);
                     if !? (!circle1)
                     then (!planet_set_arg_0) (!! (!circle1));
                     ((!planet_step) ()) == Pause)
                then Bitset.remove pendulum_state 1;
                if
                  not
                    (if !? quit2 then (!planet_1_set_arg_3) (!! quit2);
                     if !? move then (!planet_1_set_arg_2) (!! move);
                     if !? ctx then (!planet_1_set_arg_1) (!! ctx);
                     if !? (!circle2)
                     then (!planet_1_set_arg_0) (!! (!circle2));
                     ((!planet_1_step) ()) == Pause)
                then Bitset.remove pendulum_state 2);
             if
               (Bitset.mem pendulum_state 1) ||
               (Bitset.mem pendulum_state 2)
             then raise Pause_exc
             else
               (Bitset.remove pendulum_state 3;
                Bitset.remove pendulum_state 4;
                Bitset.remove pendulum_state 5;
                Bitset.remove pendulum_state 6;
                Bitset.remove pendulum_state 7;
                raise Finish_exc))
        with | Pause_exc  -> (set_absent (); Pause)
             | Finish_exc  ->
               (set_absent (); Bitset.add pendulum_state 0; Finish)))

(* let%to_dot_grc mouse_machine = *)
(*   input ctx; *)
(*   input hw; *)
(*   input move; *)
(*   input quit1; *)
(*   input quit2; *)

(*   let dist = 400. in *)
(*   let radius = 20. in *)
(*   let circle1  = ({x = fst (!!move); y = !!dist; radius = !!radius; color = "green"}) in *)
(*   let circle2 = ({x = !!dist; y = snd !!move; radius = !!radius; color = "red"}) in *)
(*   begin run planet (circle1, ctx, move, quit1) end *)
(*   || *)
(*   begin run planet (circle2, ctx, move, quit2) end *)






let c10 = mk_circle 400. 400. 20. ~color:"blue"
let pi = 4.0 *. atan 1.0

let (+%.) = fun a b -> float @@ (int_of_float a mod b)

let _ =
  let open Dom_html in

  window##.onload := handler (fun _ ->
      let canvas = "canvas" @> CoerceTo.canvas in
      let ctx = canvas##getContext (Dom_html._2d_) in
      let (set_ctx, set_hw, set_move, set_quit1, set_quit2), step =
        mouse_machine (ctx, (canvas##.height, canvas##.width), (0.,0.), (), ())
      in
      let is_eq k q = Js.Optdef.case k
          (fun _ -> false)
          (fun x -> x = Char.code q)
      in

      let c = ref (mk_circle ~color:"yellow" 400. 400. 30.) in
      let c1 = ref (mk_circle ~color:"blue" 400. 400. 20.) in
      let c2 = ref (mk_circle ~color:"red" 400. 400. 10.) in

      let draw center c radius inc angle =

        (* clear ctx canvas##.height canvas##.width; *)

        c := {!c with
          x = (!center).x +. radius *. (cos angle) *. pi;
          y = (!center).y +. radius *. (sin angle) *. pi
        };

        let angle = if angle > 360. then inc else angle +. inc in

        draw_circle ctx !c;
        angle
      in

      draw_circle ctx !c;

      let rec redraw_clear _ =
        clear ctx canvas##.height canvas##.width;
        let _ = window##requestAnimationFrame (Js.wrap_callback redraw_clear) in
        ()
      in

      let rec redraw_circle _ =
        step ();
        let _ = window##requestAnimationFrame (Js.wrap_callback redraw_circle) in
        ()
      in

      let rec redraw_planets angle1 angle2 _ =
        draw_circle ctx !c;
        let angle1 = draw c c1 50. 0.005 angle1 in
        let angle2 = draw c1 c2 20. 0.1 angle2 in
        let _ = window##requestAnimationFrame (Js.wrap_callback @@ redraw_planets angle1 angle2) in
        ()
      in

      let reqid = window##requestAnimationFrame (Js.wrap_callback redraw_clear) in
      let reqid = window##requestAnimationFrame (Js.wrap_callback redraw_circle) in
      let reqid = window##requestAnimationFrame (Js.wrap_callback @@ redraw_planets 0. 0.) in

      window##.onkeypress := handler (fun e ->
          if is_eq e##.charCode 'q' then begin
            set_quit1 ()
          end
          else if is_eq e##.charCode 'a' then set_quit2 ();
          Js._false
        );

      window##.onmousemove := handler (fun e ->
          let x, y = (float @@ e##.clientX, float @@ e##.clientY) in
          set_move (x, y);
          Js._false);

      Js._false)

