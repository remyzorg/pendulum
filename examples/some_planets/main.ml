
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

let%sync mouse_machine =
  input ctx;
  input hw;
  input move;
  input quit1;
  input quit2;

  let dist = 400. in
  let radius = 20. in
  let circle1  = ({x = fst (!!move); y = !!dist; radius = !!radius; color = "green"}) in
  let circle2 = ({x = !!dist; y = snd !!move; radius = !!radius; color = "red"}) in
  begin run planet (circle1, ctx, move, quit1) end
  ||
  begin
    trap ex2 (
      loop (
        present quit2 (exit ex2);
        atom (move_circle !!ctx !!circle2 !!circle2.x (snd !!move));
        emit circle2 {!!circle with y = snd !!move};
        pause
      ))
  end






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

