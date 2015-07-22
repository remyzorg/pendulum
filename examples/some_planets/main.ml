
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


let draw_circle ctx c =
  let pi = 3.14159265 in
  ctx##.fillStyle := (Js.string c.color);
  ctx##beginPath;
  ctx##arc c.x c.y c.radius 0. (pi*.2.) Js._true;
  ctx##fill

let move_circle ctx c x y =
  draw_circle ctx {c with color ="white"; radius = c.radius +. 1.};
  draw_circle ctx {c with x; y}


let%sync mouse_machine =
  input ctx;
  input move;

  signal circle ({x = fst (!!move); y = 50.; radius = 20.; color = "green"})
    begin
      loop (present move (
          atom (
            move_circle !!ctx !!circle (fst !!move) !!circle.y
          );
          emit circle {!!circle with x = fst !!move; y = !!circle.y}
        ); pause)
    end
  ||
  signal circle ({x = 50.; y = snd (!!move); radius = 20.; color = "green"})
    begin
      loop (present move (
          atom (
            move_circle !!ctx !!circle !!circle.x (snd !!move)
          );
          emit circle {!!circle with x = !!circle.x; y = snd !!move}
        ); pause)
    end


let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->
      let c = "canvas" @> CoerceTo.canvas in
      let ctx = c##getContext (Dom_html._2d_) in

      let (set_ctx, set_move), step =
        mouse_machine (ctx, (0.,0.))
      in

      window##.onmousemove := handler (fun e ->
          let x, y = (float @@ e##.clientX, float @@ e##.clientY) in
          set_move (x, y);
          ignore @@ step ();
          Js._false);

      Js._false)

