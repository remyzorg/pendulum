







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


module Controles = struct

  let pressed = Array.make 1000 0

  let keycode e = e##.keyCode

  type key = Right | Left | Up | Nope of int | Refresh

  let string_of_key = function
    | Right -> "Right" | Left -> "Left" | Refresh -> "Refresh"
    | Up -> "Up" | Nope i -> "Nope " ^ string_of_int i

  let to_key = function
    | 37 | 72 | 81 -> Left
    | 39 | 76 | 68 -> Right
    | 32 | 38 -> Up
    | 82 -> Refresh
    | i -> Nope i


end

open Controles

let is_move =
  function Right | Left | Up -> true | _ -> false


let clear ctx = ctx##clearRect 0. 0.
    (float_of_int ctx##.canvas##.clientWidth)
    (float_of_int ctx##.canvas##.clientHeight)


type entity = {
  x : float; y : float; w : float; h : float; color : string;
  vx : float; vy : float
}

let pp_entity () e =
  Format.sprintf "{x = %f,y = %f); vx = %f}"
    e.x e.y e.vx

type model = {
  ground : float;
  player : entity
}

let pp_model () m =
  Format.sprintf "{player = %a}" pp_entity m.player


let draw_model ctx model =
  ctx##.fillStyle := Js.string model.player.color;
  let y = float_of_int ctx##.canvas##.clientHeight
          -. model.ground -. model.player.h -. model.player.y
  in ctx##fillRect model.player.x y model.player.w model.player.h

let init_model ctx =
  let h = 20. in
  let x = float_of_int @@ ctx##.canvas##.clientWidth / 2 in
  let ground = 0. in
  {ground; player = {x; y = 0.; w = 10.; h; color = "red"; vx = 10.; vy = 0.}}

let move_entity e dir =
  match dir with
  | Right -> { e with x = e.x +. e.vx }
  | Left -> { e with x = e.x -. e.vx }
  | Up when e.y = 0. -> { e with vy = 24. }
  | _ -> e


let move_model _ m move = { m with player = move_entity m.player move }
let gravity _ ({player} as m) =
  let y = max 0. (player.y +. player.vy) in
  let vy = if y > 0. then player.vy -. 4. else 0. in
  {m with player = { player with y; vy;} }


let%sync game ~obj w ctx dt =
  input keydowns (fun acc k -> k :: acc);
  input keyups (fun acc k -> k :: acc);

  let model = init_model !!ctx in
  let redraw = () in
  let left = () in let right = () in let up = () in

  loop begin
    present (keydowns & List.mem Left !!keydowns) (
      trap up (loop (
          present (keyups & List.mem Left !!keyups)
            (exit up)
        ; emit left ()
        ; pause))
    ) ; pause
  end
  ||
  loop begin
    present (keydowns & List.mem Right !!keydowns) (
      trap up (loop (
          present (keyups & List.mem Right !!keyups)
            (exit up)
        ; emit right ()
        ; pause))
    ) ; pause
  end
  ||
  loop begin
    present (keydowns & List.mem Up !!keydowns) (
      trap up (loop (
          present (keyups & List.mem Up !!keyups)
            (exit up)
        ; emit up ()
        ; pause))
    ) ; pause
  end
  ||
  loop begin
    !(clear !!ctx);

       present (keydowns & List.mem Refresh !!keydowns) !((!!w)##.location##reload)
    || present left (emit model (move_model dt !!model Left))
    || present right (emit model (move_model dt !!model Right))
    || present up (emit model (move_model dt !!model Up))

    ; emit model (gravity dt !!model)
    ; emit redraw
    ; pause
  end
  ||
  loop begin
    present redraw !(draw_model !!ctx !!model);
    pause
  end




let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->

      let canvas = "canvas" @> CoerceTo.canvas in
      let ctx = canvas##getContext (Dom_html._2d_) in

      let g = game (window, ctx, 0., [], []) in

      window##.onkeydown := handler (fun ev ->
          let k = (to_key ev##.keyCode) in
          g#keydowns k; Js._false
        );

      window##.onkeyup := handler (fun ev ->
          let k = (to_key ev##.keyCode) in
          g#keyups k; Js._false
        );

      let tick  = ref 20. in

      let rec loop_raf timestamp =
        let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in
        if timestamp > !tick +. 20. then begin
          let elaps = timestamp -. !tick in
          tick := timestamp;
          g#dt elaps;
          ignore @@ g#react;
        end;
        ()
      in
      let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in




      Js._false
    )

