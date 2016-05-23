







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
  speed : float;
}

let pp_entity () e =
  Format.sprintf "{x = %f,y = %f); speed = %f}"
    e.x e.y e.speed

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
  {ground; player = {x; y = 0.; w = 10.; h; color = "red"; speed = 2.}}


let move_entity e dir =
  {e with x = begin e.x +. match dir with
  | Right -> e.speed | Left -> ~-. (e.speed) | _ -> 0.end}

let move_model m move = { m with player = move_entity m.player move }

let%sync game ~obj w ctx =
  input keydowns (fun acc k -> k :: acc);
  input keyups (fun acc k -> k :: acc);

  let model = init_model !!ctx in
  let redraw = () in
  let left = () in let right = () in let up = () in

  loop begin
    !(debug "[%s]" (String.concat "; " @@ List.map string_of_key !!keydowns));
  end
  ||
  loop begin
    await (keydowns & List.mem Left !!keydowns); trap up begin
      loop (emit left (); pause)
      || loop (present (keyups & List.mem Left !!keyups) (exit up))
    end; pause
  end
  ||
  loop begin
    await (keydowns & List.mem Right !!keydowns); trap up begin
      loop (emit right (); pause)
      || loop (present (keyups & List.mem Right !!keyups) (exit up))
    end; pause
  end
  ||
  loop begin
    await (keydowns & List.mem Up !!keydowns); trap up begin
      loop (emit up (); pause)
      || loop (present (keyups & List.mem Up !!keyups) (exit up))
    end; pause
  end
  ||
  loop begin
    !(clear !!ctx);

    present (keydowns & List.mem Refresh !!keydowns) !((!!w)##.location##reload)
    || present left (emit model (move_model !!model Left))
    || present right (emit model (move_model !!model Right))
    || present up (emit model (move_model !!model Up))

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

      let g = game (window, ctx, [], [Nope 0]) in

      document##.onkeydown := handler (fun ev ->
          debug "";
          let k = (to_key ev##.keyCode) in
          g#keydowns k; Js._false
        );

      document##.onkeyup := handler (fun ev ->
          let k = (to_key ev##.keyCode) in
          g#keyups k; Js._false
        );

      let tick  = ref 0. in

      let rec loop_raf timestamp =
        let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in
        if timestamp > !tick then begin
          tick := timestamp +. 0.;
          ignore @@ g#react;
        end;
        ()
      in
      let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in




      Js._false
    )

