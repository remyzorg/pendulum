







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


let keycode e = e##.keyCode

type key = Right | Left | Space | Nope of int | Refresh
let is_move = function Right | Left | Space -> true | _ -> false

let string_of_key = function Right -> "Right" | Left -> "Left"
  | Space -> "Space" | Nope i -> "Nope " ^ string_of_int i | Refresh -> "Refresh"

let to_key = function
  | 37 | 72 | 81 -> Left
  | 39 | 76 | 68 -> Right
  | 32 -> Space
  | 82 -> Refresh
  | i -> Nope i

let clear ctx = ctx##clearRect 0. 0.
    (float_of_int ctx##.canvas##.clientWidth)
    (float_of_int ctx##.canvas##.clientHeight)


type entity = {
  x : float; y : float; w : float; h : float; color : string;
  speed : float;
}

type model = {
  player : entity
}

let draw_model ctx model =
  ctx##.fillStyle := Js.string model.player.color;
  ctx##fillRect model.player.x model.player.y model.player.w model.player.h

let init_model ctx =
  let h = 10. in
  let x = float_of_int @@ ctx##.canvas##.clientWidth / 2 in
  let y = float_of_int @@ ctx##.canvas##.clientHeight - (int_of_float h) in
  {player = {x; y; w = 10.; h; color = "red"; speed = 1.}}

let move_entity e dir = {
  e with x = begin e.x +. match dir with
  | Right -> e.x +. e.speed
  | Left -> e.x -. e.speed
  | _ -> e.x
  end}

let%sync game ~obj w ctx key =

  let model = init_model !!ctx in
  let redraw = () in

  loop begin
    !(clear !!ctx);

    present (key & !!key = Refresh) !((!!w)##.location##reload);

    present (key & is_move !!key) begin
      emit model {player = move_entity (!!model).player !!key}
    end

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

      let g = game (window, ctx, Nope 0) in

      window##.onkeydown := handler (fun ev ->
          let k = (to_key ev##.keyCode) in
          g#key k; Js._false
        );

      let rec loop_raf _ =
        ignore @@ g#react;
        let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in
        ()
      in
      let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in




      Js._false
    )

