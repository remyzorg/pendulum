


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

(* w x y h *)
let sprites = [
  "wleft" , (15., 49. , 0. , 19.);
  "left"  , (14., 169., 0. , 20.);
  "right" , (14., 209., 0. , 20.);
  "wright", (15., 328., 0. , 19.);
  (* "dleft" , (16., 8.  , 38., 24.); *)
  (* "jleft" , (15., 89. , 43., 14.); *)
  (* "fleft" , (15., 49. , 39., 21.); *)
  "jleft"    , (16., 128., 40., 20.);
  (* "s5"    , (16., 168., 39., 22.); *)
  (* "s6"    , (16., 208., 39., 22.); *)
  "jright"    , (16., 248., 40., 20.);
  (* "s8"    , (15., 288., 43., 14.); *)
]

let to_jsstring s = Js.(some @@ string s)

let draw_sprites ctx =
  let img = Dom_html.(createImg document) in
  img##.src := Js.string "mario.png";
  List.iter (fun (name, (w, x, y, h)) ->
      ctx##drawImage_full img x y w h x y w h;
    ) sprites

let draw_image ctx name img x y =
  try
    let (w, xi, yi, h) = List.assoc name sprites in
    ctx##drawImage_full img xi yi w h x y w h;
  with Not_found -> ()


module Controles = struct

  let pressed = Array.make 1000 0

  let keycode e = e##.keyCode

  type key = Right | Run | Left | Up | Nope of int | Refresh
  type direction = R | L (* [@@deriving yojson, show] *)

  let string_of_key = function
    | Right -> "Right" | Left -> "Left" | Refresh -> "Refresh"
    | Run -> "Run" | Up -> "Up" | Nope i -> "Nope " ^ string_of_int i

  let to_key = function
    | 37 | 72 | 81 -> Left
    | 39 | 76 | 68 -> Right
    | 38 | 75 | 90 -> Up
    | 87 | 32 -> Run
    | 82 -> Refresh
    | i -> Nope i

end

open Controles

let is_move =
  function Right | Left | Up | Run -> true | _ -> false

let clear ctx = ctx##clearRect 0. 0.
    (float_of_int ctx##.canvas##.clientWidth)
    (float_of_int ctx##.canvas##.clientHeight)

type entity = {
  x : float;
  y : float;
  w : float;
  h : float;
  color : string;
  vx : float;
  vy : float;
  dir : direction;
  vx_mod : bool;
} (* [@@deriving yojson, show] *)

(* let pp_entity e = *)
(*   let json = entity_to_yojson e in *)
(*   Yojson.Safe.pretty_to_string ~std:true json *)


type model = {
  ground : float;
  player : entity
}

let draw_model dt img t ctx ({player = p} as m) =
  let asp = int_of_float @@ if dt = 0. then 20. else 20. /. dt in
  let asp = if p.vx_mod then asp / 2 else asp in
  let asp = if asp = 0 then 8 else asp in
  let walk = p.vx <> 0. && t mod asp < asp / 2 in
  let name =
    match p.dir with
    | R -> if p.y > 0. then "jright" else if walk then "wright" else "right"
    | L -> if p.y > 0. then "jleft" else if walk then "wleft" else "left"
  in
  let y = float_of_int ctx##.canvas##.clientHeight
          -. m.ground -. p.h -. p.y in
  let background = "back" @> Dom_html.CoerceTo.img in
  ctx##drawImage background 0. 0.;
  draw_image ctx name img p.x y

let init_model ctx = {
  ground = 0.;
  player = {
    x = float_of_int @@ ctx##.canvas##.clientWidth / 2;
    y = 0.; w = 10.; h = 20.; color = "red";
    vx = 1.; vx_mod = false;
    vy = 0.;
    dir = R;
  }}

let walk dir' ({vx; dir; vx_mod} as p) =
  let vx = match dir' with
    | L -> ~-.2.
    | R -> 2.
  in
  let vx_mod = dir = dir' && vx_mod in
  let vx = if vx_mod then vx *. 2. else vx in
  {p with vx; dir = dir'; vx_mod}

let jump p =
  if p.y = 0. then { p with vy = if p.vx_mod then 7. else 6.;
                   vx = p.vx *. 1.5} else p

let speed_up p =
  if p.y = 0. then { p with vx_mod = true } else p

let gravity dt p =
  { p with vy = if p.y > 0. then p.vy -. dt /. 4. else 0. }

let reset_vx p =
  let vx = if p.y = 0. then 0. else p.vx in
  let vx_mod = if p.y = 0. then false else p.vx_mod in
  { p with vx; vx_mod;}

let physics dt p =
  let x = p.x +. p.vx *. dt in
  let y = max 0. (p.y +. dt *. p.vy) in
  { p with x; y;}

let player f ({player} as m) = { m with player = f player }


let%sync plex keydowns keyups v out =
  loop begin
    present (keydowns & List.mem v !!keydowns) (
      trap up (loop (
          present (keyups & List.mem v !!keyups)
            (exit up)
        ; emit out
        ; pause))
    ) ; pause
  end

let%sync game w img ctx debuglb dt =
  input keydowns (fun acc k -> k :: acc);
  input keyups (fun acc k -> k :: acc);

  let model = init_model !!ctx in
  let frame = 0 in
  let redraw = () in
  let left = () in
  let right = () in
  let up = () in
  let run = () in

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
        ; emit right () ; pause))
    ) ; pause
  end
  ||
  loop begin
    present (keydowns & List.mem Up !!keydowns) (
      trap up (loop (
          present (keyups & List.mem Up !!keyups)
            (exit up)
        ; emit up () ; pause))
    ) ; pause
  end
  ||
  loop begin
    present (keydowns & List.mem Run !!keydowns) (
      trap up (loop (
          present (keyups & List.mem Run !!keyups)
            (exit up)
        ; emit run () ; pause))
    ) ; pause
  end
  ||
  loop begin
    emit frame (!!frame + 1);
    !(clear !!ctx)

    ; emit model (player (gravity !!dt) !!model)
    ; emit model (player reset_vx !!model)

    ; present (keydowns & List.mem Refresh !!keydowns) !((!!w)##.location##reload)
    || present run (emit model (player speed_up !!model))
    || present left
      (present right nothing
         (emit model (player (walk L) !!model)))
      (present right
         (emit model (player (walk R) !!model)))
    || present up (emit model (player jump !!model))

    ; emit model (player (physics !!dt) !!model)


    ; emit redraw
    ; pause
  end
  ||
  loop begin
    present redraw !(draw_model !!dt !!img !!frame !!ctx !!model)
    (* ; emit debuglb##.textContent (to_jsstring @@ pp_entity (!!model).player) *)
    ; pause
  end



let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->

      let debuglb = "debug" @> CoerceTo.pre in
      (* let debuglb2 = "debug2" @> CoerceTo.label in *)
      let canvas = "canvas" @> CoerceTo.canvas in
      let mario = "mario" @> CoerceTo.img in
      let ctx = canvas##getContext (Dom_html._2d_) in

      let g = game#create (window, mario, ctx, debuglb, 0., [], []) in

      window##.onkeydown := handler (fun ev ->
          g#keydowns (to_key ev##.keyCode); Js._false
        );
      window##.onkeyup := handler (fun ev ->
          g#keyups (to_key ev##.keyCode); Js._false
        );

      let fps = 30. in
      let interval = 1000. /. fps in
      let tick  = ref 0. in
      let rec loop_raf timestamp =
        let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in
        let delta = timestamp -. !tick in
        if delta > interval then begin
          tick := timestamp -. (mod_float delta interval);
          g#dt (delta /. 20.);
          ignore @@ g#react;
        end;
        ()
      in
      let _ = window##requestAnimationFrame (Js.wrap_callback loop_raf) in




      Js._false
    )

