
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

let erase_circle ctx c =
  draw_circle ctx {c with color ="white"; radius = c.radius +. 1.}

let move_circle ctx c x y =
  erase_circle ctx c;
  draw_circle ctx {c with x; y}

(* let mouse_machine = *)
(*   let open Pendulum.Runtime_misc in *)
(*     let open Pendulum.Machine in *)
(*       fun (ctx,move,quit1,quit2)  -> *)
(*         let pendulum_state = Bitset.make 35 in *)
(*         let ctx = make_signal ctx in *)
(*         let move = make_signal move in *)
(*         let quit1 = make_signal quit1 in *)
(*         let quit2 = make_signal quit2 in *)
(*         let circle = *)
(*           ref *)
(*             (make_signal *)
(*                { x = (fst (!! move)); y = 50.; radius = 20.; color = "green" *)
(*                }) in *)
(*         let circle_1 = *)
(*           ref *)
(*             (make_signal *)
(*                { x = 50.; y = (snd (!! move)); radius = 20.; color = "red" }) in *)
(*         let set_absent () = *)
(*           set_absent ctx; *)
(*           set_absent move; *)
(*           set_absent quit1; *)
(*           set_absent quit2; *)
(*           set_absent (!circle); *)
(*           set_absent (!circle_1); *)
(*           () in *)
(*         (((fun set_arg  -> set_present_value ctx set_arg), *)
(*            (fun set_arg  -> set_present_value move set_arg), *)
(*            (fun set_arg  -> set_present_value quit1 set_arg), *)
(*            (fun set_arg  -> set_present_value quit2 set_arg)), *)
(*           (fun ()  -> *)
(*              if Bitset.mem pendulum_state 0 *)
(*              then (set_absent (); Bitset.add pendulum_state 0; Finish) *)
(*              else *)
(*                (if Bitset.mem pendulum_state 33 *)
(*                 then *)
(*                   (if Bitset.mem pendulum_state 13 *)
(*                    then *)
(*                      (Bitset.remove pendulum_state 9; *)
(*                       Bitset.remove pendulum_state 10; *)
(*                       Bitset.remove pendulum_state 11; *)
(*                       Bitset.add pendulum_state 11; *)
(*                       Bitset.add pendulum_state 3; *)
(*                       if !? quit1 *)
(*                       then *)
(*                         (Bitset.remove pendulum_state 13; *)
(*                          Bitset.add pendulum_state 31; *)
(*                          (let circle = !circle in *)
(*                           let () = erase_circle (!! ctx) (!! circle) in ()); *)
(*                          Bitset.add pendulum_state 30; *)
(*                          (let circle = !circle in *)
(*                           circle_1 := *)
(*                             (make_signal *)
(*                                { *)
(*                                  x = 50.; *)
(*                                  y = (snd (!! move)); *)
(*                                  radius = 20.; *)
(*                                  color = "red" *)
(*                                })); *)
(*                          Bitset.add pendulum_state 29; *)
(*                          Bitset.add pendulum_state 27; *)
(*                          Bitset.add pendulum_state 26; *)
(*                          Bitset.add pendulum_state 25; *)
(*                          Bitset.add pendulum_state 17; *)
(*                          if !? quit2 *)
(*                          then *)
(*                            (Bitset.remove pendulum_state 27; *)
(*                             (let circle = !circle in *)
(*                              let circle = !circle_1 in *)
(*                              let () = erase_circle (!! ctx) (!! circle) in ()); *)
(*                             Bitset.remove pendulum_state 29; *)
(*                             Bitset.remove pendulum_state 30) *)
(*                          else *)
(*                            (Bitset.remove pendulum_state 17; *)
(*                             Bitset.add pendulum_state 24; *)
(*                             Bitset.add pendulum_state 22; *)
(*                             if !? move *)
(*                             then *)
(*                               (Bitset.add pendulum_state 21; *)
(*                                (let circle = !circle in *)
(*                                 let circle = !circle_1 in *)
(*                                 let () = *)
(*                                   move_circle (!! ctx) (!! circle) *)
(*                                     (!! circle).x (snd (!! move)) in *)
(*                                 ()); *)
(*                                set_present_value (!circle_1) *)
(*                                  (let circle = !circle in *)
(*                                   let circle = !circle_1 in *)
(*                                   { *)
(*                                     (!! circle) with *)
(*                                     x = ((!! circle).x); *)
(*                                     y = (snd (!! move)) *)
(*                                   }); *)
(*                                Bitset.remove pendulum_state 21); *)
(*                             Bitset.remove pendulum_state 22; *)
(*                             Bitset.add pendulum_state 23)) *)
(*                       else *)
(*                         (Bitset.remove pendulum_state 3; *)
(*                          Bitset.add pendulum_state 10; *)
(*                          Bitset.add pendulum_state 8; *)
(*                          if !? move *)
(*                          then *)
(*                            (Bitset.add pendulum_state 7; *)
(*                             (let circle = !circle in *)
(*                              let () = *)
(*                                move_circle (!! ctx) (!! circle) *)
(*                                  (fst (!! move)) (!! circle).y in *)
(*                              ()); *)
(*                             set_present_value (!circle) *)
(*                               (let circle = !circle in *)
(*                                { *)
(*                                  (!! circle) with *)
(*                                  x = (fst (!! move)); *)
(*                                  y = ((!! circle).y) *)
(*                                }); *)
(*                             Bitset.remove pendulum_state 7); *)
(*                          Bitset.remove pendulum_state 8; *)
(*                          Bitset.add pendulum_state 9; *)
(*                          set_absent (); *)
(*                          Pause)) *)
(*                    else *)
(*                      if Bitset.mem pendulum_state 30 *)
(*                      then *)
(*                        (if Bitset.mem pendulum_state 27 *)
(*                         then *)
(*                           (Bitset.remove pendulum_state 23; *)
(*                            Bitset.remove pendulum_state 24; *)
(*                            Bitset.remove pendulum_state 25; *)
(*                            Bitset.add pendulum_state 25; *)
(*                            Bitset.add pendulum_state 17; *)
(*                            if !? quit2 *)
(*                            then *)
(*                              (Bitset.remove pendulum_state 27; *)
(*                               (let circle = !circle in *)
(*                                let circle = !circle_1 in *)
(*                                let () = erase_circle (!! ctx) (!! circle) in *)
(*                                ()); *)
(*                               Bitset.remove pendulum_state 29; *)
(*                               Bitset.remove pendulum_state 30) *)
(*                            else *)
(*                              (Bitset.remove pendulum_state 17; *)
(*                               Bitset.add pendulum_state 24; *)
(*                               Bitset.add pendulum_state 22; *)
(*                               if !? move *)
(*                               then *)
(*                                 (Bitset.add pendulum_state 21; *)
(*                                  (let circle = !circle in *)
(*                                   let circle = !circle_1 in *)
(*                                   let () = *)
(*                                     move_circle (!! ctx) (!! circle) *)
(*                                       (!! circle).x (snd (!! move)) in *)
(*                                   ()); *)
(*                                  set_present_value (!circle_1) *)
(*                                    (let circle = !circle in *)
(*                                     let circle = !circle_1 in *)
(*                                     { *)
(*                                       (!! circle) with *)
(*                                       x = ((!! circle).x); *)
(*                                       y = (snd (!! move)) *)
(*                                     }); *)
(*                                  Bitset.remove pendulum_state 21); *)
(*                               Bitset.remove pendulum_state 22; *)
(*                               Bitset.add pendulum_state 23)) *)
(*                         else *)
(*                           (Bitset.remove pendulum_state 29; *)
(*                            Bitset.remove pendulum_state 30)); *)
(*                    if *)
(*                      (Bitset.mem pendulum_state 14) || *)
(*                        (Bitset.mem pendulum_state 30) *)
(*                    then (set_absent (); Pause) *)
(*                    else *)
(*                      (Bitset.remove pendulum_state 31; *)
(*                       Bitset.remove pendulum_state 32; *)
(*                       Bitset.remove pendulum_state 33; *)
(*                       set_absent (); *)
(*                       Bitset.add pendulum_state 0; *)
(*                       Finish)) *)
(*                 else *)
(*                   (Bitset.add pendulum_state 33; *)
(*                    circle := *)
(*                      (make_signal *)
(*                         { *)
(*                           x = (fst (!! move)); *)
(*                           y = 50.; *)
(*                           radius = 20.; *)
(*                           color = "green" *)
(*                         }); *)
(*                    Bitset.add pendulum_state 32; *)
(*                    Bitset.add pendulum_state 13; *)
(*                    Bitset.add pendulum_state 12); *)
(*                 Bitset.add pendulum_state 11; *)
(*                 Bitset.add pendulum_state 3; *)
(*                 if !? quit1 *)
(*                 then *)
(*                   (Bitset.remove pendulum_state 13; *)
(*                    Bitset.add pendulum_state 31; *)
(*                    (let circle = !circle in *)
(*                     let () = erase_circle (!! ctx) (!! circle) in ()); *)
(*                    Bitset.add pendulum_state 30; *)
(*                    (let circle = !circle in *)
(*                     circle_1 := *)
(*                       (make_signal *)
(*                          { *)
(*                            x = 50.; *)
(*                            y = (snd (!! move)); *)
(*                            radius = 20.; *)
(*                            color = "red" *)
(*                          })); *)
(*                    Bitset.add pendulum_state 29; *)
(*                    Bitset.add pendulum_state 27; *)
(*                    Bitset.add pendulum_state 26; *)
(*                    Bitset.add pendulum_state 25; *)
(*                    Bitset.add pendulum_state 17; *)
(*                    if !? quit2 *)
(*                    then *)
(*                      (Bitset.remove pendulum_state 27; *)
(*                       (let circle = !circle in *)
(*                        let circle = !circle_1 in *)
(*                        let () = erase_circle (!! ctx) (!! circle) in ()); *)
(*                       Bitset.remove pendulum_state 29; *)
(*                       Bitset.remove pendulum_state 30) *)
(*                    else *)
(*                      (Bitset.remove pendulum_state 17; *)
(*                       Bitset.add pendulum_state 24; *)
(*                       Bitset.add pendulum_state 22; *)
(*                       if !? move *)
(*                       then *)
(*                         (Bitset.add pendulum_state 21; *)
(*                          (let circle = !circle in *)
(*                           let circle = !circle_1 in *)
(*                           let () = *)
(*                             move_circle (!! ctx) (!! circle) (!! circle).x *)
(*                               (snd (!! move)) in *)
(*                           ()); *)
(*                          set_present_value (!circle_1) *)
(*                            (let circle = !circle in *)
(*                             let circle = !circle_1 in *)
(*                             { *)
(*                               (!! circle) with *)
(*                               x = ((!! circle).x); *)
(*                               y = (snd (!! move)) *)
(*                             }); *)
(*                          Bitset.remove pendulum_state 21); *)
(*                       Bitset.remove pendulum_state 22; *)
(*                       Bitset.add pendulum_state 23); *)
(*                    if *)
(*                      (Bitset.mem pendulum_state 14) || *)
(*                        (Bitset.mem pendulum_state 30) *)
(*                    then (set_absent (); Pause) *)
(*                    else *)
(*                      (Bitset.remove pendulum_state 31; *)
(*                       Bitset.remove pendulum_state 32; *)
(*                       Bitset.remove pendulum_state 33; *)
(*                       set_absent (); *)
(*                       Bitset.add pendulum_state 0; *)
(*                       Finish)) *)
(*                 else *)
(*                   (Bitset.remove pendulum_state 3; *)
(*                    Bitset.add pendulum_state 10; *)
(*                    Bitset.add pendulum_state 8; *)
(*                    if !? move *)
(*                    then *)
(*                      (Bitset.add pendulum_state 7; *)
(*                       (let circle = !circle in *)
(*                        let () = *)
(*                          move_circle (!! ctx) (!! circle) (fst (!! move)) *)
(*                            (!! circle).y in *)
(*                        ()); *)
(*                       set_present_value (!circle) *)
(*                         (let circle = !circle in *)
(*                          { *)
(*                            (!! circle) with *)
(*                            x = (fst (!! move)); *)
(*                            y = ((!! circle).y) *)
(*                          }); *)
(*                       Bitset.remove pendulum_state 7); *)
(*                    Bitset.remove pendulum_state 8; *)
(*                    Bitset.add pendulum_state 9; *)
(*                    set_absent (); *)
(*                    Pause)))) *)

let%to_dot_grc mouse_machine =
  input ctx;
  input move;
  input quit1;
  input quit2;

  let circle  = ({x = fst (!!move); y = 50.; radius = 20.; color = "green"}) in
  trap ex1 (
    loop (
      present quit1 (exit ex1);
      present move (
        atom (
          move_circle !!ctx !!circle (fst !!move) !!circle.y
        );
        emit circle {!!circle with x = fst !!move; y = !!circle.y}
      ); pause)
  );
  atom (erase_circle !!ctx !!circle)
  ||
  let circle = ({x = 50.; y = snd (!!move); radius = 20.; color = "red"}) in
  begin
    trap ex2 (
      loop (
        present quit2 (exit ex2);
        present move (
          atom (
            move_circle !!ctx !!circle !!circle.x (snd !!move)
          );
          emit circle {!!circle with x = !!circle.x; y = snd !!move}
        ); pause));
    atom (erase_circle !!ctx !!circle)
  end


let _ =
  let open Dom_html in
  window##.onload := handler (fun _ ->
      let c = "canvas" @> CoerceTo.canvas in
      let ctx = c##getContext (Dom_html._2d_) in

      let (set_ctx, set_move, set_quit1, set_quit2), step =
        mouse_machine (ctx, (0.,0.), (), ())
      in
      let is_eq k q = Js.Optdef.get k (fun _ -> 0) = Char.code q in

      window##.onkeypress := handler (fun e ->
          if is_eq e##.charCode 'q' then begin
            set_quit1 ()
          end
          else if is_eq e##.charCode 'a' then set_quit2 ();
          ignore @@ step ();
          Js._false
        );

      window##.onmousemove := handler (fun e ->
          let x, y = (float @@ e##.clientX, float @@ e##.clientY) in
          set_move (x, y);
          ignore @@ step ();
          Js._false);

      Js._false)

