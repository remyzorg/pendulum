
open Firebug

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

module CoerceTo = Dom_html.CoerceTo
let (@>) s coerce =
  Js.Opt.get Dom_html.(coerce @@ getElementById s)
    (fun () -> error "can't find element %s" s)




module View = struct

  let raf f =
    ignore @@ Dom_html.window##requestAnimationFrame
      (Js.wrap_callback @@ fun _ -> ignore @@ f (); ())

  let create_item cnt animate delete_sig str =
    let mli = Dom_html.(createLi document) in
    (* mli##.className := Js.string "editing"; *)
    let mdiv = Dom_html.(createDiv document)  in
    mdiv##.className := Js.string "view";
    let tgl = Dom_html.(createInput document) in
    tgl##.className := Js.string "toggle";
    tgl##setAttribute (Js.string "type")(Js.string "checkbox");
    let lbl = Dom_html.(createLabel document) in
    lbl##.textContent := Js.some str;
    let btn = Dom_html.(createButton document) in
    btn##.className := Js.string "destroy";
    Dom.appendChild mdiv tgl;
    Dom.appendChild mdiv lbl;
    Dom.appendChild mdiv btn;
    Dom.appendChild mli mdiv;
    btn##.onclick := Dom_html.handler (fun _  ->
        Pendulum.Machine.set_present_value delete_sig cnt;
        animate ();
        Js._true) ;
    mli




end

let iter f opt =
  match opt with
  | None -> ()
  | Some o -> f o

let default v f opt = 
  match opt with
  | None -> v
  | Some o -> f o

let jstr s = Js.some @@ Js.string ""

let is_eq_char k q = Js.Optdef.case k
    (fun _ -> false)
    (fun x -> x = Char.code q)

let enter_pressed ev =
   default false (fun ev ->
      Js.Optdef.case ev##.charCode
        (fun () -> false)
        (fun c -> c = 13)
    ) ev


module Controller = struct
  let%sync machine ~animate =
    input items_ul;
    input newit;

    let delete_item = -1 in
    let add_item = Dom_html.(createDiv document) in
    let tasks = [] in
    let cnt = 0 in
    loop begin
      present (newit##onkeypress
               & enter_pressed !!(newit##onkeypress)
               && newit##.value <> Js.string "")
        (emit cnt (!!cnt + 1);
         emit add_item (View.create_item !!cnt animate delete_item newit##.value))
    ; pause end
    ||
    loop begin
      present add_item (
        !(Dom.appendChild !!items_ul !!add_item; newit##.value := Js.string "");
        emit tasks ((!!cnt, !!add_item) :: !!tasks)
      ); pause
    end
    ||
    loop begin
      present delete_item (
        !(debug "delete item %d" !!delete_item);
        emit tasks (List.filter (fun (id, elt) ->
            if id = !!delete_item
            then (Dom.removeChild !!items_ul elt; false)
            else true
          ) !!tasks)
      );
      pause
    end



end

let main _ =
  let items_ul = "todo-list" @> CoerceTo.ul in
  let new_todo = "new-todo" @> CoerceTo.input in
  let _m_react = Controller.machine (items_ul, new_todo) in
  Js._false


let () = Dom_html.(window##.onload := handler main)

