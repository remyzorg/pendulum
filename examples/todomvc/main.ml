
open Firebug

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

module CoerceTo = struct
  include Dom_html.CoerceTo
end

module Dom = struct
  include Dom

  module Opt = struct
    include Js.Opt
    let mapopt o f =
      Js.Opt.case o
        (fun () -> Js.null)
        (fun o -> f o)
  end

  let firstChild n = n##.firstChild
  let firstChildOpt n = Opt.mapopt n firstChild
  let nextSibling n = n##.nextSibling
  let nextSiblingOpt n = Opt.mapopt n nextSibling
end

let (@>) s coerce =
  Js.Opt.get Dom_html.(coerce @@ getElementById s)
    (fun () -> error "can't find element %s" s)

let (@>>) s coerce =
  Js.Opt.get Dom_html.(Dom.Opt.mapopt s coerce)
    (fun () -> error "can't find element")


module Opt = struct
  let iter f opt =
    match opt with
    | None -> ()
    | Some o -> f o

  let default v f opt =
    match opt with
    | None -> v
    | Some o -> f o
end

let enter_pressed ev =
  Opt.default false (fun ev -> ev##.keyCode = 13) ev


module View = struct

  let create_item cnt animate delete_sig blur_sig dblclick_sig keydown_sig select_sig str =
    let mli = Dom_html.(createLi document) in
    let mdiv = Dom_html.(createDiv document)  in
    mdiv##.className := Js.string "view";
    let tgl = Dom_html.(createInput document) in
    tgl##.className := Js.string "toggle";
    tgl##setAttribute (Js.string "type")(Js.string "checkbox");
    let lbl = Dom_html.(createLabel document) in
    lbl##.textContent := Js.some str##trim;
    let btn = Dom_html.(createButton document) in
    btn##.className := Js.string "destroy";
    let ie = Dom_html.(createInput document) in
    ie##setAttribute (Js.string "type") (Js.string "text");
    ie##.className := Js.string "edit";
    ie##.value := str##trim;
    ie##.id := Js.string @@ Format.sprintf "it-edit-%d" cnt;
    ie##.onblur := Dom_html.handler (fun _ ->
        Pendulum.Machine.set_present_value blur_sig cnt; animate ();
        Js._true);

    let keyhandler = Dom_html.handler (fun ev ->
        (* if ev##.keyCode = 13 || ev##.keyCode = 27 then begin *)
          Pendulum.Machine.set_present_value keydown_sig (cnt, ev##.keyCode); animate ();
        (* end; *)
        Js._true)
    in
    ie##.onkeydown := keyhandler;
    ie##.onkeypress := keyhandler;
    tgl##.onclick := Dom_html.handler (fun _ ->
        Pendulum.Machine.set_present_value select_sig cnt; animate ();
        Js._true);
    lbl##.ondblclick := Dom_html.handler (fun _ ->
        Pendulum.Machine.set_present_value dblclick_sig cnt; animate ();
        Js._true);
    Dom.(appendChild mdiv tgl;
         appendChild mdiv lbl;
         appendChild mdiv btn;
         appendChild mli mdiv;
         appendChild mli ie);
    btn##.onclick := Dom_html.handler (fun _ ->
        Pendulum.Machine.set_present_value delete_sig cnt; animate ();
        Js._true) ;
    mli, ie, lbl, false

end


let jstr s = Js.some @@ Js.string ""

let is_eq_char k q = Js.Optdef.case k
    (fun _ -> false)
    (fun x -> x = Char.code q)

let get_remove h items_ul elt_id =
  try
    let elt, _, _, _ = Hashtbl.find h elt_id in
    Hashtbl.remove h elt_id;
    Dom.removeChild items_ul elt
  with Not_found -> ()

let add_append h cnt items_ul (mli, _, _, _ as added_item) =
  Hashtbl.add h cnt added_item;
  Dom.appendChild items_ul mli

let edited_item h id =
  try
    let (mli, edit, lbl, completed) = Hashtbl.find h id in
    if edit##.value##.length = 0 then begin
      Js.Opt.iter mli##.parentNode (fun p -> Dom.removeChild p mli);
      Hashtbl.remove h id
    end else begin
      lbl##.textContent := Js.some edit##.value;
      mli##.className := Js.string (if completed then "completed" else "");
    end
  with Not_found -> ()

let focus_iedit h id =
  try
    let (mli, edit, _, _) = Hashtbl.find h id in
    mli##.className := Js.string "editing";
    edit##focus
  with Not_found -> ()

let selected_item h id =
  try
    let (mli, edit, lbl, completed) = Hashtbl.find h id in
    mli##.className := Js.string (if completed then "" else "completed");
    Hashtbl.replace h id (mli, edit, lbl, not completed);
  with Not_found -> ()



let add_item_default =
  Dom_html.(createDiv document, createInput document, createLabel document, false)

module Controller = struct

  let%sync machine ~animate =
    input items_ul;
    input newit;
    let delete_item = -1 in
    let blur_item = -1 in
    let keydown_item = -1, 0 in
    let dblclick_item = -1 in
    let select_item = -1 in
    let add_item = add_item_default in
    let tasks = Hashtbl.create 19 in
    let cnt = 0 in
    loop (
      present keydown_item & (snd !!keydown_item = 13 || snd !!keydown_item = 27)
                             !(edited_item !!task (fst !!keydown_item)
      ||
      present select_item !(selected_item !!tasks !!select_item)
      ||
      present blur_item !(edited_item !!tasks !!blur_item)
      ||
      present add_item !(
        newit##.value := Js.string "";
        add_append !!tasks !!cnt !!items_ul !!add_item)
      ||
      present delete_item
        !(get_remove !!tasks !!items_ul !!delete_item)
    ; pause)
    ||
    loop (present dblclick_item !(focus_iedit !!tasks !!dblclick_item); pause)
    ||
    loop begin
      present (newit##onkeydown
               & enter_pressed !!(newit##onkeydown)
               && newit##.value##.length > 0)
        (emit cnt (!!cnt + 1);
         emit add_item
           (View.create_item !!cnt animate
              delete_item blur_item dblclick_item keydown_item select_item newit##.value));
      pause end

end

let main _ =
  let items_ul = "todo-list" @> CoerceTo.ul in
  let new_todo = "new-todo" @> CoerceTo.input in
  let _m_react = Controller.machine (items_ul, new_todo) in
  Js._false


let () = Dom_html.(window##.onload := handler main)

