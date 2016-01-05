
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







module View = struct

  let raf f =
    ignore @@ Dom_html.window##requestAnimationFrame
      (Js.wrap_callback @@ fun _ -> ignore @@ f (); ())

  let create_item cnt animate delete_sig blur_sig dblclick_sig str =
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
        debug "lost focus";
        Js._true);
    lbl##.ondblclick := Dom_html.handler (fun _ ->
        Pendulum.Machine.set_present_value dblclick_sig cnt; animate ();
        Js._true);
    Dom.(appendChild mdiv tgl;
         appendChild mdiv lbl;
         appendChild mdiv btn;
         appendChild mli mdiv;
         appendChild mli ie);
    btn##.onclick := Dom_html.handler (fun _  ->
        Pendulum.Machine.set_present_value delete_sig cnt; animate ();
        Js._true) ;
    mli, ie, lbl

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


let get_remove h items_ul elt_id =
  try
    let elt, _, _ = Hashtbl.find h elt_id in
    Hashtbl.remove h elt_id;
    Dom.removeChild items_ul elt
  with Not_found -> ()

let add_append h cnt items_ul (mli, _, _ as added_item) =
  Hashtbl.add h cnt added_item;
  Dom.appendChild items_ul mli


let left_item h id =
  try
    let (mli, edit, lbl) = Hashtbl.find h id in
    mli##.className := Js.string "";
    lbl##.textContent := Js.some edit##.value
  with Not_found -> ()

let focus_iedit h id =
  try
    let (mli, edit, _) = Hashtbl.find h id in
    debug "Ok found it : %s" (Js.to_string edit##.id);
    mli##.className := Js.string "editing";
    edit##focus
  with Not_found -> ()


module Controller = struct
  let%sync machine ~animate =
    input items_ul;
    input newit;

    let delete_item = -1 in
    let blur_item = -1 in
    let dblclick_item = -1 in
    let add_item =
      Dom_html.(createDiv document, createInput document, createLabel document)
    in
    let tasks = Hashtbl.create 19 in
    let cnt = 0 in
    loop (present blur_item !(left_item !!tasks !!blur_item); pause)
    || loop (present dblclick_item !(focus_iedit !!tasks !!dblclick_item); pause)
    || loop begin
      present (newit##onkeypress
               & enter_pressed !!(newit##onkeypress)
               && newit##.value <> Js.string "")
        (emit cnt (!!cnt + 1);
         emit add_item
           (View.create_item !!cnt animate
              delete_item blur_item dblclick_item newit##.value));
      pause end
    ||
    loop begin
      present add_item !(
        newit##.value := Js.string "";
        add_append !!tasks !!cnt !!items_ul !!add_item
      ); pause
    end
    ||
    loop begin
      present delete_item
        !(get_remove !!tasks !!items_ul !!delete_item);
      pause
    end



end

let main _ =
  let items_ul = "todo-list" @> CoerceTo.ul in
  let new_todo = "new-todo" @> CoerceTo.input in
  let _m_react = Controller.machine (items_ul, new_todo) in
  Js._false


let () = Dom_html.(window##.onload := handler main)

