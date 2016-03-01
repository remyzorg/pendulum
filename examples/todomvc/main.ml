
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


module Model = struct

  open Dom_html
  type item = {
    mutable txt : string;
    item_li : divElement Js.t;
    edit : inputElement Js.t;
    lbl : labelElement Js.t;
    mutable selected : bool;
  }


  let add_item_default =
    Dom_html.({
        txt = "";
        item_li = createDiv document;
        edit = createInput document;
        lbl = createLabel document;
        selected = false;
      })

end

module View = struct

  open Tyxml_js
  open Model

  let style_of_visibility state = function
    | None -> "block"
    | Some false -> if state then "none" else "block"
    | Some true -> if state then "block" else "none"

  let create_item cnt animate delete_sig blur_sig dblclick_sig keydown_sig select_sig visibility str =
    let open Html5 in
    let open Pendulum in
    let lbl_content = label ~a:[a_ondblclick (fun evt ->
        Machine.set_present_value dblclick_sig cnt; animate (); true)]
        [pcdata @@ Js.to_string (str##trim)]
    in
    let btn_rm = button ~a:[a_class ["destroy"]; a_onclick (fun evt ->
        Machine.set_present_value delete_sig cnt; animate (); true)] []
    in
    let keyhandler ev =
      Machine.set_present_value keydown_sig (cnt, ev##.keyCode);
      animate (); true
    in
    let txt = Js.to_string @@ str##trim in
    let input_edit_item = input ~a:[
        a_input_type `Text; a_class ["edit"];
        a_value txt;
        a_id (Format.sprintf "it-edit-%d" cnt);
        a_onblur (fun _ -> Machine.set_present_value blur_sig cnt; animate (); true);
        a_onkeydown keyhandler; a_onkeypress keyhandler
      ] ()
    in
    let tgl_done = input ~a:[
        a_input_type `Checkbox; a_class ["toggle"];
        a_onclick (fun _ ->
            Pendulum.Machine.set_present_value select_sig cnt; animate (); true)
      ] ()
    in
    let mdiv = div ~a:[a_class ["view"]] [tgl_done; lbl_content; btn_rm] in
    let item_li = To_dom.of_li @@ li [mdiv; input_edit_item] in
    item_li##.style##.display := Js.string @@ style_of_visibility false visibility;
    To_dom.({txt; item_li; edit = of_input input_edit_item;
                            lbl = of_label lbl_content; selected = false})

  let jstr s = Js.some @@ Js.string ""

  let is_eq_char k q = Js.Optdef.case k
      (fun _ -> false)
      (fun x -> x = Char.code q)

  let get_remove h items_ul elt_id =
    try
      let it = Hashtbl.find h elt_id in
      Hashtbl.remove h elt_id;
      Dom.removeChild items_ul it.item_li;
      if it.selected then 0 else 1
    with Not_found -> 0

  let add_append h cnt items_ul it =
    Hashtbl.add h cnt it;
    Dom.appendChild items_ul it.item_li

  let edited_item h id =
    try
      let it = Hashtbl.find h id in
      if it.edit##.value##.length = 0 then begin
        Js.Opt.iter it.item_li##.parentNode (fun p -> Dom.removeChild p it.item_li);
        Hashtbl.remove h id
      end else begin
        it.lbl##.textContent := Js.some it.edit##.value;
        it.item_li##.className := Js.string (if it.selected then "completed" else "");
        it.txt <- Js.to_string it.edit##.value;
      end
    with Not_found -> ()

  let focus_iedit h id =
    try
      let it = Hashtbl.find h id in
      it.item_li##.className := Js.string "editing";
      it.edit##focus
    with Not_found -> ()

  let selected_item h id =
    try
      let it = Hashtbl.find h id in
      it.item_li##.className := Js.string (if it.selected then "" else "completed");
      it.selected <- not it.selected;
      if not it.selected then 1 else -1
    with Not_found -> 0

  let items_left h footer left_cnt clear_complete select_all =
    let sp =
      Html5.(span ~a:[a_class ["todo-count"]] [
        strong ~a:[] [pcdata @@ Format.sprintf "%d" left_cnt];
        pcdata (Format.sprintf " item%s left" (if left_cnt > 1 then "s" else ""))])
    in
    let size = Hashtbl.length h in
    footer##.style##.display := Js.string @@ if size = 0 then "none" else "block";
    clear_complete##.style##.display :=
      Js.string @@ if size = left_cnt then "none" else "block";
    select_all##.checked := Js.bool (size > 0 && left_cnt = 0);
    select_all##.style##.display :=
      Js.string @@ if size > 0 then "block" else "none";
    Js.Opt.iter (footer##.firstChild)
      (Dom.replaceChild footer (To_dom.of_element sp))

  let clear_complete items_ul h =
    Hashtbl.iter (fun k it ->
        if it.selected then begin
          Dom.removeChild items_ul it.item_li;
          Hashtbl.remove h k
        end) h

  let select_all cnt_left h =
    let selected = cnt_left > 0 in
    Hashtbl.iter (fun k it ->
        it.selected <- selected;
        let toggler = Dom.Opt.mapopt (Dom.firstChildOpt (Dom.firstChild it.item_li))
            CoerceTo.element @>> CoerceTo.input
        in
        toggler##.checked := Js.bool selected;
        it.item_li##.className := Js.string @@ if selected then "completed" else ""
      ) h;
    if cnt_left = 0 then Hashtbl.length h else 0

  let change_visiblity h (all, completed, active) v =
    debug "when";
    let set (s1, s2, s3) =
      all##.className := Js.string s1;
      completed##.className := Js.string s2;
      active##.className := Js.string s3;
    in
    begin match v with
    | None -> set ("selected", "", "")
    | Some true -> set ("", "selected", "")
    | Some false -> set ("", "", "selected")
    end;
    Hashtbl.iter (fun _ it ->
        it.item_li##.style##.display := Js.string @@ style_of_visibility it.selected v
      ) h

end

module Controller = struct
  open Dom_html

  let%sync machine ~animate
      (items_ul : element Js.t)
      (newit : inputElement Js.t)
      (itemcnt : element Js.t)
      clear_complete select_all
      all completed active
    =
    let delete_item = 0 in let blur_item = 0 in
    let keydown_item = 0, 0 in let dblclick_item = 0 in
    let select_item = 0 in
    let add_item = Model.add_item_default in
    let tasks = Hashtbl.create 19 in
    let cnt = 0 in
    let cntleft = 0 in
    let write = () in
    let visibility = None in
    loop (
      present (keydown_item & (snd !!keydown_item = 13 || snd !!keydown_item = 27))
        !(View.edited_item !!tasks (fst !!keydown_item))

      || present select_item
        (emit cntleft (!!cntleft + View.selected_item !!tasks !!select_item))

      || present blur_item !(View.edited_item !!tasks !!blur_item)

      || present add_item !(
        newit##.value := Js.string "";
        View.add_append !!tasks !!cnt !!items_ul !!add_item)

      || present delete_item (
        emit cntleft (!!cntleft - View.get_remove !!tasks !!items_ul !!delete_item);
      )

      || present dblclick_item !(View.focus_iedit !!tasks !!dblclick_item)


      || present select_all##onclick (
        emit cntleft (View.select_all !!cntleft !!tasks)
      )

      || present clear_complete##onclick (
        !(View.clear_complete !!items_ul !!tasks);
        emit cntleft (!!cntleft)
      )

      || present all##onclick (emit visibility None)
      || present completed##onclick (emit visibility (Some true))
      || present active##onclick (emit visibility (Some false))

      || present visibility !(View.change_visiblity !!tasks
                                (all, completed, active) !!visibility)

      || present (newit##onkeydown
                  & enter_pressed !!(newit##onkeydown)
                  && newit##.value##.length > 0)
        (emit cnt (!!cnt + 1);
         emit cntleft (!!cntleft + 1);
         emit add_item
           (View.create_item !!cnt animate
              delete_item blur_item dblclick_item keydown_item select_item !!visibility newit##.value))

      || present cntleft
              !(View.items_left !!tasks
                  !!itemcnt !!cntleft clear_complete select_all)
    ; pause)

end

let main _ =
  let items_ul = "todo-list" @> CoerceTo.ul in
  let new_todo = "new-todo" @> CoerceTo.input in
  let filter_footer = "filter_footer" @> CoerceTo.element in
  let clear_complete = "clear_complete" @> CoerceTo.button in
  let select_all = "select_all" @> CoerceTo.input in
  let visibility_active = "visibility_active" @> CoerceTo.a in
  let visibility_all = "visibility_all" @> CoerceTo.a in
  let visibility_completed = "visibility_completed" @> CoerceTo.a in

  let _m_react = Controller.machine
      (items_ul, new_todo, filter_footer,
       clear_complete, select_all, visibility_all,
       visibility_completed, visibility_active)
  in Js._false


let () = Dom_html.(window##.onload := handler main)

