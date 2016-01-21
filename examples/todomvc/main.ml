
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

  open Tyxml_js

  let create_item cnt animate delete_sig blur_sig dblclick_sig keydown_sig select_sig str =
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
    let input_edit_item = input ~a:[
        a_input_type `Text; a_class ["edit"];
        a_value (Js.to_string @@ str##trim);
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
    let mli = li [mdiv; input_edit_item] in
    To_dom.(of_li mli, of_input input_edit_item, of_label lbl_content, false)

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
      if completed then 1 else -1
    with Not_found -> 0

  let items_left footer left_cnt =
    let sp =
      let open Html5 in
      span ~a:[a_class ["todo-count"]] [
        strong ~a:[] [pcdata @@ Format.sprintf "%d" left_cnt];
        pcdata (Format.sprintf " item%s left" (if left_cnt > 1 then "s" else ""))
      ]
    in
    footer##.style##.display := Js.string @@ if left_cnt = 0 then "none" else "block";
    Js.Opt.case (footer##.firstChild)
      (fun () -> debug "wtf bbq ?")
      (Dom.replaceChild footer (To_dom.of_element sp))



end




let add_item_default =
  Dom_html.(createDiv document, createInput document, createLabel document, false)

module Controller = struct

  let%sync machine ~animate =
    input items_ul;
    input newit;
    input itemcnt;

    let delete_item = 0 in let blur_item = 0 in
    let keydown_item = 0, 0 in let dblclick_item = 0 in
    let select_item = 0 in
    let add_item = add_item_default in
    let tasks = Hashtbl.create 19 in
    let cnt = 0 in
    let cntleft = 0 in
    let write = () in
    loop (
      present (keydown_item & (snd !!keydown_item = 13 || snd !!keydown_item = 27))
        !(View.edited_item !!tasks (fst !!keydown_item))

      || present select_item
        (emit cntleft (!!cnt + View.selected_item !!tasks !!select_item))

      || present blur_item !(View.edited_item !!tasks !!blur_item)

      || present add_item !(
        newit##.value := Js.string "";
        View.add_append !!tasks !!cnt !!items_ul !!add_item)

      || present delete_item (
        emit cntleft (!!cntleft - 1);
        !(View.get_remove !!tasks !!items_ul !!delete_item)
      )

      || present dblclick_item !(View.focus_iedit !!tasks !!dblclick_item)

      || present (newit##onkeydown
                  & enter_pressed !!(newit##onkeydown)
                  && newit##.value##.length > 0)
        (emit cnt (!!cnt + 1);
         emit cntleft (!!cntleft + 1);
         emit add_item
           (View.create_item !!cnt animate
              delete_item blur_item dblclick_item keydown_item select_item newit##.value))
      ;
      emit write ();
      pause;
    )
    ||
    loop (
      present cntleft !(View.items_left !!itemcnt !!cntleft);
      pause
    )


end

let main _ =
  let items_ul = "todo-list" @> CoerceTo.ul in
  let new_todo = "new-todo" @> CoerceTo.input in
  let filter_footer = "filter_footer" @> CoerceTo.element in
  let _m_react = Controller.machine (items_ul, new_todo, filter_footer) in
  Js._false


let () = Dom_html.(window##.onload := handler main)

