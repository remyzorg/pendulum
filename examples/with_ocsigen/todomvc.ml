

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

module Storage = struct

  open Js

  let storage =
    Optdef.case Dom_html.window##.localStorage
      (fun () -> failwith "Storage is not supported by this browser")
      (fun v -> v)

  let key = string "jsoo-todo-state"

  let clean_all () = storage##removeItem key

  let find () =
    let r = storage##getItem key in
    Opt.to_option @@ Opt.map r to_string

  let set v = storage##setItem key (string v)

  let init default = match find () with
    | None -> set default ; default
    | Some v -> v

end

module Model = struct

  open Dom_html

  type item = {
    mutable txt : string;
    mutable item_li : divElement Js.t;
    mutable edit : inputElement Js.t;
    mutable lbl : labelElement Js.t;
    mutable selected : bool;
  }

  type doubly_linked = {
    prev : doubly_linked;
    value : item;
    next : doubly_linked
  }

  let rec item_to_json k buf { txt; selected } =
    ((let open! Ppx_deriving_runtime in
       Buffer.add_string buf "[0,";
       Deriving_Json.Json_int.write buf k;
       ((Buffer.add_string buf ",";
         Deriving_Json.Json_string.write buf txt);
        Buffer.add_string buf ",";
        Deriving_Json.Json_bool.write buf selected);
       Buffer.add_string buf "]")[@ocaml.warning "-A"])

  type extracted_item = (int * string * bool) list [@@deriving json]

  let add_item_default =
    Dom_html.({
        txt = "";
        item_li = createDiv document;
        edit = createInput document;
        lbl = createLabel document;
        selected = false;
      })

  let read h cnt add_item create_item =
    match Storage.find () with
    | None -> 0
    | Some v ->
      let cntleft = ref 0 in
      let cntmax = ref 0 in
      let l = Deriving_Json.from_string [%derive.json: extracted_item] v in
      List.iter (fun ((k, str, sel) : int * string * bool) ->
          if not sel then incr cntleft;
          cntmax := max !cntmax k;
          add_item k @@ create_item k (Js.string str) sel;
        ) @@ List.sort (fun (k1, _, _) (k2, _, _) -> compare k1 k2) l;
      Pendulum.Machine.setval cnt !cntmax;
      !cntleft


  let write h =
    let b = Buffer.create 100 in
    let size = Hashtbl.length h in
    if size > 0 then begin
      Hashtbl.iter (fun k i -> Printf.bprintf b "[0,%a," (item_to_json k) i;) h;
      Buffer.add_char b '0';
      for _i = size downto 1 do
        Buffer.add_char b ']'
      done;
    end else
      Buffer.add_char b '0';
    let s = Buffer.contents b in
    Storage.set s


end

module View = struct

  open Tyxml_js
  open Model

  let style_of_visibility state = function
    | None -> "block"
    | Some false -> if state then "none" else "block"
    | Some true -> if state then "block" else "none"

  let create_item
      animate delete_sig blur_sig dblclick_sig
      keydown_sig select_sig visibility cnt str selected
    =
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
        a_onblur (fun _ -> Machine.set_present_value blur_sig cnt;
                   animate (); true);
        a_onkeydown keyhandler; a_onkeypress keyhandler
      ] ()
    in
    let tgl_done =
      let a = [
        a_input_type `Checkbox; a_class ["toggle"];
        a_onclick (fun _ ->
            Pendulum.Machine.set_present_value select_sig cnt; animate (); true)
      ] in
      let a = if selected then (a_checked `Checked) :: a else a
      in input ~a ()
    in
    let mdiv = div ~a:[a_class ["view"]] [tgl_done; lbl_content; btn_rm] in
    let item_li = To_dom.of_li @@ li [mdiv; input_edit_item] in
    item_li##.className := Js.string (if selected then "completed" else "");
    item_li##.style##.display := Js.string @@ style_of_visibility false visibility;
    To_dom.({txt; item_li; edit = of_input input_edit_item;
             lbl = of_label lbl_content; selected})

  let add_append h items_ul cnt it =
    Dom.appendChild items_ul it.item_li;
    Hashtbl.add h cnt it

  let create_items cnt tasks animate items_ul delete_sig blur_sig
      dblclick_sig keydown_sig select_sig strs selected =
    let open Pendulum.Machine in
    let nb = List.fold_left (fun acc str ->
        let cnt = cnt.value + acc + 1 in
        let it = create_item animate delete_sig blur_sig dblclick_sig
            keydown_sig select_sig None cnt str selected
        in add_append tasks items_ul cnt it; acc + 1
      ) 0 strs
    in
    Pendulum.Machine.set_present_value cnt (!!cnt + nb);
    nb


  let jstr s = Js.some @@ Js.string ""

  let is_eq_char k q = Js.Optdef.case k
      (fun _ -> false)
      (fun x -> x = Char.code q)

  let remove_items h items_ul ids =
    List.fold_left (fun acc id ->
        try
          let it = Hashtbl.find h id in
          Hashtbl.remove h id;
          Dom.removeChild items_ul it.item_li;
          acc + if it.selected then 0 else 1
        with Not_found -> acc
      ) 0 ids

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

  let select_items h ids =
    List.fold_left (fun acc id ->
        try
          let it = Hashtbl.find h id in
          it.item_li##.className := Js.string (if it.selected then "" else "completed");
          it.selected <- not it.selected;
          if not it.selected then acc + 1 else acc -1;
        with Not_found -> acc
      ) 0 ids

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

  let%sync machine ~animate =
    input items_ul;
    input newit {
      onkeydown = [], fun acc ev ->
          if ev##.keyCode = 13 && newit##.value##.length > 0
          then newit##.value :: acc else acc
    };

    input (itemcnt : element Js.t);
    input clear_complete, select_all;
    input selected_items (fun l id -> id :: l);
    input deleted_items (fun l id -> id :: l);
    input all, completed, active, removestorage;

    let blur_item = 0 in
    let keydown_item = 0, 0 in
    let dblclick_item = 0 in
    let cnt = 0 in
    let cntleft = 0 in
    let write = () in
    let visibility = None in
    let tasks = Hashtbl.create 19 in

    emit cntleft (Model.read !!tasks cnt
        (View.add_append !!tasks !!items_ul)
        (View.create_item animate
           deleted_items blur_item dblclick_item keydown_item
           selected_items !!visibility));

    loop (
      present removestorage##onclick !(Storage.clean_all ())

      || present (keydown_item & (snd !!keydown_item = 13 || snd !!keydown_item = 27)) (
        !(View.edited_item !!tasks (fst !!keydown_item));
        emit write;
      )

      || present selected_items (
        emit cntleft (!!cntleft + View.select_items !!tasks !!selected_items);
        emit write
      )

      || present blur_item !(View.edited_item !!tasks !!blur_item)

      || present deleted_items (
        emit cntleft (!!cntleft - View.remove_items !!tasks !!items_ul !!deleted_items);
        emit write
      )

      || present dblclick_item !(View.focus_iedit !!tasks !!dblclick_item)


      || present select_all##onclick (
        emit cntleft (View.select_all !!cntleft !!tasks);
        emit write;
      )

      || present clear_complete##onclick (
        !(View.clear_complete !!items_ul !!tasks);
        emit cntleft (!!cntleft);
        emit write;
      )

      || present all##onclick (emit visibility None)
      || present completed##onclick (emit visibility (Some true))
      || present active##onclick (emit visibility (Some false))

      || present visibility !(View.change_visiblity !!tasks
                                (all, completed, active) !!visibility)

      || present (newit##onkeydown & !!(newit##onkeydown) <> []) (
        emit cntleft
          (!!cntleft + View.create_items cnt !!tasks animate !!items_ul
             deleted_items blur_item dblclick_item keydown_item
             selected_items !!(newit##onkeydown) false);
        emit newit##.value (Js.string "");
        emit write;
      )

      || present cntleft !(View.items_left !!tasks !!itemcnt
                             !!cntleft clear_complete select_all)

      || present write !(Model.write !!tasks);
      pause
    )

end

