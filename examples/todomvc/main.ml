
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

  open Tyxml_js
  (* let task_entry f = *)
  (*   let task_input = *)
  (*     Html5.(input ~a:[ *)
  (*         a_class ["new-todo"] ; *)
  (*         a_placeholder "What needs to be done?" ; *)
  (*         a_autofocus `Autofocus ; *)
  (*       ] ()) *)
  (*   in *)
  (*   Html5.(header ~a:[a_class ["header"]] [ *)
  (*       h1 [pcdata "todos"]; *)
  (*       task_input *)
  (*     ]) *)

  let create_item cnt delete_sig str =
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
    Pendulum.Machine.set_present_value delete_sig cnt;
    mli



    (* Html5.(div ~a:[a_class ["view"]] [ *)
    (*     input ~a:[a_class ["toggle"]; a_input_type `Checkbox ] (); *)
    (*     label [pcdata str]; *)
    (*     button ~a:[a_class ["destroy"]] [] *)
    (* ]) *)

  (* let add_item ul newit = *)
  (*   Dom.appendChild ul (create_item (Js.to_string (newit##.value))); *)
  (*   newit##.value := Js.string "" *)

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
  let%sync machine =
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
         emit add_item (View.create_item !!cnt delete_item newit##.value))
    ; pause end
    ||
    loop begin
      present add_item (
        !(Dom.appendChild !!items_ul !!add_item; newit##.value := Js.string "");
        emit tasks ((!!cnt, !!add_item) :: !!tasks)
      ); pause
    end


  let addifenter render set_add ev =
    if ev##.keyCode = 13 then begin
      Js.Opt.iter (Dom_html.CoerceTo.input (Dom.eventTarget ev))
        (fun x ->
           set_add (Js.to_string x##.value);
           x##.value := Js.string ""
        );
      View.raf render
    end;
    true

end

let main _ =
  let items_ul = "todo-list" @> CoerceTo.ul in
  let new_todo = "new-todo" @> CoerceTo.input in
  let _m_react = Controller.machine (items_ul, new_todo) in



  Js._false


let () = Dom_html.(window##.onload := handler main)

