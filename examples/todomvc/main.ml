
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
  let task_entry f =
    let task_input =
      Html5.(input ~a:[
          a_class ["new-todo"] ;
          a_placeholder "What needs to be done?" ;
          a_autofocus `Autofocus ;
          a_onkeypress f
        ] ())
    in
    Html5.(header ~a:[a_class ["header"]] [
        h1 [pcdata "todos"];
        task_input
      ])

  let create_item str =
    Html5.(div ~a:[a_class ["view"]] [
        input ~a:[a_class ["toggle"]; a_input_type `Checkbox ] ();
        label [pcdata str];
        button ~a:[a_class ["destroy"]] []
    ])

end


module Controller = struct

  let%sync machine =
    input items_ul;
    input add_item;
    input remove_item;
    input toggle_item;
    input edit_item;
    input toggle_all;

    let items = [] in
    loop begin
      present add_item begin
        atom (debug "%s" !!add_item);
        emit items (View.create_item !!add_item :: !!items);
      end;
      pause
    end
    ||
    loop begin
      pause
    end
    ||
    loop begin
      pause
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
  let _doc = Dom_html.document in
  let parent = "todomvc" @> CoerceTo.div in
  let items_ul = "todo-list" @> CoerceTo.ul in
  (* let new_todo = "new-todo" @> CoerceTo.input in *)
  let dummy_li = View.create_item "" in

  let set_ul, set_add_item, set_toggle,
      set_remove, edit_item, set_toggle_all, m_react =
    Controller.machine (items_ul, "", dummy_li, dummy_li, dummy_li, ()) in

  let task_entry = View.task_entry (Controller.addifenter m_react set_add_item) in
  Dom.appendChild parent (Tyxml_js.To_dom.of_header task_entry);


  (* task_entry##.onkeypress := Dom_html.handler  *)
  (* set_ul items_ul; *)
  (* ignore (m_react ()); *)


  Js._false


let () = Dom_html.(window##.onload := handler main)

