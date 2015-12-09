
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

let onclick elt f = elt##.onclick := Dom_html.handler (fun ev -> f (); Js._true)
let checked elt = Js.to_bool elt##.checked
let value elt = Js.to_string elt##.value

let%sync machine =
  input checked, unchecked, request;
  output reset;
  loop begin
    trap t begin (
        await checked;
        await request;
        !(Dom_html.window##alert(Js.string !!request));
        exit t;
      ) || loop (present unchecked (exit t); pause)
    end;
    emit reset () ; pause
  end

let main _ =
  let open Dom_html in
  let text_content = "checksubmit_content" @> CoerceTo.input in
  let checkbox_accept = "checksubmit_accept" @> CoerceTo.input in
  let request_button = "checksubmit_request" @> CoerceTo.button in
  (* let c = "canvas" @> CoerceTo.canvas in *)
  (* let ctx = canvas##getContext (Dom_html._2d_) in *)

  let set_checked, set_unchecked, set_request, get_reset, react =
    machine ((), (), "", ())
  in
  checkbox_accept##.onclick := handler (fun ev ->
      if Js.to_bool checkbox_accept##.checked then
        set_checked ()
      else
        set_unchecked ();
      ignore @@ react (); Js._true);

  request_button##.onclick := handler (fun ev ->
      set_request (Js.to_string @@ text_content##.value);
      ignore @@ react (); Js._true);
  Js._false


let () = Dom_html.(window##.onload := handler main)
