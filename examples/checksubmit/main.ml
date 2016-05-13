
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

open Dom_html
let%sync machine =
  input (cb : inputElement Js.t);
  input (sub : buttonElement Js.t);
  input (field : inputElement Js.t);
  output reset;

  loop begin
    trap t begin (
        await (cb##onclick & cb##.checked = Js._true);
        await sub##onclick;
        !(Dom_html.window##alert (!!field)##.value);
        exit t;
      ) || loop (present (cb##onclick & cb##.checked = Js._false) (exit t); pause)
    end;
    emit reset () ; pause
  end


let main _ =
  let open Dom_html in
  let text_content = "checksubmit_content" @> CoerceTo.input in
  let checkbox_accept = "checksubmit_accept" @> CoerceTo.input in
  let request_button = "checksubmit_request" @> CoerceTo.button in
  let _ = machine (checkbox_accept, request_button, text_content, ()) in
  Js._false


let () = Dom_html.(window##.onload := handler main)
