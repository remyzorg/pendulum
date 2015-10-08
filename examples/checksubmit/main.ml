
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


let%sync machine =
  input checked;
  input unchecked;
  input textcontent;
  input request;

  loop begin
    trap t begin
      begin
        await checked;
        await request;
        atom (
          debug "%s" (Js.to_string @@ (!!textcontent)##.value);
          (!!checked)##.checked := Js._false;
          (!!textcontent)##.value := Js.string "";
        )
      end
      ||
      loop (present unchecked (exit t); pause)
    end;
    pause
  end

let main _ =
  let open Dom_html in
  let text_content = "content" @> CoerceTo.input in
  let checkbox_accept = "accept" @> CoerceTo.input in
  let request_button = "request" @> CoerceTo.button in
  (* let c = "canvas" @> CoerceTo.canvas in *)
  (* let ctx = canvas##getContext (Dom_html._2d_) in *)

  let set_checked, set_unchecked, set_textcontent, set_request, react =
    machine (checkbox_accept, checkbox_accept, text_content, ())
  in
  set_textcontent (text_content);
  checkbox_accept##.onclick := handler (fun ev ->
      if Js.to_bool checkbox_accept##.checked then
        set_checked (checkbox_accept)
      else
        set_unchecked (checkbox_accept);
      ignore @@ react (); Js._true);

  request_button##.onclick := handler (fun ev ->
      set_request ();
      ignore @@ react (); Js._true);
  Js._false


let () = Dom_html.(window##.onload := handler main)
