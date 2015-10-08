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
    trap t begin
      begin
        await (checked & !!checked);
        await request;
        atom (print_endline !!request);
        exit t;
      end
      || loop (present (checked & !(!!checked)) (exit t); pause)
    end;
    emit reset () ; pause
  end

let main _ =
  let open Dom_html in
  let text_content = "content" @> CoerceTo.input in
  let checkbox_accept = "accept" @> CoerceTo.input in
  let request_button = "request" @> CoerceTo.button in
  let reset () =
    text_content##.value := Js.to_string "";
    checkbox_accept##.checked := Js.false
  in
  let set_checked, set_unchecked, set_request, get_reset, react =
    machine ((), (), "", reset)
  in
  Pendulum.Js.bind react (checkbox_accept##.onclick)
    (fun ev -> set_checked (checked checkbox_accept));

  Pendulum.Js.bind react (request_button##.onclick)
    (fun ev -> set_request (value text_content));

let () = Dom_html.(window##.onload := handler main)
