let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log (Js.string s)) f

let get coerce s =
  let elt = coerce @@ Dom_html.getElementById s in
  Js.Opt.case elt (fun () -> raise Not_found) (fun x -> x)

open Dom_html
open Lwt.Infix
open Tyxml_js

open Pendulum

let model : (string, (bool * string) list) Hashtbl.t = Hashtbl.create 45

let add_model repo (files : string list) =
  Hashtbl.add model repo []

open Github

let extract_infos animate req s =
  let open Signal in
  let path = Re.(split (compile @@ str "/") (Js.to_string s)) in
  let cb res = set_present_value req res; animate () in
  let cberr () = set_present_value req Api.Undefined; animate () in
  match path with
  | [] -> ()
  | [user] -> get_repos user cb cberr
  | [user; repo] -> get_trees user repo cb cberr
  | user :: repo :: path ->
    get_contents user repo (String.concat "/" path) cb cberr

let highlight (elt : Dom_html.element Js.t) : unit =
  let hljs = (Js.Unsafe.js_expr "hljs") in
  let () = hljs##highlightBlock elt in ()

let insert_dom animate req click_sig repolist =
  let open Html5 in
  let cb res = Signal.set_present_value req res; animate () in
  let cberr () = Signal.set_present_value req Api.Undefined; animate () in
  let replace_div result_div =
    Js.Opt.iter (repolist##.firstChild)
      (Dom.replaceChild repolist (To_dom.of_element result_div))
  in
  let open Api in
  match req.Pendulum.Signal.value with
  | Undefined -> ()
    (* replace_div (div []) *)
  | Content (fr, s) ->
    replace_div @@
    div [pre [code ~a:[a_class ["OCaml"]] [pcdata s]]];
    highlight repolist
  | File f -> get_file f cb cberr; replace_div (div [])
  | Dir files ->
    replace_div @@ div @@ List.map (fun file ->
        div ~a:[
          a_onclick (fun e ->
              Pendulum.Signal.set_present_value click_sig file.name;
              animate ();
              false);
        ] [ pcdata file.name ]
      ) files

let%sync github_fetch_sync ~print:pdf ~animate =
  element namefield;
  element repos;
  input request_result;
  input clickitem;

  loop begin
    present clickitem
      !(debug "%s" !!clickitem);
    pause
  end
  ||
  loop begin
    present namefield##onkeyup (
      !(extract_infos animate request_result namefield##.value)
    ); pause
  end
  ||
  loop begin
    trap reset (
      loop (
        present request_result (
          !(insert_dom animate request_result clickitem repos);
          exit reset
        )
      ; pause)
      ||
      loop (present namefield##onkeyup (exit reset) ; pause)
    );
    pause
  end

(* remyzorg/pendulum/src/preproc/grc.ml *)

let run _ =
  let url_args = Url.Current.arguments in
  try
    let username_field = get CoerceTo.input "github_username" in
    let repos = getElementById "github_repos" in
    begin try
        let token = List.assoc "access_token" url_args in
        debug "%s" token;
        Api.token := token
      with Not_found -> ()
    end;

    let _syncfetcher = github_fetch_sync#create
        (username_field, repos, Api.Undefined, "")
    in



    Js._true
  with
  | Not_found -> Js._true




let () = window##.onload := handler run
