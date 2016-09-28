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

module Github_api = struct

  let token = ref ""
  
  let base = "https://api.github.com"
  let at = "?access_token="
  let github_url = "/repos"

  let repo_list user =
    Format.sprintf "%s/users/%s/repos?access_token=%s"
      base user !token


end

module Json = Yojson.Basic.Util

let insert_dom repolist repos =
  let open Html5 in
  let repos_div = div @@ List.map (fun repo ->
      div ~a:[a_onclick (fun x -> false)] [
        pcdata repo
      ]
    ) repos
  in
  Js.Opt.iter (repolist##.firstChild)
    (Dom.replaceChild repolist (To_dom.of_element repos_div))

let create_open react err_sig name =
  let rq = XmlHttpRequest.create () in
  let url = Github_api.repo_list (Js.to_string name) in
  rq##.onreadystatechange := Js.wrap_callback (fun _ ->
      let open XmlHttpRequest in
      begin match rq##.readyState with
        | DONE when rq##.status == 200 ->
          let res = Js.to_string rq##.responseText in
          let json = Yojson.Basic.from_string res in
          let repos = Json.(
              [json]
              |> flatten
              |> filter_member "full_name"
              |> filter_string)
          in
          Signal.set_present_value err_sig repos; react ()
        | DONE ->
          Signal.set_present_value err_sig []; react ()
        | _ -> ()
      end);
  let () = rq##_open (Js.string "GET") (Js.string url) Js._true in
  rq##send Js.null; ()


let%sync github_fetch_sync ~animate =
  element namefield;
  element repos;
  input request_result;

  loop begin
    present namefield##onkeyup (
      !(create_open animate request_result namefield##.value)
    ); pause
  end
  ||
  loop (
    trap reset (
      loop (
        present request_result (
          !(insert_dom repos !!request_result);
          exit reset
        )
      ; pause)
      ||
      loop (present namefield##onkeyup (exit reset) ; pause)
    )
    ; pause
  )



let run _ =
  let url_args = Url.Current.arguments in
  try
    let username_field = get CoerceTo.input "github_username" in
    let repos = getElementById "github_repos" in
    begin try
        let token = List.assoc "access_token" url_args in
        debug "%s" token;
        Github_api.token := token
        (* username_field##.value := Js.string user; *)
        (* run_fetch user *)
      with Not_found -> ()
    end;

    let syncfetcher = github_fetch_sync#create
        (username_field, repos, []) in



    Js._true
  with
  | Not_found -> Js._true




let () = window##.onload := handler run
