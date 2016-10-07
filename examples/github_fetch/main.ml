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

  type file_ref = { url : string; name : string }

  type contents =
    | File of file_ref
    | Dir of string list
    | Content of file_ref * string
    | Undefined

  let token = ref ""
  
  let base = "https://api.github.com"
  let at = "?access_token="
  let github_url = "/repos"

  let repos user =
    Format.sprintf "%s/users/%s/repos?access_token=%s"
      base user !token

  let trees ?(sha="master") user repos =
    Format.sprintf
      "%s/repos/%s/%s/git/trees/%s?access_token=%s"
      base    user repos       sha           !token

  let contents user repos path =
    Format.sprintf
      "%s/repos/%s/%s/contents/%s?access_token=%s"
      base    user repos     path           !token

  let file s = s.url

  module Json = Yojson.Basic.Util

  module Extract = struct

    open Yojson.Basic
    open Yojson.Basic.Util


    let members str json =
      [json] |> flatten |> filter_member str |> filter_string

    let paths = members "path"
    let names = members "name"

    let repos s = Dir (names @@ from_string s)

    let trees s =
      let json = from_string s in
      Dir (json |> member "tree" |> paths)

    let file file s = Content (file, s)

    let contents s =
      let json = Yojson.Basic.from_string s in
      match json with
      | `List _ -> Dir (names json)
      | `Assoc _ ->
        let url = json |> member "download_url" |> to_string in
        let name = json |> member "name" |> to_string in
        File { url; name }
      | _ -> debug "undefined"; Undefined
  end

  let mk_req react signal extract =
    let rq = XmlHttpRequest.create () in
    rq##.onreadystatechange := Js.wrap_callback (fun _ ->
        let open XmlHttpRequest in
        begin match rq##.readyState with
          | DONE when rq##.status == 200 ->
            begin try
                let res = extract @@ Js.to_string rq##.responseText in
                Signal.set_present_value signal res; react ()
              with _ -> debug "parsing error"
            end
          | DONE ->
            Signal.set_present_value signal Undefined; react ()
          | _ -> ()
        end); rq

  let get rq url =
    rq##_open (Js.string "GET") url Js._true;
    rq##send Js.null

end

let get_repos react req_sig name =
  let open Github_api in
  let rq = mk_req react req_sig Extract.repos in
  get rq (Js.string (repos name))

let get_trees react req_sig user repo =
  let open Github_api in
  let rq = mk_req react req_sig Extract.trees in
  get rq (Js.string @@ trees user repo)

let get_contents react req_sig user repo path =
  let open Github_api in
  let rq = mk_req react req_sig Extract.contents in
  get rq (Js.string @@ contents user repo path)

let get_file react req_sig f =
  let open Github_api in
  let rq = mk_req react req_sig (Extract.file f) in
  get rq (Js.string @@ file f)

let extract_infos animate req s =
  let path = Re.(split (compile @@ str "/") (Js.to_string s)) in
  match path with
  | [] -> ()
  | [user] ->
    get_repos animate req user
  | [user; repo] ->
    (* debug "trees : %s %s" user repo *)
    get_trees animate req user repo
  | user :: repo :: path ->
    (* debug "contents : %s %s '%s'" user repo (String.concat "/" path) *)
    get_contents animate req user repo (String.concat "/" path)

let insert_dom animate req repolist =
  let open Html5 in
  let result_div =
    let open Github_api in
    match req.Pendulum.Signal.value with
    | Undefined -> (); div []
    | Content (fr, s) ->
      div [pre [code ~a:[a_class ["OCaml"]] [pcdata s]]]
    | File f -> get_file animate req f; div []
    | Dir names ->
      div @@ List.map (fun name ->
          div ~a:[a_onclick (fun x -> false)]
            [pcdata name]
        ) names
  in
  Js.Opt.iter (repolist##.firstChild)
    (Dom.replaceChild repolist (To_dom.of_element result_div));
  let hljs = (Js.Unsafe.js_expr "hljs") in
  hljs##highlightBlock repolist

let%sync github_fetch_sync ~animate =
  input namefield;
  element repos;
  input request_result;

  loop begin
    present namefield##onkeyup (
      !(extract_infos animate request_result namefield##.value)
    ); pause
  end
  ||
  loop (
    trap reset (
      loop (
        present request_result (
          !(insert_dom animate request_result repos);
          exit reset
        )
      ; pause)
      ||
      loop (present namefield##onkeyup (exit reset) ; pause)
    )
    ; pause)

(* remyzorg/pendulum/src/preproc/grc.ml *)

let run _ =
  let url_args = Url.Current.arguments in
  try
    let username_field = get CoerceTo.input "github_username" in
    let repos = getElementById "github_repos" in
    begin try
        let token = List.assoc "access_token" url_args in
        debug "%s" token;
        Github_api.token := token
      with Not_found -> ()
    end;

    let syncfetcher = github_fetch_sync#create
        (username_field, repos, Github_api.Undefined)
    in



    Js._true
  with
  | Not_found -> Js._true




let () = window##.onload := handler run
