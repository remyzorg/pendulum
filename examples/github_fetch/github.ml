
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log (Js.string s)) f

module Api = struct

  type ftype = Subdir of string | Blob of string
  type file_ref = { ty : ftype; name : string }
  let mk_file (name, ty) = {name; ty}

  type contents =
    | File of file_ref
    | Dir of file_ref list
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

  let file s = match s.ty with Blob s -> s | Subdir url -> url

  module Json = Yojson.Basic.Util

  module Extract = struct

    open Yojson.Basic
    open Yojson.Basic.Util


    let members str json =
      [json] |> flatten |> filter_member str |> filter_string

    let paths = members "path"
    let names = members "name"
    let urls = members "download_url"
    let types = members "type"

    (* let repos s = Dir (names @@ from_string s) *)

    let repos s =
      let json = from_string s in
      Dir List.(
        combine (names json) (members "html_url" json)
        |> map (fun (name, url) -> mk_file (name, Subdir url)))

    let trees s =
      let json = from_string s in
      let tree = json |> member "tree" in
      Dir List.(
          combine (paths tree)
            (combine (urls tree) (types tree))
          |> map (fun (name, (url, type_)) ->
              {name; ty = if type_ = "blob" then
                         Blob url else Subdir url }
            )
        )

    let file file s = Content (file, s)

    let contents s =
      let json = Yojson.Basic.from_string s in
      match json with
      | `List _ ->
        Dir List.(
            combine (paths json)
              (combine (urls json) (types json))
            |> map (fun (name, (url, type_)) ->
                {name; ty = if type_ = "file" then
                           Blob url else Subdir url }
              )
          )
      | `Assoc _ ->
        let url = json |> member "download_url" |> to_string in
        let name = json |> member "name" |> to_string in
        File { ty = Blob url; name }
      | _ -> debug "undefined"; Undefined
  end

  let mk_req extract callback cberr =
    let rq = XmlHttpRequest.create () in
    rq##.onreadystatechange := Js.wrap_callback (fun _ ->
        let open XmlHttpRequest in
        begin match rq##.readyState with
          | DONE when rq##.status == 200 ->
            begin try
                let res = extract @@ Js.to_string rq##.responseText in
                callback res
              with _ -> debug "parsing error"
            end
          | DONE -> cberr ()
          | _ -> ()
        end
      ); rq

  let get rq url =
    rq##_open (Js.string "GET") url Js._true;
    rq##send Js.null

end

let get_repos name cb cberr =
  let open Api in
  let rq = mk_req Extract.repos cb cberr in
  get rq (Js.string (repos name))

let get_trees user repo cb cberr =
  let open Api in
  let rq = mk_req Extract.trees cb cberr in
  get rq (Js.string @@ trees user repo)

let get_contents user repo path cb cberr =
  let open Api in
  let rq = mk_req Extract.contents cb cberr in
  get rq (Js.string @@ contents user repo path)

let get_file extract cb cberr =
  let open Api in
  let rq = mk_req (Extract.file extract) cb cberr in
  get rq (Js.string @@ file extract)

