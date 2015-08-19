

let rest = "https://api.flickr.com/services/rest/"
let apikey = "b551afe7603cf01eac70d54262cd07df"

open Lwt

module Net = struct
  let get ?(frm=None) url =
    XmlHttpRequest.(
      (get url) >|=
      (fun x -> match frm with
         | None -> x.content
         | Some frm -> frm := x; x.content)
    )
end

let mk_rq ?(frm=None) meth others =
  let req = rest ^ "?api_key=" ^ apikey ^ "&method=" ^ meth ^ "&format=json" ^ others
  in Net.get ~frm req


module Json = struct
  open Yojson
  include Yojson.Basic.Util

  let json_header = "jsonFlickrApi("
  let pretty = Basic.pretty_to_string

  let extract_json data =
    Basic.from_string String.(sub data (length json_header)
              (length data - 1 - length json_header))


end

module Method = struct
  let flickr = "flickr."

  module People = struct
    let string  m = flickr ^ "people." ^ m
    let findByUsername ?frm username =
      mk_rq ~frm (string "findByUsername") ("&username=" ^ username ^ "&extra=url_o")

    let getPhotos ?frm user_id =
      mk_rq ~frm (string "getPhotos") ("&user_id=" ^ user_id ^ "&extra=url_o")

    let extract_photos json =
      let open Json in
      [extract_json json] |> filter_member "photos"
      |> filter_member "photo"
      |> flatten
      |> filter_member "id"
      |> filter_string

    let extract_user_id json =
      let open Json in
      extract_json json
      |> member "user"
      |> member "id"
      |> to_string

  end

  module Photos = struct
    let string m = flickr ^ "photos." ^ m
    let getSizes ?frm photo_id =
      mk_rq ~frm (string "getSizes") ("&photo_id=" ^ photo_id)

    type size =
      | Square | LargeSquare | Thumbnail | Small
      | Small320 | Medium | Medium640 | Medium800 | Large | Original

    let size_to_string (size : size) = match size with
      | Square  -> "Square"
      | LargeSquare  -> "Large Square"
      | Thumbnail  -> "Thumbnail"
      | Small  -> "Small"
      | Small320  -> "Small 320"
      | Medium  -> "Medium"
      | Medium640  -> "Medium 640"
      | Medium800  -> "Medium 800"
      | Large  -> "Large"
      | Original  -> "Original"

    let extract_url size json =
      let open Json in
      [extract_json json]
      |> filter_member "sizes"
      |> filter_member "size"
      |> flatten
      |> List.filter (fun x ->
          [x] |> filter_member "label"
          |> filter_string
          |> List.mem @@ size_to_string size)
      |> filter_member "source"
      |> filter_string
  end

end
