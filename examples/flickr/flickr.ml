

let rest = "https://api.flickr.com/services/rest/"
let apikey = "b551afe7603cf01eac70d54262cd07df"


module Net = struct
  let get = XmlHttpRequest.get
end

let mk_rq meth others =
  let req = rest ^ "?api_key=" ^ apikey ^ "&method=" ^ meth ^ "&format=json" ^ others
  in Net.get req


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
    let findByUsername username =
      mk_rq (string "findByUsername") ("&username=" ^ username ^ "&extra=url_o")

    let getPhotos user_id =
      mk_rq (string "getPhotos") ("&user_id=" ^ user_id ^ "&extra=url_o")

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
    let getSizes photo_id =
      mk_rq (string "getSizes") ("&photo_id=" ^ photo_id)

    let extract_url json =
      let open Json in
      [extract_json json]
      |> filter_member "sizes"
      |> filter_member "size"
      |> flatten
      |> List.filter (fun x ->
          [x] |> filter_member "label"
          |> filter_string
          |> List.mem "Original")
      |> filter_member "source"
      |> filter_string
  end

end
