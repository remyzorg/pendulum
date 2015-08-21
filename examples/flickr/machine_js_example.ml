

open Flickr
open Method


let%sync flick_gallery =
  js_input (onchange, oninput, onkeyup) input_txt;
  input gallery;


  let username_response = "" in
  let photos_response = "" in
  let one_photo = "" in
  let gallery_div = createDiv document in
  loop begin
    present input_txt (
      atom (
        Js.Opt.iter (photos_area##.firstChild) (Dom.removeChild photos_area)
      );
      let url = Flickr.Method.People.findByUsername @@
        Js.to_string (!!input_txt)##.value
      in
      run Sync_js.xhr (url, username_response)
    ); pause
  end ||
  loop begin
    present username_response (
      let id = extract_user_id !!username_response in
      let url = People.getPhotos !!id in
      run Sync_js.xhr (url, photos_response)
    ) ; pause
  end ||
  loop begin
    present photos_response (
      let urls = extract_photos !!photos_response in
      atom (
        let gallery_div = createDiv document in
        Dom.appendChild gallery gallery_div;
      );
      let allfinished = () in
      run Sync_js.par_xhrs (urls, one_photo, allfinished)
    ); pause
  end ||
  loop begin
    present one_photo (
      atom (
        let url = Photo.extract_url Square !!one_photo in
        let photo_img = createImg document in
        photo_img##.src := Js.string e;
        Dom.appendChild gallery_div photo_img
      ));
    pause
  end


