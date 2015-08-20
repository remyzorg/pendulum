




let%sync flick_gallery =
  js_input (onchange, oninput, onkeyup) input_txt;
  js_output gallery;


  let username_response = "" in
  loop begin
    present input_txt (
      let url = Flickr.Method.People.findByUsername @@
        Js.to_string (!!input_txt)##.value
      in
      run Sync_js.xhr url username_response
    );
    pause
  end
  ||
  loop begin
    present username_response
      () ;
    pause
  end




