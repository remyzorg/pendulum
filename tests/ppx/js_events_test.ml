



let%sync basic =
  input elt;

  loop begin
    present elt##click
      (atom (Format.printf "click\n"))
  end


