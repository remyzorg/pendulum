
let%sync react_prog =
  react e1;
  loop begin
    emit e1;
    pause
  end



let%sync mouse =
  element w {
    onmousemove = "", (fun x ev -> Format.sprintf "%d,%d" ev##.clientX ev##.clientY);
  };
  react write;
  loop begin
    present w##onmousemove
      (emit write !!(w##onmousemove))
  ; pause
  end
