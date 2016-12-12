
let%sync react_prog =
  react e1;
  loop begin
    emit e1;
    pause
  end
