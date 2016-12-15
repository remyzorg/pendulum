
let%sync counter ~animate =
  element sp, mbtn;
  input sec 1000;
  let s = 0 in
  every sec (emit s (!!s + 1))
  || loop (present mbtn##onclick (emit s 0); pause)
  || loop (present s (emit sp##.value (Js.s "%d" !!s)); pause)




let%sync timer =
  element progress;
  input sec 1000;
  input s;
  input max;

  let seeking = () in
  every sec (emit s (!!s + 1))
  ||
  each s (
    present seeking nothing
      (emit progress##.value (upd !!s))
  )
  ||
  each progress##onmousedown begin
    trap t begin
      loop begin
        emit seeking;
        present progress##onmouseup
          (!(upd_time !!s); exit t)
      ; pause
      end
    end
  end





