
open Firebug

let error f = Printf.ksprintf
    (fun s -> Firebug.console##error (Js.string s); failwith s) f
let debug f = Printf.ksprintf
    (fun s -> Firebug.console##log(Js.string s)) f
let alert f = Printf.ksprintf
    (fun s -> Dom_html.window##alert(Js.string s); failwith s) f

module CoerceTo = Dom_html.CoerceTo
let (@>) s coerce =
  Js.Opt.get Dom_html.(coerce @@ getElementById s)
    (fun () -> error "can't find element %s" s)




module View = struct

  let create_item cnt animate delete_sig str = assert false

end

let iter f opt =
  match opt with
  | None -> ()
  | Some o -> f o

let default v f opt = 
  match opt with
  | None -> v
  | Some o -> f o

let jstr s = Js.some @@ Js.string ""

let is_eq_char k q = Js.Optdef.case k
    (fun _ -> false)
    (fun x -> x = Char.code q)

let enter_pressed ev =
   default false (fun ev ->
      Js.Optdef.case ev##.charCode
        (fun () -> false)
        (fun c -> c = 13)
    ) ev


module Controller = struct
  let%sync machine ~animate =
    input items_ul;
    input new_field;

    (* FILL HERE *)
    loop pause


    end



let main _ =
  let items_ul = "todo-list" @> CoerceTo.ul in
  let new_todo = "new-todo" @> CoerceTo.input in
  let _m_react = Controller.machine (items_ul, new_todo) in
  Js._false


let () = Dom_html.(window##.onload := handler main)

