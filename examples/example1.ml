

let submit = Dom.submit "Send"
let input_name = Dom.input ~hint:"Your name..."
let input_message = Dom.input ~hint:"Say something..."
let form = Dom.form [
    input_name;
    input_message;
    submit
  ]

let message_list = div []

let m = [%sync
  input event sub;
  input name;
  input message;
  output msgdiv;
  output sendserver;
  input event inserver;

  signal m ("", "");
  every begin
    emit m (name, message);
    emit sendserver (name, message);
  end (sub && name != "" && message != "")
  ||
  every begin
    emit messages ((name, message) :: messages);
  end (inserver || m)
  ||
  every begin
    emit msgdiv (map (fun l -> List.map (fun (a, m) ->
        div [h1 (text a); text m]
      ) l) messages);
  end messages
]

let () =
  Pendulum.run m (
    onclick submit,
    text input_name,
    text input_message;
    message_list,
    Up.new_message,
    Down.new_message
  )

let append n s = React.map (fun v -> n.append s) s

(* ============================================================= *)


let newmsg_up = Eliom_react.Up.create (Eliom_parameter.ocaml ("","") Json.t<string>)
let newmsg_down = Eliom_react.Up.create (Eliom_parameter.ocaml ("", "") Json.t<string>


{client{

let message_list = div []
let submit = Dom.submit "Send"
let input_name = Dom.input ~hint:"Your name..."
let input_message = Dom.input ~hint:"Say something..."

let newmsg_up = Sync.of_react %newmsg_up
let newmsg_down = Sync.of_react %newmsg_down

let m_mlist = [%sync
  every newmsgs_up (msg, auth) begin
    emit !append (div [h1 [auth]; msg])
  end]

let m_submit = [%sync
  every !click begin
    emit newmsg_down (input_name#value, input_message#value)
  end]

let () =
  Sync.attach mlist m_mlist;
  Sync.attach m_submit

let form = Dom.form [
    input_name;
    input_message;
    submit
  ]

}}
