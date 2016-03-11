[%%shared
    open Eliom_lib
    open Eliom_content
    open Html5.D
]

module With_ocsigen_app =
  Eliom_registration.App (
    struct
      let application_name = "with_ocsigen"
    end)

let main_service =
  Eliom_service.App.service ~path:[] ~get_params:Eliom_parameter.unit ()


let todomvc_base () =
  let open Html5.F in
  div ~a:[a_id "todomvc"] [
    div ~a:[a_class [ "todomvc-wrapper"]] [
      section ~a:[a_class ["todoapp"]]
        [ header ~a:[a_class ["header"]]
            [ h1 [pcdata "todos"]
            ; input ~a:[ a_id "new-todo" ; a_class ["new-todo"] ; a_input_type `Text
                       ; a_placeholder "What needs to be done ?" ; a_autofocus `Autofocus
                       ; a_name "newTodo"] ()
            ]
        ; section ~a:[a_class ["main"]; a_style "visibility : visible;";]
            [ input ~a:[a_id "select_all"; a_input_type `Checkbox;
                        a_style "display : none;"; a_class ["toggle-all"]] ()
            (* ; label ~a:[a_for "toggle_all"] [pcdata "Mark all as complete"] *)
            ; ul ~a:[a_id "todo-list"; a_class ["todo-list"]] [];
            ]
        ; footer ~a:[a_id "filter_footer"; a_class ["footer"]; a_style "display : none;"]
            [ span ~a:[a_class ["todo-count"]][]
            ; ul ~a:[a_class ["filters"]]
                [ li  [Raw.a ~a:[a_href @@ Xml.uri_of_string "#/"; a_class ["selected"]; a_id "visibility_all"]
                         [pcdata "All"]]
                ; li  [Raw.a ~a:[a_href @@ Xml.uri_of_string "#/active"; a_id "visibility_active"]
                         [pcdata "Active"]]
                ; li  [Raw.a ~a:[a_href @@ Xml.uri_of_string "#/completed"; a_id "visibility_completed"]
                         [pcdata "Completed"]]
                ]
            ; button ~a:[a_id "clear_complete"; a_class ["clear-completed"]; a_style "display : none;" ]
                [pcdata "Clear completed"]
            ]
        ]
    ]
  ]



let info_footer () =
  let open Html5.F in
  footer ~a:[a_class ["info"]]
    [ span ~a:[a_id "remove_storage"] [pcdata "remove local storage"]
    ; p [ pcdata "Double-click to edit a todo"]
    ; p [ pcdata "Based on the "
        ; Raw.a ~a:[a_href @@ Xml.uri_of_string "https://ocsigen.github.io/blog/2015/10/07/react-example-todomvc/"]
            [pcdata "jsoo + react-ocaml version"]
        ; pcdata ", made by OCsigen Team and the "
        ; Raw.a ~a:[a_href @@ Xml.uri_of_string "https://github.com/evancz/elm-todomvc"]
            [pcdata "Elm one"]]
    ; p [ pcdata "Written by "
        ; Raw.a ~a:[a_href @@ Xml.uri_of_string "http://remyzorg.github.io"] [pcdata "Remyzorg"]
        ]
    ]


let%client main _ =
  let open Todomvc in
  let  _, _, _, _, m_react = Controller.machine CoerceTo.(
    "todo-list" @> ul
  , "new-todo" @> input
  , "filter_footer" @> element
  , "clear_complete" @> button
  , "select_all" @> input, [], []
  , "visibility_all" @> a
  , "visibility_completed" @> a
  , "visibility_active" @> a
  , "remove_storage" @> element) in
  ignore (m_react ());
  false

let%client _ =  Lwt_js_events.onload () >|= main



let () =
  With_ocsigen_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"Pendulum on Eliom • TodoMVC"
           ~css:[["css"; "node_modules"; "todomvc-common"; "base.css"];
                 ["css"; "node_modules"; "todomvc-app-css"; "index.css"]  ]
           Html5.F.(body [
             todomvc_base ();
             info_footer ()
           ])))
