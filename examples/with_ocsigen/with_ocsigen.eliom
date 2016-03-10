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


let todomvc_base =
  let open Html5.D in

  div ~a:[a_id "todomvc"] [
    section ~a:[a_class ["todoapp"]] [
      header ~a:[a_class ["header"]] [
        h1 [pcdata "todos"];
        input ~a:[
          a_id "new-todo";
          a_class ["new-todo"];
          a_input_type `Text;
          a_placeholder "What needs to be done ?";
          a_autofocus `Autofocus;
          a_name "newTodo";
        ] ()
      ];
      section ~a:[a_class ["main"]; a_style "visibility : visible;";] [
        input ~a:[
          a_id "select_all"; a_input_type `Checkbox;
          a_style "display : none;"; a_class ["toggle-all"];
        ] ();
        label ~a:[a_for "toggle_all"] [pcdata "Mark all as complete"];
        ul ~a:[a_id "todo-list"; a_class ["todo-list"]] [];
      ];
      footer ~a:[a_id "filter_footer"; a_class ["footer"]; a_style "display : none;"] [
        span ~a:[a_class ["todo-count"]][];
        ul ~a:[a_class ["filters"]] [
          li ~a:[(* a_href "#/"; *) a_class ["selected"]; a_id "visibility_all"] [pcdata "All"];
          li ~a:[(* a_href "#/active"; *) a_class ["selected"]; a_id "visibility_all"] [pcdata "All"];
          li ~a:[(* a_href "#/complete"; *) a_class ["selected"]; a_id "visibility_all"] [pcdata "All"];
        ]
      ]
    ]
  ]



(*
    <div id="todomvc">
      <div class="todomvc-wrapper">
        <section class="todoapp">
          <header class="header">
            <h1>todos</h1>
            <input id="new-todo"
                   class="new-todo"
                   type="text"
                   placeholder="What needs to be done?" autofocus
                   name="newTodo"/>
          </header>
          <section class="main" style="visibility: visible">
            <input id="select_all" type="checkbox" style="display: none" class="toggle-all">
            <label for="toggle-all">Mark all as complete</label>
            <ul id="todo-list" class="todo-list">
            </ul>
          </section>
          <footer id="filter_footer" class="footer" style="display: none">
            <span class="todo-count">
            </span>
            <ul class="filters">
              <li><a href="#/" class="selected" id="visibility_all" >All</a></li>
              <li><a href="#/active" id="visibility_active">Active</a></li>
              <li><a href="#/completed" id="visibility_completed">Completed</a></li>
            </ul>
            <button id="clear_complete" class="clear-completed" style="display: none">
              Clear completed</button>
          </footer>
        </section>
      </div>
    </div>
    <footer class="info">
      <a id="remove_storage">remove local storage</a>
      <p>Double-click to edit a todo</p>
      <p>Based on the <a href="https://ocsigen.github.io/blog/2015/10/07/react-example-todomvc/">js_of_ocaml + react-ocaml version</a>,
        made by OCsigen Team and the <a href="https://github.com/evancz/elm-todomvc">Elm one</a></p>
      <p>Created by <a href="http://github.com/remyzorg/">remyzorg</a></p>

    </footer>
    <script src="main.js"></script>
  </body>

*)



[%%client

    open Todomvc



]


let () =
  With_ocsigen_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"with_ocsigen"
           ~css:[["css";"with_ocsigen.css"]]
           Html5.F.(body [todomvc_base])))
