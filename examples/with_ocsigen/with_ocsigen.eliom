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




let () =
  With_ocsigen_app.register
    ~service:main_service
    (fun () () ->
      Lwt.return
        (Eliom_tools.F.html
           ~title:"with_ocsigen"
           ~css:[["css";"with_ocsigen.css"]]
           Html5.F.(body [
             h2 [pcdata "Welcome from Eliom's distillery!"];
           ])))
