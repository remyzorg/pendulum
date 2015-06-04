


(* OASIS_START *)
(* OASIS_STOP *)

open Ocamlbuild_plugin

let () =
  dispatch @@ MyOCamlbuildBase.dispatch_combine [
    (fun hook ->
       dispatch_default hook;
       match hook with

       | After_rules ->
         let env = BaseEnvLight.load ~allow_empty:true
             ~filename:MyOCamlbuildBase.env_filename () in

         let native_suffix =
           if BaseEnvLight.var_get "is_native" env = "true"
           then "native" else "byte"
         in

         flag ["ocaml"; "compile"; "ppx_pendulum"] &
         S [A "-ppx"; A ("src/ppx/ppx_pendulum." ^ native_suffix)];
         ignore(S [A "-dsource"]);

         mark_tag_used("tests");
         mark_tag_used("pkg_ppx_pendulum");
         mark_tag_used("pkg_ppx_deriving.std");
         mark_tag_used("pkg_ppx_deriving");

       | _ ->
         ());
    dispatch_default
  ]
