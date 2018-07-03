
all: main.ml
	ocamlbuild -use-ocamlfind \
    -pkgs lwt,lwt.ppx \
		-pkgs ppx_deriving,js_of_ocaml-ppx.deriving,js_of_ocaml.deriving \
		-pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml \
		-pkgs pendulum-lib,pendulum-ppx \
		main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
