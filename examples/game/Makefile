

all: main.ml
	ocamlbuild -use-ocamlfind \
		-pkgs js_of_ocaml.ppx,js_of_ocaml \
		-pkgs ppx_deriving,ppx_deriving.std,ppx_deriving_yojson,yojson \
		-pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml \
		-pkgs pendulum,pendulum.ppx \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
