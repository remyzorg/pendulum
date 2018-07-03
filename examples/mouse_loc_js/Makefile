
all: main.ml
	ocamlbuild -use-ocamlfind \
		-pkgs js_of_ocaml.ppx,js_of_ocaml.tyxml \
		-package pendulum-lib \
		-package pendulum-ppx \
		-package react \
		-package js_of_ocaml \
		main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
