
all: main.ml
	ocamlbuild -use-ocamlfind \
		-cflags "-ppx ppx_pendulum" \
		-pkgs js_of_ocaml.ppx,js_of_ocaml.tyxml \
		-package pendulum \
		-package react \
		-package js_of_ocaml \
		main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
