all: main.ml
	ocamlbuild -use-ocamlfind \
		-cflags "-ppx ppx_pendulum" \
		-package js_of_ocaml.ppx \
		-package pendulum \
		-package re \
		-package js_of_ocaml \
		-package js_of_ocaml.tyxml \
		-package cohttp.js \
		-package yojson \
		main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
