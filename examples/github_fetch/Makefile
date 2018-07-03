all: main.ml
	ocamlbuild -use-ocamlfind \
		-package js_of_ocaml.ppx \
		-package pendulum-lib \
		-package pendulum-ppx \
		-package re \
		-package js_of_ocaml \
		-package js_of_ocaml.tyxml \
		-package cohttp-lwt-jsoo \
		-package yojson \
		main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
