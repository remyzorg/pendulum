
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum-lib  \
		-package pendulum-ppx  \
		-package js_of_ocaml -package js_of_ocaml.ppx main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
