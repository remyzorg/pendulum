

all: main.ml
	ocamlbuild -use-ocamlfind \
		-package js_of_ocaml.ppx \
		-package pendulum \
		-package js_of_ocaml \
		-package pendulum.ppx \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
