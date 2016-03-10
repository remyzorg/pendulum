
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package js_of_ocaml.ppx \
		-package pendulum \
		-package pendulum.ppx \
		-package js_of_ocaml \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
