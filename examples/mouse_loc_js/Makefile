
all: main.ml
	ocamlbuild -use-ocamlfind \
		-cflags "-ppx ppx_pendulum" \
		-package js_of_ocaml.ppx \
		-package pendulum \
		-package js_of_ocaml \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
