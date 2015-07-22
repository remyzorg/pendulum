
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum -dsource" \
		-package js_of_ocaml -package js_of_ocaml.ppx main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
