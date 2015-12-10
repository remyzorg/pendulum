
all: reactiveplayer.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum" \
		-package js_of_ocaml -package js_of_ocaml.ppx reactiveplayer.byte
	js_of_ocaml reactiveplayer.byte

clean:
	ocamlbuild -clean
