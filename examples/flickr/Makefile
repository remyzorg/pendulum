
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum" \
		-package js_of_ocaml -package yojson -package js_of_ocaml.ppx \
		-package lwt -package lwt.ppx \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
