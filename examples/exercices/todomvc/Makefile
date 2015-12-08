
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum" \
		-pkgs js_of_ocaml,js_of_ocaml.ppx main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
