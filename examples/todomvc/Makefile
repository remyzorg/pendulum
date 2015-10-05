
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum" \
		-pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml,tyxml main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
