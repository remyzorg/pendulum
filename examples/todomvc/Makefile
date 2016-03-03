
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum" \
		-pkgs js_of_ocaml,js_of_ocaml.ppx,js_of_ocaml.tyxml \
    -pkgs lwt,lwt.ppx \
		-pkgs ppx_deriving,js_of_ocaml.deriving.ppx,js_of_ocaml.deriving \
		main.byte
	js_of_ocaml +weak.js main.byte

clean:
	ocamlbuild -clean
