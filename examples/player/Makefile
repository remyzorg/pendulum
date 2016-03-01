
all: reactiveplayer.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum" \
		-pkgs js_of_ocaml,js_of_ocaml.ppx \
    -pkgs ppx_deriving,js_of_ocaml.deriving.ppx,js_of_ocaml.deriving \
    -pkgs lwt,lwt.ppx \
     reactiveplayer.byte
	js_of_ocaml reactiveplayer.byte

clean:
	ocamlbuild -clean
