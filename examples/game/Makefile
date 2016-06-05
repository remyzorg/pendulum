

all: main.ml
	ocamlbuild -use-ocamlfind \
		-cflags "-ppx ppx_pendulum" \
		-pkgs js_of_ocaml.ppx,js_of_ocaml \
		-pkgs ppx_deriving,ppx_deriving.std \
		-pkgs pendulum \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
