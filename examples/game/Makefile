

all: main.ml
	ocamlbuild -use-ocamlfind \
		-pkgs js_of_ocaml.ppx,js_of_ocaml \
		-pkgs pendulum,pendulum.ppx \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
