
all: main.ml
	ocamlbuild main.byte -use-ocamlfind \
		-pkgs js_of_ocaml,js_of_ocaml.syntax \
		-syntax camlp4o
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
