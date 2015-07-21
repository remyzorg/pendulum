
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package js_of_ocaml -package js_of_ocaml.ppx main.byte

clean:
	ocamlbuild -clean
