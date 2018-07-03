

#jsoo ppx has to be put before pendulum
#compile the bytecode with jsoo compiler

all: player.ml
	ocamlbuild -use-ocamlfind \
		-pkgs js_of_ocaml,js_of_ocaml.ppx \
		-pkgs pendulum-lib,pendulum-ppx \
     player.byte
	js_of_ocaml player.byte

clean:
	ocamlbuild -clean
