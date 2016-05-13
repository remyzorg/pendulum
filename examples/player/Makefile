

#jsoo ppx has to be put before pendulum
#compile the bytecode with jsoo compiler

all: reactiveplayer.ml
	ocamlbuild -use-ocamlfind \
		-pkgs js_of_ocaml,js_of_ocaml.ppx \
		-pkgs pendulum,pendulum.ppx \
     reactiveplayer.byte
	js_of_ocaml reactiveplayer.byte

clean:
	ocamlbuild -clean
