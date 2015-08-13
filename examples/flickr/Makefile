
all: main.ml
	ocamlbuild -use-ocamlfind \
		-package pendulum -cflags "-ppx ppx_pendulum" \
		-pkgs js_of_ocaml,yojson,js_of_ocaml.ppx,lwt,lwt.unix,lwt.ppx \
		main.byte
	js_of_ocaml main.byte

clean:
	ocamlbuild -clean
