

rml:
	rmlc test.rml

# ocamlfind ocamlc -I `opam config var lib`/rml/lco unix.cma rmllib.cma test.ml

ml:
	ocamlfind ocamlc -linkpkg -thread -package pendulum -package pendulum.ppx \
		-I `opam config var lib`/rml/lco rmllib.cma test.cmo main.ml

clean:
	rm -rf *.cm* *.rzi a.out
