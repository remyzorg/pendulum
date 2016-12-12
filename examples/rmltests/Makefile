

rml:
	rmlc -thread test.rml

ml: rml
	ocamlc -I `opam config var lib`/rml/lco unix.cma rmllib.cma test.ml
	ocamlc -I `opam config var lib`/rml/lco unix.cma rmllib.cma test.cmo main.ml

clean:
	rm -rf *.cm* *.rzi a.out
