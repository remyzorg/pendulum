

rml:
	rmlc -thread test.rml

ml: rml
	ocamlfind ocamlc -I `opam config var lib`/rml/lco unix.cma rmllib.cma test.ml
	ocamlfind ocamlc -package pendulum -package pendulum.ppx -I `opam config var lib`/rml/lco unix.cma rmllib.cma test.cmo main.ml

clean:
	rm -rf *.cm* *.rzi a.out
