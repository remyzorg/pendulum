

rml:
	rmlc -thread test.rml

# ocamlfind ocamlc -I `opam config var lib`/rml/lco unix.cma rmllib.cma test.ml

ml: rml
	ocamlfind ocamlc -package pendulum -package pendulum.ppx \
		-I `opam config var lib`/rml/lco unix.cma rmllib.cma test.cmo main.ml

clean:
	rm -rf *.cm* *.rzi a.out
