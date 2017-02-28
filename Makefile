all:
	ocaml setup.ml -build

oasis:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

clean:
	oasis setup-clean
	rm -rf _build
	rm -f setup.ml setup.log
	rm myocamlbuild.ml
	setup.log
