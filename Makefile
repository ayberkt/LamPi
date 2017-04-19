all:
	ocaml setup.ml -build

oasis:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

clean:
	ocaml setup.ml -clean
