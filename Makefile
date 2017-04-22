all:
	ocaml setup.ml -build

oasis:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

bnfc:
	bnfc --ocaml src/LamPi.bnfc -o src/parser --make

clean:
	ocaml setup.ml -clean
