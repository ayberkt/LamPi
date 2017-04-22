all:
	ocaml setup.ml -build

oasis:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

bnfc:
	bnfc --ocaml src/LamPi.bnfc -o src/parser --make

docs:
	mkdir docs
	bnfc --latex src/LamPi.bnfc -o docs

clean:
	ocaml setup.ml -clean
