all:
	ocaml setup.ml -build

oasis:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

bnfc:
	bnfc --ocaml src/LamPi.bnfc -o src

bnfc-clean:
	rm -rf src/AbsLamPi.ml
	rm -rf src/BNFC_Util.ml
	rm -rf src/LexLamPi.mll
	rm -rf src/ParLamPi.mly
	rm -rf src/PrintLamPi.ml
	rm -rf src/ShowLamPi.ml
	rm -rf src/SkelLamPi.ml
	rm -rf src/TestLamPi.ml


docs:
	mkdir docs
	bnfc --latex src/LamPi.bnfc -o docs;
	lualatex --output-directory=docs docs/LamPi.tex

docs-clean:
	rm -rf docs

clean:
	ocaml setup.ml -clean
