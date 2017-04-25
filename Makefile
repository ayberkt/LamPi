all:
	ocaml setup.ml -build

oasis:
	oasis setup
	ocaml setup.ml -configure
	ocaml setup.ml -build

bnfc:
	bnfc --ocaml src/LamPi.bnfc -o src

bnfc-clean:
	rm -rf AbsLamPi.ml
	rm -rf BNFC_Util.ml
	rm -rf LexLamPi.mll
	rm -rf ParLamPi.mly
	rm -rf PrintLamPi.ml
	rm -rf ShowLamPi.ml
	rm -rf SkelLamPi.ml
	rm -rf TestLamPi.ml


docs:
	mkdir docs
	bnfc --latex src/LamPi.bnfc -o docs

clean:
	ocaml setup.ml -clean
