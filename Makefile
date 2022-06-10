all:
	dune build --root .

test:
	dune runtest --root .

opam:
	dune build --root . *.opam
