all:
	dune build --root .

test:
	dune runtest --root .
