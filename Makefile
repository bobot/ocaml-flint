all:
	dune build --root . @install

test:
	dune runtest --root .
