all:
	dune build --root .

test:
	dune runtest --root .

opam:
	dune build --root . *.opam

release:
	dune-release tag ${VERSION}
	dune-release distrib --include-submodules --skip-build
	dune-release publish distrib
	dune-release opam pkg
	dune-release opam submit
