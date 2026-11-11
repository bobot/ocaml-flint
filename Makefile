.PHONY: all test opam release

all:
	dune build --root .

test:
	dune runtest --root .

opam:
	dune build --root . *.opam

# make release TAG=x.y.z, from an up-to-date clean main whose CHANGES starts
# with "## Release x.y.z ##"
release:
	@test -n "$(TAG)" || { echo "usage: make release TAG=x.y.z"; exit 1; }
	@test "$$(sed -n '1s/^## *\(Release \)\{0,1\}\([^ ]*\).*/\2/p' CHANGES)" = "$(TAG)" \
	  || { echo "CHANGES does not start with ## Release $(TAG) ##"; exit 1; }
	@git diff --quiet HEAD || { echo "uncommitted changes"; exit 1; }
	@git fetch -q origin main
	@test "$$(git rev-parse HEAD)" = "$$(git rev-parse origin/main)" \
	  || { echo "HEAD is not origin/main"; exit 1; }
	@sed -n '1p;2,/^##/{/^##/!p;}' CHANGES
	@echo "Is the CHANGES correct for $(TAG) (y/n)?"
	@read yesno; test "$$yesno" = y
	dune-release tag $(TAG)
	dune-release distrib
	dune-release publish
	dune-release opam pkg
	dune-release opam submit
