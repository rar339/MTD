.PHONY: test

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/test.exe

bisect: bisect-clean
	-dune exec --instrument-with bisect_ppx --force test/test.exe
	bisect-ppx-report html

bisect-clean:
	rm -rf _coverage bisect*.coverage

clean: bisect-clean
	dune clean

mtd:
	OCAMLRUNPARAM=b dune exec bin/main.exe

doc:
	dune build @doc

opendoc: doc
	@bash opendoc.sh
