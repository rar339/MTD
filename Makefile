.PHONY: test

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/mtd.exe

mtd:
	OCAMLRUNPARAM=b dune exec bin/main.exe