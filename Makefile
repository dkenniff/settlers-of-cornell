.PHONY: test bisect python

build:
	dune build

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f release.zip
	zip -r release.zip . -x@exclude.lst

bisect:
	rm -f bisect*.coverage
	OCAMLRUNPARAM=b dune exec --instrument-with bisect_ppx test/main.exe
	bisect-ppx-report html

bisectopen: bisect
	open _coverage/index.html 

clean:
	dune clean
	rm -f bisect*.coverage
	rm -r -f _coverage/

doc:
	dune build @doc

opendoc: doc
	open _build/default/_doc/_html/index.html

lines:
	cloc --by-file --include-lang=OCaml .

python:
	OCAMLRUNPARAM=b dune exec python/main.exe
