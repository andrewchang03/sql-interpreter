.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

run:
	OCAMLRUNPARAM=b dune exec terminal/main.exe

zip:
	rm -f database.zip
	zip -r database.zip . -x@exclude.lst

clean:
	dune clean
	rm -f database.zip

docs:
	dune build @doc

linecount:
	ocamlbuild -clean
	cloc --by-file --include-lang=OCaml .