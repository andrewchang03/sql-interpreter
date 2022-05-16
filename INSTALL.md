# Installation

## Setting Up OCaml
First and foremost, if OCaml is not installed on your machine, please visit
[this link](https://cs3110.github.io/textbook/chapters/preface/install.html)
to set up OCaml. Full credit to Professor Michael Clarkson, 
Cornell's CS 3110 instructor, for his tremendous work on building this
textbook.

## Packages
If not yet installed, please make sure the following packages are installed
via opam:
- ANSITerminal
- ounit2
- csv
  - download from https://github.com/Chris00/ocaml-csv
  - follow instructions on github linked on above webpage and do opam install csv
- unix
- lablgl 
  - download from http://wwwfun.kurims.kyoto-u.ac.jp/soft/olabl/lablgl.html
  - follow instructions on github linked on above webpage and do opam install lablgl 


The appropriate command is
  ``opam install [package name]``.

Other packages that may be necessary for you to build, run, and test this
project should have been installed with OCaml, but if not installed, the
terminal should give you a warning on missing packages, and you should be able
to do ``opam install`` command as above to install what's needed.

## Make and Run
The Makefile configures the following commands which you can run in
the main project directory:
- ``make build`` which runs dune build
- ``make utop`` which launches the top level with src files
- ``make run`` which launches the REPL to interact with our database management
system
- ``make test`` to run unit tests
- ``make linecount`` which runs ``cloc --by-file --include-lang=OCaml .`` to
count the number of OCaml code lines for grading purposes. Note that this also
runs ``ocamlbuild -clean``, which cleans out the directory including _build,
do you'll need to build the project again before doing other things with the
project.