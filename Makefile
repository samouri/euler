# Makefile
build:
	ocamlbuild -use-ocamlfind solutions.d.byte
clean:
	ocamlbuild -clean
.PHONY: build clean
