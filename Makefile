all:
	ocamlbuild -use-ocamlfind whimper.native

test: all
	./whimper.native test/abcdump.abc

clean:
	ocamlbuild -clean

dep:
	opam install uint extlib sexplib
