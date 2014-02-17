all:
	ocamlbuild -use-ocamlfind whimper.native

test: all
	./whimper.native s.abc t.abc
	hd s.abc >s.hex
	hd t.abc >t.hex
	diff -u s.hex t.hex |head

clean:
	ocamlbuild -clean

dep:
	opam install uint extlib sexplib
