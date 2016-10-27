OCB_FLAGS = -use-ocamlfind -use-menhir -I isa -I src
OCB       = ocamlbuild $(OCB_FLAGS)


all:	native byte # profile debug

clean:
	$(OCB) -clean

native:	sanity
	$(OCB) otis.native

byte:	sanity
	$(OCB) otis.byte

doc:
	$(OCB) otis.docdir/index.html

profile:	sanity
	$(OCB) -tag profile atbpa.native

debug:	sanity
	$(OCB) -tag debug atbpa.byte

sanity: # check that menhir is installed
	which menhir

test:	native
	./otis.native "-help"

.PHONY:
	all clean byte native profile debug sanity test
