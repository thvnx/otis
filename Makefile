
# - The -I flag introduces sub-directories 
# - -use-ocamlfind is required to find packages
# - _tags file introduces packages, pxp
# - using *.mll and *.mly are handled automatically

OCB_FLAGS = -use-ocamlfind -use-menhir -I isa -I src

OCB = ocamlbuild $(OCB_FLAGS)

all:	native byte # profile debug

clean:
	$(OCB) -clean

native:	sanity
	$(OCB) atbpa.native

byte:	sanity
	$(OCB) atbpa.byte

profile:	sanity
	$(OCB) -tag profile atbpa.native

debug:	sanity
	$(OCB) -tag debug atbpa.byte

sanity: # check that menhir is installed, use "opam install menhir"
	which menhir

test:	native
	./atbpa.native "-help"

.PHONY:
	all clean byte native profile debug sanity test
