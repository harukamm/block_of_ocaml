INCDIRS = ../
SOURCES = ui.ml
LIBS = block_of_ocaml
RESULT = converter
PACKS = js_of_ocaml js_of_ocaml.syntax compiler-libs.common compiler-libs.bytecomp
PREDS = camlp4o
OCAMLMAKEFILE = ../OCamlMakefile
OCAMLFLAGS = -syntax camlp4o

$(RESULT).js: byte-code
	js_of_ocaml +weak.js +toplevel.js +dynlink.js $(RESULT)

include $(OCAMLMAKEFILE)
