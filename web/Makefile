INCDIRS = ../
SOURCES = ui.ml
LIBS = block_of_ocaml
RESULT = converter
PACKS = js_of_ocaml js_of_ocaml.syntax compiler-libs.common compiler-libs.bytecomp
PREDS = camlp4o
OCAMLMAKEFILE = ~/include/JsOfOcamlMakefile
include $(OCAMLMAKEFILE)

$(RESULT).js : $(RESULT)
	js_of_ocaml +weak.js $<
