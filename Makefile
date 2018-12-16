SOURCES = xml.ml astToBlock.ml my_compile.ml main.ml
PACKS = compiler-libs.common compiler-libs.bytecomp
INCDIRS = +camlp4 +camlp4/Camlp4Parsers
LIBS = dynlink camlp4fulllib str
RESULT = block_of_ocaml
OCAMLMAKEFILE = ~/include/OcamlMakefile
include $(OCAMLMAKEFILE)
