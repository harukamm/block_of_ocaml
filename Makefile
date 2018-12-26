SOURCES = xml.ml astToBlock.ml my_parse.mli my_parse.ml my_compile.ml main.ml
PACKS = compiler-libs.common compiler-libs.bytecomp
RESULT = block_of_ocaml
OCAMLMAKEFILE = OCamlMakefile
include $(OCAMLMAKEFILE)
