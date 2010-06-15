# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = commons.ml asm.ml triasm.ml leoc.ml optim.ml noise.ml leo.ml tokens.ml leolex.mll parsercomb.ml parse.ml main.ml

# the name of the resulting executable
RESULT  = leoc

# generate type information (.annot files)
ANNOTATE = yes
MSVC = true

OCAMLFLAGS = -I c:/ocaml/extlib
OCAMLLDFLAGS = -I c:/ocaml/extlib
LIBS = extlib 

# make target (see manual) : byte-code, debug-code, native-code, ...
all: debug-code

include OCamlMakefile