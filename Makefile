# see: http://www.ocaml.info/home/ocaml_sources.html#toc16

# put here the names of your source files (in the right order)
SOURCES = perf.c prof.ml commons.ml asm.ml triasm.ml leoc.ml subexp.ml noise.ml \
 leo.ml optim.ml tokens.ml leolex.mll parsercomb.ml parse.ml fib.ml shrink.ml main.ml

# the name of the resulting executable
RESULT  = leoc

# generate type information (.annot files)
ANNOTATE = yes
#MSVC = true

OCAMLFLAGS = -I c:/overbld/ocaml/lib/site-lib/extlib
OCAMLLDFLAGS = -I c:/overbld/ocaml/lib/site-lib/extlib -cclib "-L c:\mingw\lib"
LIBS = extlib 

# make target (see manual) : byte-code, debug-code, native-code, ...
all: native-code

include OCamlMakefile