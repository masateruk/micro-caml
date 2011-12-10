SOURCES = type.mli type.ml id.ml syntax.ml parser.mly lexer.mll m.ml typing.mli typing.ml s.ml kNormal.mli kNormal.ml alpha.mli alpha.ml assoc.mli assoc.ml closure.mli closure.ml c.mli c.ml main.ml
LIBS = str
RESULT  = ucaml

include OCamlMakefile
