SOURCES = d.ml l.ml type.mli type.ml id.ml syntax.ml parser.mly lexer.mll m.ml s.ml typing.mli typing.ml wrap.mli wrap.ml kNormal.mli kNormal.ml alpha.mli alpha.ml assoc.mli assoc.ml closure.mli closure.ml c.mli c.ml main.ml
RESULT = ucaml

all:debug-code $(RESULT) do_test

do_test: 
	make -C test 

clean-test:
	make -C test clean

include OCamlMakefile
