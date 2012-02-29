SOURCES = d.ml l.ml type.mli type.ml id.ml syntax.ml parser.mly lexer.mll m.ml s.ml typing.mli typing.ml wrap.mli wrap.ml kNormal.mli kNormal.ml alpha.mli alpha.ml assoc.mli assoc.ml closure.mli closure.ml c.mli c.ml optimize.ml cFormat.ml main.ml
RESULT = ucaml

CC = gcc
CFLAGS = -g -Wall

.PHONY:all debug-check do-test full_test lib clean-test

all:debug-code $(RESULT) lib

debug-check:all do-test

do-test: $(RESULT) lib
	make -C test 

full-test: debug-code $(RESULT) lib
	make -C test full-test

clean-test:
	make -C test clean

clean-lib:
	make -C lib clean

lib: 
	make -C lib

include OCamlMakefile
