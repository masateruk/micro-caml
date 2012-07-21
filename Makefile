SOURCES = d.ml l.ml id.ml m.ml type.mli type.ml syntax.ml parser.mly lexer.mll s.ml typing.mli typing.ml wrap.mli wrap.ml kNormal.mli kNormal.ml alpha.mli alpha.ml assoc.mli assoc.ml closure.mli closure.ml cType.ml c.mli c.ml optimize.ml cFormat.ml main.ml
RESULT = ucaml
LIBS = str

ifneq ($(shell which ocp-ocamlc.opt),)
OCAMLC = ocp-ocamlc.opt
OCAMLOPT = ocamlopt.opt
OCAMLFLAGS = -dtypes
endif

CC = gcc
CFLAGS = -g -Wall 

TRASH = *.cmt *.cmti *.annot .typerex

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
