PWD = $(shell pwd)

PROGRAM = ../ucaml
TESTS = $(shell ls *.ml)
TRASH = *.log *.c *.out *.cmo *.cmi

INC_DIRS := \
	/usr/local/include \
	$(PWD)/..

SYSTEM = $(shell uname -s)
ifeq ($(SYSTEM), Cygwin)
INC_DIRS := $(shell cygpath -m $(INC_DIRS))
endif

CFLAGS := -Wall \
	$(addprefix -I, $(INC_DIRS))

.PHONY:all do_test clean foo
.PRECIOUS: $(patsubst %.ml, %.c, $(TESTS))

all: clean do_test

do_test: $(TESTS:%.ml=%.diff) 

%.c: $(PROGRAM) %.ml
	$(PROGRAM) $*.ml -v -o $*.c 

%.out: %.c
	gcc $*.c -o $*.out $(CFLAGS) -lgc -L/usr/local/lib

%.log: %.out
	./$*.out > $*.log

%.ocaml.out: %.ml
	ocamlc $*.ml -o $*.ocaml.out

%.ocaml.log: %.ocaml.out
	./$*.ocaml.out > $*.ocaml.log

%.diff: %.log %.ocaml.log
	diff $*.log $*.ocaml.log

clean:
	rm -f $(TRASH)
