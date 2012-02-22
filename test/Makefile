PWD = $(shell pwd)
UNAME := $(shell uname -s)

PROGRAM = ../ucaml
OPTIONS := -v
TESTS = $(shell ls *.ml)
DEFFILES = $(patsubst %.ml, %.d, $(TESTS))
TRASH = *.log *.c *.d *.o *.out *.cmo *.cmi

USE_GC ?= 0

INC_DIRS := \
	/usr/local/include \
	../lib/include

CFLAGS := -g -Wall \
	-DDEBUG \
	-DUSE_GC=$(USE_GC) \
	$(addprefix -I, $(INC_DIRS))

LDFLAGS := -L../lib/src -lucaml

ifeq ($(USE_GC), 1)
OPTIONS += --gc
CFLAGS += -I/usr/local/include
LDFLAGS += -L/usr/local/lib -lgc
endif

.PHONY:all do-test clean depend full-test
.PRECIOUS: $(patsubst %.ml, %.c, $(TESTS))

all: do-test

do-test: $(TESTS:%.ml=%.diff) 

full-test: do-full-test clean

do-full-test:
	make USE_GC=0 clean_test
	make USE_GC=1 clean_test

clean_test: clean
	make do-test

%.c: $(PROGRAM) %.ml
	$(PROGRAM) $*.ml $(OPTIONS) -o $*.c 

%.o: %.c
	gcc $*.c -c -o $*.o $(CFLAGS)

%.out: %.o
	gcc $(LDFLAGS) -o $*.out $*.o

%.log: %.out
ifeq ($(UNAME), Darwin)
	env DYLD_LIBRARY_PATH=$$DYLD_LIBRARY_PATH:../lib/src ./$*.out > $*.log
else
	env LD_LIBRARY_PATH=$$LD_LIBRARY_PATH:../lib/src ./$*.out > $*.log
endif

%.ocaml.out: %.ml
	ocamlc $*.ml -o $*.ocaml.out

%.ocaml.log: %.ocaml.out
	./$*.ocaml.out > $*.ocaml.log

%.diff: %.log %.ocaml.log
	diff $*.log $*.ocaml.log

clean:
	rm -f $(TRASH)

%.d: %.c
	gcc -M $(CFLAGS) $<  >  $@

#depend: $(DEFFILES)

#-include $(DEFFILES)
