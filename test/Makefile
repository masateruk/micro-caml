PWD = $(shell pwd)
PROGRAM = $(PWD)/../ucaml

TESTS = $(shell ls *.ml)
TRASH = *.log *.c *.out *.cmo *.cmi

.PRECIOUS: $(patsubst %.ml, %.c, $(TESTS))

all: clean do_test

do_test: $(TESTS:%.ml=%.diff) 

%.c: $(PROGRAM) %.ml
	$(PROGRAM) $*.ml -v -o $*.c 

%.out: %.c
	gcc -Wall $*.c -o $*.out -I/usr/local/include -I$(PWD)/.. -lgc -L/usr/local/lib

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
