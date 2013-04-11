PWD = $(shell pwd)
UNAME := $(shell uname -s)

V ?= 0
PROGRAM = ../ucaml
ifeq ($(V), 1)
OPTIONS := -v
else
OPTIONS := 
endif
SRCS = $(shell ls *.ml)
CFILES = $(patsubst %.ml, %.c, $(SRCS))
TRASH = *.c *.d *.o *.cmo *.cmi

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

.PHONY:all clean
.PRECIOUS: $(CFILES)

all: $(PROGRAM) $(SRCS:%.ml=%.c) 

%.c: %.ml
	$(PROGRAM) $*.ml $(OPTIONS) -o $*.c 

%.o: %.c
	gcc $*.c -c -o $*.o $(CFLAGS)

%.out: %.o
	gcc -o $*.out $*.o $(LDFLAGS) 

clean:
	rm -f $(TRASH)

%.d: %.c
	gcc -M $(CFLAGS) $<  >  $@
