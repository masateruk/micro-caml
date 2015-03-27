
UNAME := $(shell uname -s)
ifeq ($(UNAME), Darwin)
  SHLIB_EXT = dylib
  SHLIB_FLAGS = -dynamiclib -fPIC
else
  SHLIB_FLAGS = -shared -fPIC
  ifeq ($(UNAME), CYGWIN_NT-6.1-WOW64)
    SHLIB_EXT = dll
  else
    SHLIB_EXT = so
  endif
endif

SOURCES = ucaml.c
LIB = libucaml
RESULT = $(LIB).$(SHLIB_EXT)

MAJOR = 0
MINOR = 0

CC = gcc
CFLAGS = -g -Wall -fPIC -I../include
TRASH = *.o *.so *.dylib

all:$(RESULT)

clean:
	rm -f $(TRASH)

$(RESULT): $(SOURCES:%.c=%.o) 
	$(CC) -g $(SHLIB_FLAGS) -o $(LIB).$(MAJOR).$(MINOR).$(SHLIB_EXT) $<
	ln -sf $(LIB).$(MAJOR).$(SHLIB_EXT) $(RESULT)
	ln -sf $(LIB).$(MAJOR).$(MINOR).$(SHLIB_EXT) $(LIB).$(MAJOR).$(SHLIB_EXT)

%.o : %.c
	$(CC) $(CFLAGS) -c -o $*.o $<
