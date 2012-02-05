#ifndef __ucaml_h__
#define __ucaml_h__

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>
#include <gc.h>

typedef struct {
  void* p;
} box_t;

static inline void print_int(int x) { 
  printf("%d", x); 
}

#endif

