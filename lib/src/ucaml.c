#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include "ucaml.h"

enum {
  NUM_OF_SLOTS = 256,
};

typedef struct {
  void* p;
  char* file;
  int line;
} meminfo_t;

static meminfo_t* slots[NUM_OF_SLOTS] = { 0 };

void* debug_malloc(size_t size, char* file, int line)
{
  int i;
  
  void* p = malloc(size);
  assert(p != NULL);

  for (i = 0; i < NUM_OF_SLOTS; ++i) {
    if (slots[i] == NULL) {
      meminfo_t* m = malloc(sizeof(meminfo_t));
      assert(m != NULL);

      m->p = p;
      m->file = strdup(file);
      m->line = line;
      
      slots[i] = m;
      break;
    }
  }

  assert(i != NUM_OF_SLOTS);
  return p;
}

void debug_free(void* p)
{
  int i;
  
  for (i = 0; i < NUM_OF_SLOTS; ++i) {
    if (slots[i] && slots[i]->p == p) {

      free(slots[i]->p);
      free(slots[i]->file);
      
      free(slots[i]);
      slots[i] = NULL;
      break;
    }
  }
  
  assert(i != NUM_OF_SLOTS);
}

void __attribute__ ((constructor)) ucaml_init(void)
{
}

void __attribute__ ((destructor)) ucaml_fini(void)
{
  int i;
  for (i = 0; i < NUM_OF_SLOTS; ++i) {
    if (slots[i] != NULL) {
      printf("%p leaks. It was allocated at %s : %d.\n", slots[i]->p, slots[i]->file, slots[i]->line);
    }
  }
}
