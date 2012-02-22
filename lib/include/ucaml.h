#ifndef __ucaml_h__
#define __ucaml_h__

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#if USE_GC

#include <gc.h>
#define D_MALLOC(size) GC_malloc(size)
#define D_FREE(p)

#else

#if DEBUG

void* debug_malloc(size_t size, char* file, int line);
void debug_free(void* p);

#define D_MALLOC(size) debug_malloc(size, __FILE__, __LINE__)
#define D_FREE(p) debug_free(p)
#else
#define D_MALLOC(size) malloc(size)
#define D_FREE(p) free(p)
#endif

#endif

typedef struct {
  void* p;
  int count;
} box_t;

typedef box_t* sp_t;

static inline sp_t new_sp(int size)
{
    box_t* b = D_MALLOC(sizeof(box_t));
    assert(b != NULL);

    b->p = D_MALLOC(size);
    assert(b->p != NULL);

    b->count = 1;
    return b;
}

#define sp_get(b) ((b)->p)
#define sp_count(b) ((b)->count)

static inline void add_ref(sp_t p)
{
    ++sp_count(p);
}

static inline void release(sp_t p)
{
    if (--sp_count(p) == 0) {
        D_FREE(sp_get(p));
        D_FREE(p);
    }
}

static inline void print_int(int x) { 
  printf("%d", x); 
}

#endif
