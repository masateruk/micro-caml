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

void* debug_malloc(size_t size, const char* file, int line);
void debug_free(void* p);

#define D_MALLOC(size) debug_malloc(size, __FILE__, __LINE__)
#define D_FREE(p) debug_free(p)
#else
#define D_MALLOC(size) malloc(size)
#define D_FREE(p) free(p)
#endif

#endif

typedef struct ref_base_t {
    int count;
    void (*destructor)(struct ref_base_t* );
} ref_base_t;

#if DEBUG

static inline ref_base_t* debug_new_ref_base(int size, void (*destructor)(ref_base_t*), const char* file, int line)
{
    ref_base_t* p = (ref_base_t*)debug_malloc(size, file, line);
    p->count = 1;
    p->destructor = destructor;
    return p;
}

static inline void debug_delete_ref_base(ref_base_t* base)
{
    base->destructor(base);
    debug_free(base);
}

#define new_ref_base(size, destructor) debug_new_ref_base(size, destructor, __FILE__, __LINE__)
#define delete_ref_base(base) debug_delete_ref_base(base)

#else

static inline ref_base_t* new_ref_base(int size, void (*destructor)(ref_base_t*))
{
    ref_base_t* p = (ref_base_t*)D_MALLOC(size);
    p->count = 1;
    p->destructor = destructor;
    return p;
}

static inline void delete_ref_base(ref_base_t* base)
{
    base->destructor(base);
    D_FREE(base);
}

#endif

static inline void ref_base_add_ref(ref_base_t* base)
{
    assert(base->count != 0);
    base->count++;
}

static inline void ref_base_release(ref_base_t* base)
{
    if (--base->count == 0) {
        delete_ref_base(base);
    }
}

#define add_ref(p) ref_base_add_ref((ref_base_t*)p)
#define release(p) ref_base_release((ref_base_t*)p)

typedef struct {
    ref_base_t base;
    void* p;
} box_t;

typedef ref_base_t* sp_t;

static inline void delete_box(sp_t p)
{
    box_t* b = (box_t*)p;
    D_FREE(b->p);
}

static inline sp_t new_box(int size)
{
    box_t* b = (box_t*)new_ref_base(sizeof(box_t), delete_box);

    b->p = D_MALLOC(size);
    assert(b->p != NULL);

    return (sp_t)b;
}

static inline void* sp_get(sp_t p)
{
    assert(((ref_base_t*)p)->count != 0);
    return ((box_t*)p)->p;
}

static inline void print_int(int x)
{ 
    printf("%d", x); 
}

#endif
