#ifndef __ucaml_base_h__
#define __ucaml_base_h__

#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <assert.h>

#if USE_GC

#include <gc.h>
#define D_MALLOC(size) GC_malloc(size)
#define D_FREE(p) 

#define debug_malloc(size, file, line) GC_malloc(size)
#define debug_free(p)

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
    bool (*ref_equal)(struct ref_base_t* , struct ref_base_t* );
} ref_base_t;

#if DEBUG

static inline ref_base_t* debug_new_ref_base(int size, void (*destructor)(ref_base_t*), bool (*ref_equal)(ref_base_t*, ref_base_t*), const char* file, int line)
{
    ref_base_t* p = (ref_base_t*)debug_malloc(size, file, line);
    p->count = 1;
    p->destructor = destructor;
    p->ref_equal = ref_equal;
    return p;
}

static inline void debug_delete_ref_base(ref_base_t* base)
{
    if (base->destructor) {
        base->destructor(base);
    }
    debug_free(base);
}

#define new_ref_base(size, destructor, ref_equal) debug_new_ref_base(size, destructor, ref_equal, __FILE__, __LINE__)
#define delete_ref_base(base) debug_delete_ref_base(base)

#else

static inline ref_base_t* new_ref_base(int size, void (*destructor)(ref_base_t*), bool (*ref_equal)(ref_base_t*, ref_base_t*))
{
    ref_base_t* p = (ref_base_t*)D_MALLOC(size);
    p->count = 1;
    p->destructor = destructor;
    p->ref_equal = ref_equal;
    return p;
}

static inline void delete_ref_base(ref_base_t* base)
{
    if (base->destructor) {
        base->destructor(base);
    }
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
#if !(USE_GC)
    box_t* b = (box_t*)p;
    D_FREE(b->p);
#endif
}

static inline sp_t new_box(int size, bool (*ref_equal)(ref_base_t*, ref_base_t*))
{
    box_t* b = (box_t*)new_ref_base(sizeof(box_t), delete_box, ref_equal);

    b->p = D_MALLOC(size);
    assert(b->p != NULL);

    return (sp_t)b;
}

static inline void* sp_get(sp_t p)
{
    assert(((ref_base_t*)p)->count != 0);
    return ((box_t*)p)->p;
}

static inline bool ref_equal_bool(ref_base_t* a, ref_base_t* b)
{
    return *(bool*)a = *(bool*)b;
}

static inline sp_t wrap_bool(bool b)
{
    sp_t p;

    p = new_box(sizeof(bool), ref_equal_bool);
    *((bool*)sp_get(p)) = b;
    return p;
}

static inline int unwrap_bool(sp_t p)
{
    return *((bool*)sp_get(p));
}

static inline int unwrap_int(sp_t p)
{
    return *((int*)sp_get(p));
}

static inline bool ref_equal_int(ref_base_t* a, ref_base_t* b)
{
    return unwrap_int(a) == unwrap_int(b);
}

static inline sp_t wrap_int(int n)
{
    sp_t p;

    p = new_box(sizeof(int), ref_equal_int);
    *((int*)sp_get(p)) = n;
    return p;
}

#define equal_r(a, b) (((ref_base_t*)(a))->ref_equal((ref_base_t*)(a), (ref_base_t*)(b)))

#endif
