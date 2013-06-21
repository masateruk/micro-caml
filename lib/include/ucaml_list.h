#ifndef __ucaml_list_h__
#define __ucaml_list_h__

#include "ucaml_base.h"

enum {
    NIL,
    CONS
};

typedef struct list {
    ref_base_t base;
    int type;
    union {
        struct Cons {
            sp_t head;
            struct list* tail;
        } Cons;
    } u;
} list;

static inline void destruct_list(ref_base_t* base)
{
    list* p = (list*)base;

    if (p->type == NIL) {
        ;
    } else if (p->type == CONS) {
        release(p->u.Cons.head);
        release(p->u.Cons.tail);
    } else {
        assert(false);
    }
}

static inline bool ref_equal_list(ref_base_t* a, ref_base_t* b)
{
    list* la = (list*)a;
    list* lb = (list*)b;

    return la->type == lb->type && 
        ((la->type != CONS) || (la->u.Cons.head->ref_equal(la->u.Cons.head, lb->u.Cons.head) && ref_equal_list((ref_base_t*)la->u.Cons.tail, (ref_base_t*)lb->u.Cons.tail)));
}

static inline list* Cons(sp_t _p16, list* _p17)
{
    list* p = (list*)new_ref_base(sizeof(list), destruct_list, ref_equal_list);

    p->type = CONS;
    p->u.Cons.head = _p16;
    add_ref(p->u.Cons.head);
    p->u.Cons.tail = _p17;
    add_ref(p->u.Cons.tail);
    return p;
}

static inline list* Nil()
{
    list* p = (list*)new_ref_base(sizeof(list), destruct_list, ref_equal_list);

    p->type = NIL;
    return p;
}

#endif /* __ucaml_list_h__ */
