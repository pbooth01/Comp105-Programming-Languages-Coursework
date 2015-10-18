/* ms.c 273a */
#include "all.h"

/* private declarations for mark-and-sweep collection 273b */
typedef struct Mvalue Mvalue;
struct Mvalue {
    Value v;
    unsigned live:1;
};
/* private declarations for mark-and-sweep collection 273d */
#ifndef GCHYPERDEBUG /*OMIT*/
#define GROWTH_UNIT 24\
                    /* increment in which the heap grows, measured in objects */
#else /*OMIT*/
#define GROWTH_UNIT 3 /*OMIT*/
#endif /*OMIT*/
typedef struct Page Page;
struct Page {
    Mvalue pool[GROWTH_UNIT];
    Page *tl;
};
/* private declarations for mark-and-sweep collection 273e */
Page *pagelist, *curpage;
Mvalue *hp, *heaplimit;
/* private declarations for mark-and-sweep collection 275a */
static void visitloc          (Value *loc);
static void visitvalue        (Value v);
static void visitenv          (Env env);
static void visitexp          (Exp exp);
static void visitexplist      (Explist el);
static void visitframe        (Frame *fr);
static void visitstack        (Stack s);
static void visitsource       (Source *s);
static void visitsourcelist   (Sourcelist ss);
static void visittest         (UnitTest t);
static void visitregister     (Register reg);
static void visitregisterlist (Registerlist regs);
static void visitroots        (void);
/* private declarations for mark-and-sweep collection 925a */
static int nalloc;              /* total number of allocations */
static int ncollections;        /* total number of collections */
static int nmarks;              /* total number of cells marked */
/* ms.c 273c */
int gc_uses_mark_bits = 1;
/* ms.c 274a */
static void makecurrent(Page *page) {
    assert(page != NULL);
    curpage = page;
    hp = &page->pool[0];
    heaplimit = &page->pool[GROWTH_UNIT];
}
/* ms.c 274b */
static int heapsize;            /* OMIT */
static void addpage(void) {
    Page *page = calloc(1, sizeof(*page));
    assert(page != NULL);

/* tell the debugging interface that each object on [[page]] has been acquired 289h */
    {   unsigned i;
        for (i = 0; i < sizeof(page->pool)/sizeof(page->pool[0]); i++)
            gc_debug_post_acquire(&page->pool[i].v, 1);
    }

    if (pagelist == NULL) {
        pagelist = page;
    } else {
        assert(curpage != NULL && curpage->tl == NULL);
        curpage->tl = page;
    }
    makecurrent(page);
    heapsize += GROWTH_UNIT;   /* OMIT */
}
/* ms.c ((prototype)) 274c */
Value* allocloc(void) {
    if (hp == heaplimit)
        addpage();
    assert(hp < heaplimit);

/* tell the debugging interface that [[&hp->v]] is about to be allocated 290a */
    gc_debug_pre_allocate(&hp->v);
    return &(hp++)->v;
}
/* ms.c 275c */
static void visitenv(Env env) {
    for (; env; env = env->tl)
        visitloc(env->loc);
}
/* ms.c ((prototype)) 275d */
static void visitloc(Value *loc) {
    Mvalue *m = (Mvalue*) loc;
    if (!m->live) {
        m->live = 1;
        visitvalue(m->v);
    }
}
/* ms.c 276a */
static void visitregister(Value *reg) {
    visitvalue(*reg);
}
/* ms.c 276b */
static void visitvalue(Value v) {
    switch (v.alt) {
    case NIL:
    case BOOL:
    case NUM:
    case SYM:
    case PRIMITIVE:
        return;
    case PAIR:
        visitloc(v.u.pair.car);
        visitloc(v.u.pair.cdr);
        return;
    case CLOSURE:
        visitexp(v.u.closure.lambda.body);
        visitenv(v.u.closure.env);
        return;
    default:
        assert(0);
        return;
    }
    assert(0);
}
/* ms.c 740a */
static void visitexp(Exp e) {
    switch (e->alt) {
    /* cases for [[visitexp]] 740b */
    case LITERAL:
        visitvalue(e->u.literal);
        return;
    case VAR:
        return;
    case IFX:
        visitexp(e->u.ifx.cond);
        visitexp(e->u.ifx.true);
        visitexp(e->u.ifx.false);
        return;
    case WHILEX:
        visitexp(e->u.whilex.cond);
        visitexp(e->u.whilex.body);
        return;
    case BEGIN:
        visitexplist(e->u.begin);
        return;
    case SET:
        visitexp(e->u.set.exp);
        return;
    case LETX:
        visitexplist(e->u.letx.el);
        visitexp(e->u.letx.body);
        return;
    case LAMBDAX:
        visitexp(e->u.lambdax.body);
        return;
    case APPLY:
        visitexp(e->u.apply.fn);
        visitexplist(e->u.apply.actuals);
        return;
    /* cases for [[visitexp]] 741a */
    case BREAKX:
        return;
    case CONTINUEX:
        return;
    case RETURNX:
        visitexp(e->u.returnx);
        return;
    case THROW:
        visitexp(e->u.throw);
        return;
    case TRY_CATCH:
        visitexp(e->u.try_catch.handler);
        visitexp(e->u.try_catch.body);
        return;
    /* cases for [[visitexp]] 741b */
    case WHILE_RUNNING_BODY:
        visitexp(e->u.whilex.cond);
        visitexp(e->u.whilex.body);
        return;
    case LETXENV:
        visitenv(e->u.letxenv);
        return;
    case CALLENV:
        visitenv(e->u.callenv);
        return;
    case HOLE:
        return;
    }
    assert(0);
}
/* ms.c 741c */
static void visitexplist(Explist el) {
    for (; el; el = el->tl)
        visitexp(el->hd);
}
/* ms.c 741d */
static void visitregisterlist(Registerlist regs) {
    for ( ; regs != NULL; regs = regs->tl)
        visitregister(regs->hd);
}
/* ms.c 741e */
/* representation of [[struct Stack]] 727 */
struct Stack {
    int size;
    Frame *frames;  /* memory for 'size' frames */
    Frame *sp;      /* points to first unused frame */
};
static void visitstack(Stack s) {
    Frame *fr;
    for (fr = s->frames; fr < s->sp; fr++) {
        visitframe(fr);
    }
}
/* ms.c 742a */
static void visitframe(Frame *fr) {
    visitexp(&fr->context);
    if (fr->syntax != NULL)
        visitexp(fr->syntax);
}
/* ms.c 742b */
static void visitsource(Source *s) {
    UnitTestlist ul;

    for (ul = s->tests; ul; ul = ul->tl)
        visittest(ul->hd);
}
/* ms.c 742c */
static void visitsourcelist(Sourcelist ss) {
    for ( ; ss; ss = ss->tl)
        visitsource(&ss->hd);
}
/* ms.c 742d */
static void visittest(UnitTest t) {
    switch (t->alt) {
    case CHECK_EXPECT:
        visitexp(t->u.check_expect.check);
        visitexp(t->u.check_expect.expect);
        return;
    case CHECK_ERROR:
        visitexp(t->u.check_error);
        return;
    }
    assert(0);
}
/* ms.c 742e */
static void visitroots(void) {
    visitenv(*roots.globals);
    visitsourcelist(roots.sources);
    visitstack(roots.stack);
    visitregisterlist(roots.registers);
}
/* ms.c ((prototype)) 758e */
/* you need to redefine these functions */
void printfinalstats(void) { 
  (void)nalloc; (void)ncollections; (void)nmarks;
  assert(0); 
}
/* ms.c ((prototype)) 758f */
void avoid_unpleasant_compiler_warnings(void) {
    (void)visitroots;
}
