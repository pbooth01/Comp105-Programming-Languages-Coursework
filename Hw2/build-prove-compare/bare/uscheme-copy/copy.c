/* copy.c 283a */
#include "all.h"

/* private declarations for copying collection 283b */
static Value *fromspace, *tospace;    /* used only at GC time */
static int semispacesize;
                                     /* # of objects in fromspace and tospace */
/* private declarations for copying collection 283c */
static Value *hp, *heaplimit;                /* used for every allocation */
/* private declarations for copying collection 284b */
static void scanenv      (Env env);
static void scanexp      (Exp exp);
static void scanexplist  (Explist el);
static void scanframe    (Frame *fr);
static void scansource   (Source *s);
static void scantest     (UnitTest t);
static void scanloc      (Value *vp);
/* private declarations for copying collection 286a */
#define isinspace(LOC, SPACE) ((SPACE) <= (LOC) && (LOC) < (SPACE) +\
                                                                  semispacesize)
static Value *forward(Value *p);
/* private declarations for copying collection 758c */
static void collect(void);
/* copy.c 283d */
/* representation of [[struct Stack]] 727 */
struct Stack {
    int size;
    Frame *frames;  /* memory for 'size' frames */
    Frame *sp;      /* points to first unused frame */
};
/* copy.c 284a */
int nalloc;   /* OMIT */
Value* allocloc(void) {
    if (hp == heaplimit)
        collect();
    assert(hp < heaplimit);
    assert(isinspace(hp, fromspace)); /*runs after spaces are swapped*/ /*OMIT*/
    nalloc++;   /* OMIT */
    /* tell the debugging interface that [[hp]] is about to be allocated 290b */
    gc_debug_pre_allocate(hp);
    return hp++;
}
/* copy.c 284g */
static void scanenv(Env env) {
    for (; env; env = env->tl)
      { /*OMIT*/
        env->loc = forward(env->loc);
        assert(isinspace(env->loc, tospace)); /*OMIT*/
      } /*OMIT*/
}
/* copy.c 285a */
static void scanloc(Value *vp) {
    switch (vp->alt) {
    case NIL:
    case BOOL:
    case NUM:
    case SYM:
        return;
    case PAIR:
        vp->u.pair.car = forward(vp->u.pair.car);
        vp->u.pair.cdr = forward(vp->u.pair.cdr);
        return;
    case CLOSURE:
        scanexp(vp->u.closure.lambda.body);
        scanenv(vp->u.closure.env);
        return;
    case PRIMITIVE:
        return;
    default:
        assert(0);
        return;
    }
}
/* copy.c 285b */
static Value* forward(Value *p) {
    if (isinspace(p, tospace)) {
                          /* already in to space; must belong to scanned root */
        return p;
    } else {
        assert(isinspace(p, fromspace));
        /* forward pointer [[p]] and return the result 279b */
        if (p->alt == FORWARD) {            /* forwarding pointer */
            assert(isinspace(p->u.forward, tospace));   /* OMIT */
            return p->u.forward;
        } else {
            assert(isinspace(hp, tospace)); /* there is room */   /* OMIT */

    /* tell the debugging interface that [[hp]] is about to be allocated 290b */
            gc_debug_pre_allocate(hp);
            *hp = *p;
            *p  = mkForward(hp);
                                /* overwrite *p with a new forwarding pointer */
            assert(isinspace(p->u.forward, tospace)); /*extra*/   /* OMIT */
            return hp++;
        }
    }
    return NULL; /* appease a stupid compiler */  /*OMIT*/
}
/* copy.c 743a */
static void scanexp(Exp e) {
    switch (e->alt) {
    /* cases for [[scanexp]] 743b */
    case LITERAL:
        scanloc(&e->u.literal);
        return;
    case VAR:
        return;
    case IFX:
        scanexp(e->u.ifx.cond);
        scanexp(e->u.ifx.true);
        scanexp(e->u.ifx.false);
        return;
    case WHILEX:
        scanexp(e->u.whilex.cond);
        scanexp(e->u.whilex.body);
        return;
    case BEGIN:
        scanexplist(e->u.begin);
        return;
    case SET:
        scanexp(e->u.set.exp);
        return;
    case LETX:
        scanexplist(e->u.letx.el);
        scanexp(e->u.letx.body);
        return;
    case LAMBDAX:
        scanexp(e->u.lambdax.body);
        return;
    case APPLY:
        scanexp(e->u.apply.fn);
        scanexplist(e->u.apply.actuals);
        return;
    /* cases for [[scanexp]] 744a */
    case BREAKX:
        return;
    case CONTINUEX:
        return;
    case RETURNX:
        scanexp(e->u.returnx);
        return;
    case THROW:
        scanexp(e->u.throw);
        return;
    case TRY_CATCH:
        scanexp(e->u.try_catch.handler);
        scanexp(e->u.try_catch.body);
        return;
    /* cases for [[scanexp]] 744b */
    case HOLE:
    case WHILE_RUNNING_BODY:
        return;
    case LETXENV:
        scanenv(e->u.letxenv);
        return;
    case CALLENV:
        scanenv(e->u.callenv);
        return;
    }
    assert(0);
}
/* copy.c 744c */
static void scanframe(Frame *fr) {
    scanexp(&fr->context);
        if (fr->syntax != NULL)
            scanexp(fr->syntax);
}
/* copy.c 744d */
static void scanexplist(Explist el) {
    for (; el; el = el->tl)
        scanexp(el->hd);
}
/* copy.c 744e */
static void scansource(Source *s) {
    UnitTestlist ul;

    for (ul = s->tests; ul; ul = ul->tl)
        scantest(ul->hd);
}
/* copy.c 745a */
static void scantest(UnitTest t) {
    switch (t->alt) {
    case CHECK_EXPECT:
        scanexp(t->u.check_expect.check);
        scanexp(t->u.check_expect.expect);
        return;
    case CHECK_ERROR:
        scanexp(t->u.check_error);
        return;
    }
    assert(0);
}
/* copy.c ((prototype)) 758d */
/* you need to redefine these functions */
static void collect(void) { (void)scanframe; (void)scansource; assert(0); }
void printfinalstats(void) { assert(0); }
/* you need to initialize this variable */
int gc_uses_mark_bits;
