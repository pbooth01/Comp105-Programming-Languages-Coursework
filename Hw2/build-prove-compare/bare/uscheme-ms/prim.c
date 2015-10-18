/* prim.c 150a */
#include "all.h"

static Primitive arith, binary, unary;
/* prim.c 150b */
enum {
  #define xx(NAME, TAG, FUNCTION) TAG,
  #include "prim.h"
  #undef xx
  UNUSED_TAG
};
/* prim.c 150c */
void addprimitives(Env *envp) {
    /* precondition: *envp must be the GC global environment roots.globals */
    #define xx(NAME, TAG, FUNCTION) \
        *envp = bindalloc(strtoname(NAME), mkPrimitive(TAG, FUNCTION), *envp);
    #include "prim.h"
    #undef xx
}
/* prim.c 151a */
static int projectint(Exp e, Value v) {
    if (v.alt != NUM)
        error("in %e, expected an integer, but got %v", e, v);
    return v.u.num;
}
/* prim.c 151c */
static int divide(int n, int m) {
    if (n >= 0)
        if (m >= 0)
            return n / m;
        else
            return -(( n - m - 1) / -m);
    else
        if (m >= 0)
            return -((-n + m - 1) /  m);
        else
            return -n / -m;
}
/* prim.c 152a */
static Value arith(Exp e, int tag, Valuelist args) {
    int n, m;

    checkargc(e, 2, lengthVL(args));
    n = projectint(e, nthVL(args, 0));
    m = projectint(e, nthVL(args, 1));

    switch (tag) {
    case PLUS:
        return mkNum(n + m);
    case MINUS:
        return mkNum(n - m);
    case TIMES:
        return mkNum(n * m);
    case DIV:
        if (m==0)
            error("division by zero");
        return mkNum(divide(n, m));
    case LT:
        return mkBool(n < m);
    case GT:
        return mkBool(n > m);
    default:
        assert(0);
        return falsev;
    }
}
/* prim.c 153a */
static Value binary(Exp e, int tag, Valuelist args) {
    Value v, w;

    checkargc(e, 2, lengthVL(args));
    v = nthVL(args, 0);
    w = nthVL(args, 1);

    switch (tag) {
    case CONS:
        /* return cons cell containing [[v]] and [[w]] 153b */
        {
            Value pair;
            Value *car;

            pushreg(&v);
            pushreg(&w);
            car = allocate(v);
            pair = mkPair(car, car); /* temporary; preserves invariant */
            car = NULL;
            pushreg(&pair);
            pair.u.pair.cdr = allocate(w);
            popreg(&pair);
            popreg(&w);
            popreg(&v);
            cyclecheck(&pair);
            return pair;
        }
    case EQ: 
        /* return [[(= v w)]] 153c */
        if (v.alt != w.alt)
            return falsev;

        switch (v.alt) {
        case NUM:
            return mkBool(v.u.num  == w.u.num);
        case BOOL:
            return mkBool(v.u.bool == w.u.bool);
        case SYM:
            return mkBool(v.u.sym  == w.u.sym);
        case NIL:
            return truev;
        default:
            return falsev;
        }
    }
    assert(0);
    return falsev; /* not reached */
}
/* prim.c 154b */
static Value unary(Exp e, int tag, Valuelist args) {
    Value v;

    checkargc(e, 1, lengthVL(args));
    v = nthVL(args, 0);
    switch (tag) {
    case NULLP:
        return mkBool(v.alt == NIL);
    case BOOLEANP:
        return mkBool(v.alt == BOOL);
    case NUMBERP:
        return mkBool(v.alt == NUM);
    case SYMBOLP:
        return mkBool(v.alt == SYM);
    case PAIRP:
        return mkBool(v.alt == PAIR);
    case PROCEDUREP:
        return mkBool(v.alt == CLOSURE || v.alt == PRIMITIVE);
    case CAR:
        if (v.alt != PAIR)
            error("car applied to non-pair %v in %e", v, e);
        return *v.u.pair.car;
    case CDR:
        if (v.alt != PAIR)
            error("cdr applied to non-pair %v in %e", v, e);
        return *v.u.pair.cdr;
    case PRINT:
        print("%v\n", v);
        return v;
    case ERROR:
        error("%v", v);
        return v;
    default:
        assert(0);
        return falsev; /* not reached */
    }
}
