/* parse.c 693c */
#include "all.h"

/* parse.c declarations 694d */
static Namelist getnamelist(Name f, Par p, Parlist pl);
/* parse.c declarations 696c */
static Exp parseexp(Par);
/* parse.c 694a */
static XDef parse(Par p) {
    switch (p->alt) {
    case ATOM:
        /* parse [[atom]] and return the result 694b */
        return mkDef(mkExp(parseexp(p)));
    case LIST:
        /* parse [[list]] and return the result 694c */
        {
            Name first;
            Parlist pl = p->u.list;
            if (pl == NULL)
                error("%p: empty list", p);
            if (nthPL(pl, 0)->alt != ATOM)
                error("%p: first item of list not name", p);

            first = nthPL(pl, 0)->u.atom;
             if (first == strtoname("define")) {
                 /* parse [[define]] and return the result */
                 if (lengthPL(pl) != 5 || nthPL(pl, 1)->alt != ATOM || 
                     nthPL(pl, 2)->alt != LIST || nthPL(pl, 3)->alt != LIST)
                     error("%p: usage: (define fun (formals) (locals) body)", p);
               {
                 Name     name    = nthPL(pl, 1)->u.atom;
                 Namelist formals = getnamelist(name, p, nthPL(pl, 2)->u.list);
                 Namelist locals  = getnamelist(name, p, nthPL(pl, 3)->u.list);
                 Exp      body    = parseexp(nthPL(pl, 4));
                 return   mkDef(mkDefine(name, mkUserfun(formals, locals, body)));
               }
             }

            if (first == strtoname("val")) {
                /* parse [[val]] and return the result 695c */
                Exp var, exp;

                if (lengthPL(pl) != 3)
                    error("%p: usage: (val var exp)", p);

                var = parseexp(nthPL(pl, 1));
                if (var->alt != VAR)
                    error("%p: usage: (val var exp) (bad variable)", p);

                exp = parseexp(nthPL(pl, 2));
                return mkDef(mkVal(var->u.var, exp));
            }
            if (first == strtoname("use")) {
                /* parse [[use]] and return the result 695d */
                if (lengthPL(pl) != 2 || nthPL(pl, 1)->alt != ATOM)
                    error("%p: usage: (use filename)", p);

                return mkUse(nthPL(pl, 1)->u.atom);
            }
            if (first == strtoname("check-expect")) {
                /* parse [[check-expect]] and return the result 695e */
                Exp check, expect;

                if (lengthPL(pl) != 3)
                    error("%p: usage: (check-expect exp exp)", p);

                check  = parseexp(nthPL(pl, 1));
                expect = parseexp(nthPL(pl, 2));
                return mkTest(mkCheckExpect(check, expect));
            }
            if (first == strtoname("check-error")) {
                /* parse [[check-error]] and return the result 696a */
                Exp check;

                if (lengthPL(pl) != 2)
                    error("%p: usage: (check-error exp)", p);

                check  = parseexp(nthPL(pl, 1));
                return mkTest(mkCheckError(check));
            }
            return mkDef(mkExp(parseexp(p)));
        }
    }
    assert(0);
    return NULL;
}
/* parse.c 695a */
static Namelist getnamelist(Name f, Par p, Parlist pl) {
    if (pl == NULL)
        return NULL;
    if (pl->hd->alt != ATOM)
        error("%p: formal-parameter list of function %n contains "
              "something that is not a name", p, f);
    return mkNL(pl->hd->u.atom, getnamelist(f, p, pl->tl));
}
/* parse.c 696b */
static Explist parselist(Parlist pl) {
    Exp e;

    if (pl == NULL)
        return NULL;

    e = parseexp(pl->hd); /* force pl->hd to be parsed first */
    return mkEL(e, parselist(pl->tl));
}
/* parse.c 696d */
static Exp parseexp(Par p) {
    switch (p->alt) {
    case ATOM:
        /* parseexp [[atom]] and return the result 696e */
        {
            const char *s = nametostr(p->u.atom);
            char *t;
            long l = strtol(s, &t, 10);
            if (*t == '\0') /* the number is the whole string */
                return mkLiteral(l);
            else
                return mkVar(p->u.atom);
        }
    case LIST:
        /* parseexp [[list]] and return the result 697a */
        {
            Parlist pl;
            Name first;
            Explist argl;

            pl = p->u.list;
            if (pl == NULL)
                error("%p: empty list in input", p);
            if (pl->hd->alt != ATOM)
                error("%p: first item of list not name", p);

            first = pl->hd->u.atom;
            argl  = parselist(pl->tl);
            if (first == strtoname("begin")) {
                /* parseexp [[begin]] and return the result 697b */
                return mkBegin(argl);
            } else if (first == strtoname("if")) {
                /* parseexp [[if]] and return the result 697c */
                if (lengthEL(argl) != 3)
                    error("%p: usage: (if cond true false)", p);
                return mkIfx(nthEL(argl, 0), nthEL(argl, 1), nthEL(argl, 2));
            } else if (first == strtoname("set")) {
                /* parseexp [[set]] and return the result 697e */
                if (lengthEL(argl) != 2)
                    error("%p: usage: (set var exp)", p);
                if (nthEL(argl, 0)->alt != VAR)
                    error("%p: set needs variable as first param", p);
                return mkSet(nthEL(argl, 0)->u.var, nthEL(argl, 1));
            } else if (first == strtoname("while")) {
                /* parseexp [[while]] and return the result 697d */
                if (lengthEL(argl) != 2)
                    error("%p: usage: (while cond body)", p);
                return mkWhilex(nthEL(argl, 0), nthEL(argl, 1));
            } else {
                /* parseexp function application and return the result 698a */
                return mkApply(first, argl);
            }
        }
    }
    assert(0);
    return NULL;
}
/* parse.c 698b */
struct XDefreader {
    Prompts prompts;
    Reader r;
    Parlist pl;
};
/* parse.c 698c */
XDef readxdef(XDefreader dr) {
    Par p;

    if (dr->pl == NULL) {
        dr->pl = readparlist(dr->r, 0, dr->prompts);
        if (dr->pl == NULL) 
            return NULL;
    }

    p      = dr->pl->hd;
    dr->pl = dr->pl->tl;
    return parse(p);
}
/* parse.c 698d */
XDefreader xdefreader(Reader r, Prompts prompts) {
    XDefreader dr = malloc(sizeof(*dr));
    assert(dr != NULL);
    dr->r       = r;
    dr->prompts = prompts;
    dr->pl      = NULL;
    return dr;
}
