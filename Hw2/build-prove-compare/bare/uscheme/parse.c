/* parse.c 718a */
#include "all.h"

/* parse.c declarations 720a */
Namelist getnamelist(Par p, Parlist pl);
/* parse.c declarations 720c */
Explist parselist(Parlist);
/* parse.c declarations 720e */
Exp parseexp(Par);
/* parse.c declarations 723b */
static void parseletbindings(Par p, Parlist bindings, Exp letexp);
/* parse.c declarations 724a */
Value parsesx(Par p);
/* parse.c 718b */
XDef parse(Par p) {
    switch (p->alt) {
    case ATOM:
        /* parse ATOM and return 718c */
        return mkDef(mkExp(parseexp(p)));
    case LIST:
        /* parse LIST and return 718d */
        {
            Parlist pl = p->u.list;

            if (pl == NULL)
                error("%p: empty list", p);
            if (nthPL(pl, 0)->alt == ATOM) {
                Name first = nthPL(pl, 0)->u.atom;
                if (first == strtoname("define")) {
                    /* parse define and return 719a */
                    Name name;
                    Lambda l;

                    if (lengthPL(pl) != 4 || nthPL(pl, 1)->alt != ATOM || nthPL(
                                                            pl, 2)->alt != LIST)
                        error("%p: usage: (define fun (args) body)", p);

                    name      = nthPL(pl, 1)->u.atom;
                    l.formals = getnamelist(p, nthPL(pl, 2)->u.list);
                    l.body    = parseexp(nthPL(pl, 3));
                    return      mkDef(mkDefine(name, l));
                }
                if (first == strtoname("val")) {
                    /* parse val and return 719b */
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
                    /* parse use and return 719c */
                    if (lengthPL(pl) != 2 || nthPL(pl, 1)->alt != ATOM)
                        error("%p: usage: (use filename)", p);

                    return mkUse(nthPL(pl, 1)->u.atom);
                }
                if (first == strtoname("check-expect")) {
                    /* parse check-expect and return 719d */
                    {
                    Exp check, expect;

                    if (lengthPL(pl) != 3)
                        error("%p: usage: (check-expect exp exp)", p);

                    check  = parseexp(nthPL(pl, 1));
                    expect = parseexp(nthPL(pl, 2));
                    return mkTest(mkCheckExpect(check, expect));
                    }
                }
                if (first == strtoname("check-error")) {
                    /* parse check-error and return 719e */
                    {
                    Exp check;

                    if (lengthPL(pl) != 2)
                        error("%p: usage: (check-error exp)", p);

                    check  = parseexp(nthPL(pl, 1));
                    return mkTest(mkCheckError(check));
                    }
                }
            }

            return mkDef(mkExp(parseexp(p)));
        }
    }
    assert(0);
    return NULL;
}
/* parse.c 720b */
Namelist getnamelist(Par p, Parlist pl) {
    if (pl == NULL)
        return NULL;
    if (pl->hd->alt != ATOM)
        error("%p: formal parameter list contains %p, which is not a name", p,
                                                                        pl->hd);
    return mkNL(pl->hd->u.atom, getnamelist(p, pl->tl));
}
/* parse.c 720d */
Explist parselist(Parlist pl) {
    Exp e;

    if (pl == NULL)
        return NULL;

    e = parseexp(pl->hd);
    return mkEL(e, parselist(pl->tl));
}
/* parse.c 720f */
Exp parseexp(Par p) {
    switch (p->alt) {
    case ATOM:
        /* parseexp [[ATOM]] and return 721 */
        {
            Name n = p->u.atom;
            const char *s; /* string form of n */
            char *t;       /* nondigits in s, if any */
            long l;        /* number represented by s, if any */

            if (n == strtoname("#t"))
                return mkLiteral(truev);
            else if (n == strtoname("#f"))
                return mkLiteral(falsev);

            s = nametostr(n);
            l = strtol(s, &t, 10);
            if (*t == '\0' && *s != '\0')
                                /* all the characters in s are digits base 10 */
                return mkLiteral(mkNum(l));
            else
                return mkVar(n);
        }
    case LIST:
        /* parseexp [[LIST]] and return 722a */
        {
            Parlist pl;                /* parenthesized list we are parsing */
            Name first;
                            /* first element, as a name (or NULL if not name) */
            Explist el;                /* remaining elements, as expressions */
            Exp rv;                    /* result of parsing */

            pl = p->u.list;
            if (pl == NULL)
                error("%p: empty list in input", p);

            first = pl->hd->alt == ATOM ? pl->hd->u.atom : NULL;
            if (first == strtoname("lambda")) {
                /* parseexp [[lambda]] and put the result in [[rv]] 722b */
                Par q;

                if (lengthPL(pl->tl) != 2)
                    error("%p: usage: (lambda (formals) exp)", p);
                q = nthPL(pl->tl, 0);
                if (q->alt != LIST)
                    error("%p: usage: (lambda (formals) exp)", p);
                rv = mkLambdax(mkLambda(getnamelist(p, q->u.list), parseexp(
                                                            nthPL(pl->tl, 1))));
            } else if (first == strtoname("let")
                   ||  first == strtoname("let*")
                   ||  first == strtoname("letrec")) {
                /* parseexp let and put the result in [[rv]] 723a */
                Letkeyword letword;
                Par letbindings;

                if (first == strtoname("let"))
                    letword = LET;
                else if (first == strtoname("let*"))
                    letword = LETSTAR;
                else if (first == strtoname("letrec"))
                    letword = LETREC;
                else
                    assert(0);

                if (lengthPL(pl->tl) != 2)
                    error("%p: usage: (%n (letlist) exp)", p, first);

                letbindings = nthPL(pl->tl, 0);
                if (letbindings->alt != LIST)
                    error("%p: usage: (%n (letlist) exp)", p, first);

                rv = mkLetx(letword, NULL, NULL, parseexp(nthPL(pl->tl, 1)));

                parseletbindings(p, letbindings->u.list, rv);
            } else if (first == strtoname("quote")) {
                /* parseexp [[quote]] and put the result in [[rv]] 723d */
                {
                    if (lengthPL(pl) != 2)
                        error("%p: quote needs exactly one argument", p);
                    rv = mkLiteral(parsesx(nthPL(pl, 1)));
                }
            } else {
                el = parselist(pl->tl);
                if (first == strtoname("begin")) {
                    /* parseexp [[begin]] and put the result in [[rv]] 724e */
                    rv = mkBegin(el);
                } else if (first == strtoname("if")) {
                    /* parseexp [[if]] and put the result in [[rv]] 724f */
                    if (lengthEL(el) != 3)
                        error("%p: usage: (if cond true false)", p);
                    rv = mkIfx(nthEL(el, 0), nthEL(el, 1), nthEL(el, 2));
                } else if (first == strtoname("set")) {
                    /* parseexp [[set]] and put the result in [[rv]] 725b */
                    if (lengthEL(el) != 2)
                        error("%p: usage: (set var exp)", p);
                    if (nthEL(el, 0)->alt != VAR)
                        error("%p: set needs variable as first param", p);
                    rv = mkSet(nthEL(el, 0)->u.var, nthEL(el, 1));
                } else if (first == strtoname("while")) {
                    /* parseexp [[while]] and put the result in [[rv]] 725a */
                    if (lengthEL(el) != 2)
                        error("%p: usage: (while cond body)", p);
                    rv = mkWhilex(nthEL(el, 0), nthEL(el, 1));

 /* [[RBR else LBR]] possibly parse expressions that are in \uschemeplus 723e */
                  /* nothing happens */
                } else {
                   /* parseexp application and put the result in [[rv]] 724d */
                   rv = mkApply(parseexp(pl->hd), el);
                }
            }
            return rv;
        }
    default:
        assert(0);
        return NULL;
    }
}
/* parse.c 723c */
static void parseletbindings(Par p, Parlist bindings, Exp letexp) {
    if (bindings) {
        Par t = bindings->hd;
        Name n;  /* name bound in t (if t is well formed) */
        Exp e;   /* expression on RHS of t (if t is well formed) */
        parseletbindings(p, bindings->tl, letexp);
        if (t->alt != LIST || lengthPL(t->u.list) != 2 
        ||  nthPL(t->u.list, 0)->alt != ATOM)
            error("%p: usage: (letX (letlist) exp)", p);
        n = nthPL(t->u.list, 0)->u.atom;
        e = parseexp(nthPL(t->u.list, 1));
        letexp->u.letx.nl = mkNL(n, letexp->u.letx.nl);
        letexp->u.letx.el = mkEL(e, letexp->u.letx.el);
    }
}
/* parse.c 724b */
Value parsesx(Par p) {
    switch (p->alt) {
    case ATOM:
        {
            Name n        = p->u.atom;
            const char *s = nametostr(n);
            long l;            /* value of digits in s, if any */
            char *t;           /* first nondigit in s */

            l = strtol(s, &t, 10);
            if (*t == '\0' && *s != '\0')  /* s is all digits */
                return mkNum(l);
            else if (strcmp(s, "#t") == 0)
                return truev;
            else if (strcmp(s, "#f") == 0)
                return falsev;
            else if (strcmp(s, ".") == 0)
                error("this interpreter cannot handle . in quoted S-expressions"
                                                                              );
            else
                return mkSym(n);
        }
    case LIST:
        /* parsesx [[LIST]] and return 724c */
        if (p->u.list == NULL)
            return mkNil();
        else
            return mkPair(allocate(parsesx(p->u.list->hd)),
                          allocate(parsesx(mkList(p->u.list->tl))));
    }
    assert(0);
    return falsev;
}
/* parse.c 725c */
struct XDefreader {
    Prompts prompts;  /* whether to prompt at each definition */
    Reader r;      /* underlying reader of Pars */
    Parlist pl;    /* Pars read but not yet parsed */
};
/* parse.c 725d */
XDef readxdef(XDefreader dr) {
    Par p;

    if (dr->pl == NULL) {
        dr->pl = readparlist(dr->r, 1, dr->prompts);
        if (dr->pl == NULL) 
            return NULL;
    }

    p = dr->pl->hd;
    dr->pl = dr->pl->tl;
    return parse(p);
}
/* parse.c 725e */
XDefreader xdefreader(Reader r, Prompts prompts) {
    XDefreader dr;

    dr = malloc(sizeof(*dr));
    assert(dr != NULL);

    dr->r       = r;
    dr->prompts = prompts;
    dr->pl      = NULL;
    return dr;
}
