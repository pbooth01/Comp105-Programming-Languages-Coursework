/*
 * Parsing
 * 
 * This is really not interesting.
 * 
 * <parse.c>=
 */
#include "all.h"

/*
 * The [[getnamelist]] function turns a [[Parlist]] that
 * is a list of names into a [[Namelist]], calling
 * [[error]] if the [[Parlist]] contains any sublists.
 * The passed [[Par]] parameter is only for printing a
 * good error message.
 * <parse.c declarations>=
 */
Namelist getnamelist(Par p, Parlist pl);
/*
 * Now we can move on to parsing [[Exp]]s. The
 * [[parselist]] helper function parses a list of
 * [[Par]] expressions and calls [[parseexp]] repeatedly
 * to return a list of [[Exp]]s.
 * <parse.c declarations>=
 */
Explist parselist(Parlist);
/*
 * <parse.c declarations>=
 */
Exp parseexp(Par);
/*
 * Function [[parseletbindings]] adds bindings to a let
 * expression.
 * <parse.c declarations>=
 */
static void parseletbindings(Par p, Parlist bindings, Exp letexp);
/*
 * <parse.c declarations>=
 */
Value parsesx(Par p);
/*
 * <parse.c>=
 */
XDef parse(Par p) {
    switch (p->alt) {
    case ATOM:
        /*
         * If we have a name, we treat it as an expression.
         * <parse ATOM and return>=
         */
        return mkDef(mkExp(parseexp(p)));
    case LIST:
        /*
         * If we have a list, we need to look for [[define]],
         * [[val]], [[use]], and [[check-expect]].
         * <parse LIST and return>=
         */
        {
            Parlist pl = p->u.list;

            if (pl == NULL)
                error("%p: empty list", p);
            if (nthPL(pl, 0)->alt == ATOM) {
                Name first = nthPL(pl, 0)->u.atom;
                if (first == strtoname("define")) {
                    /*
                     * Parsing definitions requires checking argument counts
                     * and then parsing subpieces.
                     * <parse define and return>=
                     */
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
                    /*
                     * <parse val and return>=
                     */
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
                    /*
                     * <parse use and return>=
                     */
                    if (lengthPL(pl) != 2 || nthPL(pl, 1)->alt != ATOM)
                        error("%p: usage: (use filename)", p);

                    return mkUse(nthPL(pl, 1)->u.atom);
                }
                if (first == strtoname("check-expect")) {
                    /*
                     * <parse check-expect and return>=
                     */
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
                    /*
                     * <parse check-error and return>=
                     */
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
/*
 * <parse.c>=
 */
Namelist getnamelist(Par p, Parlist pl) {
    if (pl == NULL)
        return NULL;
    if (pl->hd->alt != ATOM)
        error("%p: formal parameter list contains %p, which is not a name", p,
                                                                        pl->hd);
    return mkNL(pl->hd->u.atom, getnamelist(p, pl->tl));
}
/*
 * Why does [[pushexp]] work if only the copy is on the
 * root stack? Because only internal fields of values
 * get changed.
 * <parse.c>=
 */
Explist parselist(Parlist pl) {
    Exp e;
    Explist el;

    if (pl == NULL)
        return NULL;

    e = parseexp(pl->hd);
    pushcontext(*e, roots.stack);
    el = parselist(pl->tl);
    *e = topframe(roots.stack)->context;  /* e.u.literal may have changed */
    popframe(roots.stack);
    return mkEL(e, el);
}
/*
 * <parse.c>=
 */
Exp parseexp(Par p) {
    switch (p->alt) {
    case ATOM:
        /*
         * To parse an atom, we need to check whether it is a
         * boolean or a number. Otherwise it is a variable.
         * <parseexp [[ATOM]] and return>=
         */
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
        /*
         * In [[parseexp]], we have just one small change: If we
         * call [[parselist]] to parse arguments, we put the
         * resulting [[Explist]] [[el]] on the context stack.
         * <parseexp [[LIST]] and return>=
         */
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
                /*
                 * <parseexp [[lambda]] and put the result in [[rv]]>=
                 */
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
                /*
                 * When we parse a let expression, we parse the body
                 * first. The whole let expression goes on the root
                 * stack while we parse the bindings.
                 * <parseexp let and put the result in [[rv]]>=
                 */
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

                pushcontext(*rv, roots.stack);
                parseletbindings(p, letbindings->u.list, rv);
                popframe(roots.stack);
            } else if (first == strtoname("quote")) {
                /*
                 * <parseexp [[quote]] and put the result in [[rv]]>=
                 */
                {
                    if (lengthPL(pl) != 2)
                        error("%p: quote needs exactly one argument", p);
                    rv = mkLiteral(parsesx(nthPL(pl, 1)));
                }
            } else {
                el = parselist(pl->tl);
                pushcontext(mkBeginStruct(el), roots.stack);
                if (first == strtoname("begin")) {
                    /*
                     * A [[begin]] statement can have any number of
                     * parameters.
                     * <parseexp [[begin]] and put the result in [[rv]]>=
                     */
                    rv = mkBegin(el);
                } else if (first == strtoname("if")) {
                    /*
                     * An [[if]] statement needs three parameters.
                     * <parseexp [[if]] and put the result in [[rv]]>=
                     */
                    if (lengthEL(el) != 3)
                        error("%p: usage: (if cond true false)", p);
                    rv = mkIfx(nthEL(el, 0), nthEL(el, 1), nthEL(el, 2));
                } else if (first == strtoname("set")) {
                    /*
                     * A [[set]] statement requires a variable and a value.
                     * <parseexp [[set]] and put the result in [[rv]]>=
                     */
                    if (lengthEL(el) != 2)
                        error("%p: usage: (set var exp)", p);
                    if (nthEL(el, 0)->alt != VAR)
                        error("%p: set needs variable as first param", p);
                    rv = mkSet(nthEL(el, 0)->u.var, nthEL(el, 1));
                } else if (first == strtoname("while")) {
                    /*
                     * A [[while]] loop needs two.
                     * <parseexp [[while]] and put the result in [[rv]]>=
                     */
                    if (lengthEL(el) != 2)
                        error("%p: usage: (while cond body)", p);
                    rv = mkWhilex(nthEL(el, 0), nthEL(el, 1));
                /*
                 * \uschemeplusis for \chaprefschemes.
                 * <[[RBR else LBR]] possibly parse expressions that are in \
                                                                   uschemeplus>=
                 */
                  /* nothing happens */
                /*
                 * Parsing
                 * 
                 * <[[RBR else LBR]] possibly parse expressions that are in \
                                                                   uschemeplus>=
                 */
                } else if (first == strtoname("break")) {
                    /*
                     * The [[break]] keyword is an expression all by itself.
                     * <parseexp [[break]] and put the result in [[rv]]>=
                     */
                    if (el != NULL)
                        error("%p: usage: (break)", p);
                    rv = mkBreakx();
                } else if (first == strtoname("continue")) {
                    /*
                     * <parseexp [[continue]] and put the result in [[rv]]>=
                     */
                    if (el != NULL)
                        error("%p: usage: (continue)", p);
                    rv = mkContinuex();
                } else if (first == strtoname("return")) {
                    /*
                     * <parseexp [[return]] and put the result in [[rv]]>=
                     */
                    if (lengthEL(el) != 1)
                        error("%p: usage: (return exp)", p);
                    rv = mkReturnx(nthEL(el, 0));
                } else if (first == strtoname("try-catch")) {
                    /*
                     * <parseexp [[try-catch]] and put the result in [[rv]]>=
                     */
                    rv = mkTryCatch(nthEL(el, 0), nthEL(el, 1));
                } else if (first == strtoname("throw")) {
                    /*
                     * <parseexp [[throw]] and put the result in [[rv]]>=
                     */
                    if (lengthEL(el) != 1)
                        error("%p: usage: (throw exp)", p);
                    rv = mkThrow(nthEL(el, 0));
                } else {
                   /*
                    * Parsing function application.
                    * <parseexp application and put the result in [[rv]]>=
                    */
                   rv = mkApply(parseexp(pl->hd), el);
                }
                popframe(roots.stack);
            }
            return rv;
        }
    default:
        assert(0);
        return NULL;
    }
}
/*
 * <parse.c>=
 */
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
/*
 * <parse.c>=
 */
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
        /*
         * To implement quoted S-expressions, we have to
         * maintain the root stack much as we did to implement
         * [[cons]].
         * <parsesx [[LIST]] and return>=
         */
        if (p->u.list == NULL)
            return mkNil();
        else {
            Value v, w, pair;
            Value *car;

            v = parsesx(p->u.list->hd);
            pushreg(&v);
            w = parsesx(mkList(p->u.list->tl));
            pushreg(&w);
            car = allocate(v);
            pair = mkPair(car, car);
                                            /* temporary; preserves invariant */
            car = NULL;  /* don't use this after alllocate() */
            pushreg(&pair);
            pair.u.pair.cdr = allocate(w);
            popreg(&pair);
            popreg(&w);
            popreg(&v);
            cyclecheck(&pair);
            return pair;
        }
    }
    assert(0);
    return falsev;
}
/*
 * Now we can assemble [[readtop]]. We keep a list of
 * read but not yet parsed [[Par]]s in [[tr->pl]].
 * <parse.c>=
 */
struct XDefreader {
    Prompts prompts;  /* whether to prompt at each definition */
    Reader r;      /* underlying reader of Pars */
    Parlist pl;    /* Pars read but not yet parsed */
};
/*
 * <parse.c>=
 */
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
/*
 * <parse.c>=
 */
XDefreader xdefreader(Reader r, Prompts prompts) {
    XDefreader dr;

    dr = malloc(sizeof(*dr));
    assert(dr != NULL);

    dr->r       = r;
    dr->prompts = prompts;
    dr->pl      = NULL;
    return dr;
}
