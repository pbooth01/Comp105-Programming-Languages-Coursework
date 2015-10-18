/* eval.c 40a */
#include "all.h"

/* eval helpers 44a */
static Valuelist evallist(Explist el, Valenv globals, Funenv functions, Valenv
                                                                      formals) {
    if (el == NULL) {
        return NULL;
    } else {
        Value v = eval(el->hd, globals, functions, formals);
        return mkVL(v, evallist(el->tl, globals, functions, formals));
    }
}
/* eval.c 40b */
Value eval(Exp e, Valenv globals, Funenv functions, Valenv formals) {
    checkoverflow(1000000 * sizeof(char *)); /* OMIT */
    switch (e->alt) {
    case LITERAL:
        /* evaluate [[e->u.literal]] and return the result 40c */
        return e->u.literal;
    case VAR:
        /* evaluate [[e->u.var]] and return the result 41a */
        if (isvalbound(e->u.var, formals))
            return fetchval(e->u.var, formals);
        else if (isvalbound(e->u.var, globals))
            return fetchval(e->u.var, globals);
        else
            error("unbound variable %n", e->u.var);
        assert(0);   /* not reached */
        return 0;   
    case SET:
        /* evaluate [[e->u.set]] and return the result 41b */
        {
            Value v = eval(e->u.set.exp, globals, functions, formals);

            if (isvalbound(e->u.set.name, formals))
                bindval(e->u.set.name, v, formals);
            else if (isvalbound(e->u.set.name, globals))
                bindval(e->u.set.name, v, globals);
            else
                error("set: unbound variable %n", e->u.set.name);
            return v;
        }
    case IFX:
        /* evaluate [[e->u.ifx]] and return the result 42a */
        if (eval(e->u.ifx.cond, globals, functions, formals) != 0)
            return eval(e->u.ifx.true, globals, functions, formals);
        else
            return eval(e->u.ifx.false, globals, functions, formals);
    case WHILEX:
        /* evaluate [[e->u.whilex]] and return the result 42b */
        while (eval(e->u.whilex.cond, globals, functions, formals) != 0)
            eval(e->u.whilex.exp, globals, functions, formals);
        return 0;
    case BEGIN:
        /* evaluate [[e->u.begin]] and return the result 43a */
        {
            Explist el;
            Value v = 0;
            for (el=e->u.begin; el; el=el->tl)
                v = eval(el->hd, globals, functions, formals);
            return v;
        }
    case APPLY:
        /* evaluate [[e->u.apply]] and return the result 43b */
        {
            Fun f;

/* make [[f]] the function denoted by [[e->u.apply.name]], or call [[error]] 43c */
            if (!isfunbound(e->u.apply.name, functions))
                error("call to undefined function %n", e->u.apply.name);
            f = fetchfun(e->u.apply.name, functions);
            switch (f.alt) {
            case USERDEF:
                /* apply [[f.u.userdef]] and return the result 44b */
                {
                    Namelist  nl = f.u.userdef.formals;
                    Valuelist vl = evallist(e->u.apply.actuals, globals,
                                                            functions, formals);
                    checkargc(e, lengthNL(nl), lengthVL(vl));
                    return eval(f.u.userdef.body, globals, functions, mkValenv(
                                                                       nl, vl));
                }
            case PRIMITIVE:
                /* apply [[f.u.primitive]] and return the result 45a */
                {
                    Valuelist vl = evallist(e->u.apply.actuals, globals,
                                                            functions, formals);
                    if (f.u.primitive == strtoname("print"))
                        /* apply [[print]] to [[vl]] and return 45b */
                        {
                            Value v;
                            checkargc(e, 1, lengthVL(vl));
                            v = nthVL(vl, 0);
                            print("%v\n", v);
                            return v;
                        }
                    else
                        /* apply arithmetic primitive to [[vl]] and return 46 */
                        {
                            const char *s;
                            Value v, w;

                            checkargc(e, 2, lengthVL(vl));
                            v = nthVL(vl, 0);
                            w = nthVL(vl, 1);

                            s = nametostr(f.u.primitive);
                            assert(strlen(s) == 1);
                            switch (s[0]) {
                            case '<':
                                return v < w;
                            case '>':
                                return v > w;
                            case '=':
                                return v == w;
                            case '+':
                                return v + w;
                            case '-':
                                return v - w;
                            case '*':
                                return v * w;
                            case '/':
                                if (w == 0)
                                    error("division by zero in %e", e);
                                return v / w;
                            default:
                                assert(0);
                                return 0;   /* not reached */
                            }
                        }
                }
            }
            assert(0);
            return 0;   /* not reached */
        }
    }
    assert(0);
    return 0;   /* not reached */
}
/* eval.c 47a */
void readevalprint(XDefreader reader, Valenv globals, Funenv functions, Echo
                                                                         echo) {
    XDef d;
    UnitTestlist unit_tests = NULL;

    while ((d = readxdef(reader)))
        switch (d->alt) {
        case DEF:
            evaldef(d->u.def, globals, functions, echo);
            break;
        case TEST:
            unit_tests = mkUL(d->u.test, unit_tests);
            break;
        case USE:

/* evaluate [[d->u.use]], possibly mutating [[globals]] and [[functions]] 47b */
            {
                const char *filename = nametostr(d->u.use);
                FILE *fin = fopen(filename, "r");
                if (fin == NULL)
                    error("cannot open file \"%s\"", filename);
                readevalprint(xdefreader(filereader(filename, fin), NO_PROMPTS),
                              globals, functions, SILENT);
                fclose(fin);
            }
            break;
        default:
            assert(0);
        }

    set_error_mode(TESTING);
    /* run the remembered [[unit_tests]], last one first 49e */
    {   int npassed = tests_passed(unit_tests, globals, functions);
        int ntests  = lengthUL(unit_tests);
        report_test_results(npassed, ntests);
    }
    set_error_mode(NORMAL);
}

/* eval.c 48a */
void evaldef(Def d, Valenv globals, Funenv functions, Echo echo) {
    switch (d->alt) {
    case VAL:
        /* evaluate [[d->u.val]], mutating [[globals]] 48b */
        {
            Value v = eval(d->u.val.exp, globals, functions, mkValenv(NULL, NULL
                                                                             ));
            bindval(d->u.val.name, v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case EXP:
        /* evaluate [[d->u.exp]] and possibly print the result 48c */
        {
            Value v = eval(d->u.exp, globals, functions, mkValenv(NULL, NULL));
            bindval(strtoname("it"), v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case DEFINE:
        /* evaluate [[d->u.define]], mutating [[functions]] 49a */
        /* fail if [[d->u.define]] has duplicate formal parameters 49b */
        if (duplicatename(d->u.define.userfun.formals) != NULL)
            error(
         "Formal parameter named %n appears twice in definition of function %n",
                  duplicatename(d->u.define.userfun.formals), d->u.define.name);
        bindfun(d->u.define.name, mkUserdef(d->u.define.userfun), functions);
        if (echo == ECHOING)
            print("%n\n", d->u.define.name);
        return;
    }
    assert(0);
}
/* eval.c 703a */
int tests_passed(UnitTestlist tests, Valenv globals, Funenv functions) {
    if (tests == NULL)
        return 0;
    else {
        int n = tests_passed(tests->tl, globals, functions);
        UnitTest t = tests->hd;
        switch (t->alt) {
        case CHECK_EXPECT:

          /* run [[check-expect]] test [[t]], returning [[n]] or [[n+1]] 703b */
            {   Value check;    /* results of evaluating first expression */
                Value expect;   /* results of evaluating second expression */
                Valenv locals = mkValenv(NULL, NULL);
                if (setjmp(testjmp)) {

/* report that evaluating [[t->u.check_expect.check]] failed with an error 704c */
                    fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                            "value as %e, but evaluating %e causes an error.\n",
                                   t->u.check_expect.check, t->
                                                          u.check_expect.expect,
                                   t->u.check_expect.check);
                    return n;
                }
                check = eval(t->u.check_expect.check, globals, functions, locals
                                                                              );
                if (setjmp(testjmp)) {

/* report that evaluating [[t->u.check_expect.expect]] failed with an error 704d */
                    fprint(stderr,
                     "Check-expect failed: expected %e to evaluate to the same "

                            "value as %e, but evaluating %e causes an error.\n",
                                   t->u.check_expect.check, t->
                                                          u.check_expect.expect,
                                   t->u.check_expect.expect);
                    return n;
                }
                expect = eval(t->u.check_expect.expect, globals, functions,
                                                                        locals);

                if (check != expect) {
                    /* report failure because the values are not equal 704b */
                    fprint(stderr,
                           "Check-expect failed: expected %e to evaluate to %v",
                           t->u.check_expect.check, expect);
                    if (t->u.check_expect.expect->alt != LITERAL)
                        fprint(stderr, " (from evaluating %e)", t->
                                                         u.check_expect.expect);
                    fprint(stderr, ", but it's %v.\n", check);
                    return n;
                } else {
                    return n+1;
                }
            }
        case CHECK_ERROR:

           /* run [[check-error]] test [[t]], returning [[n]] or [[n+1]] 704a */
            {   Value check;    /* results of evaluating the expression */
                Valenv locals = mkValenv(NULL, NULL);
                if (setjmp(testjmp)) {
                    return n+1; /* error occurred, so the test passed */
                }
                check = eval(t->u.check_expect.check, globals, functions, locals
                                                                              );

       /* report that evaluating [[t->u.check_error]] produced [[check]] 704e */
                fprint(stderr,
                    "Check-error failed: evaluating %e was expected to produce "

                            "an error, but instead it produced the value %v.\n",
                               t->u.check_error, check);

                return n;
            }    
        }
        assert(0);
    }
}
