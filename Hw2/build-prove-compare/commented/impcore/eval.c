/*
 * Implementation of the evaluator
 * 
 * [*]
 * 
 * The most interesting parts of the implementation are
 * the evaluator (functions [[eval]] and [[evaldef]])
 * and the [[readevalprint]] loop. [*]
 * 
 * [*]
 * <eval.c>=
 */
#include "all.h"

/*
 * Applying a user-defined function has something in
 * common with [[begin]], because arguments e_1, ...,
 * e_n have to be evaluated. The difference is that
 * [[begin]] keeps only result v_n (in variable [[v]] in
 * chunk [->]), where function evaluation keeps all the
 * result values, to bind into a new environment. We use
 * the auxiliary function [[evallist]], which is given
 * e_1, ..., e_n along with xi_0, phi, and rho_0. It
 * evaluates e_1, ..., e_n in order, mutates the
 * environments so that when it is finished, xi= xi_n
 * and rho= rho_n. Finally, [[evallist]] returns the
 * list v_1, ..., v_n.
 * <eval helpers>=
 */
static Valuelist evallist(Explist el, Valenv globals, Funenv functions, Valenv
                                                                      formals) {
    if (el == NULL) {
        return NULL;
    } else {
        Value v = eval(el->hd, globals, functions, formals);
        return mkVL(v, evallist(el->tl, globals, functions, formals));
    }
}
/*
 * The rules of Impcore require that [[el->hd]] be
 * evaluated before [[el->tl]]. To ensure the correct
 * order of evaluation, we must call [[eval(el->hd,]]
 *  ...[[)]] and [[evallist(el->tl,]] ...[[)]] in
 * separate C statements. Writing both calls as
 * parameters to [[mkVL]] would be a mistake because
 * C makes no guarantees about the order in which the
 * actual parameters of a function are evaluated.
 */

/*
 * Like any other recursive interpreter, [[eval]]
 * implements the operational semantics by examining the
 * inference rules from the bottom up. The first step in
 * evaluating [[e]] is to discover what the syntactic
 * form of [[e]] is, by looking at the tag (the [[alt]]
 * field) in the discriminated union. [*]
 * <eval.c>=
 */
Value eval(Exp e, Valenv globals, Funenv functions, Valenv formals) {
    checkoverflow(1000000 * sizeof(char *)); /* OMIT */
    switch (e->alt) {
    case LITERAL:
        /*
         * Only one rule has \xliteral in its conclusion. The
         * implementation simply returns the literal value.
         * <evaluate [[e->u.literal]] and return the result>=
         */
        return e->u.literal;
    case VAR:
        /*
         * Two rules have variables in their conclusions. We
         * know which rule to use by checking x in dom rho,
         * which in C is [[isvalbound(e->u.var, formals)]]. If x
         * \notindom rho and x \notindom xi, the operational
         * semantics gets stuck—so the interpreter issues an
         * error message. Less formally, we look up the variable
         * by checking first the local environment and then the
         * global environment.
         * <evaluate [[e->u.var]] and return the result>=
         */
        if (isvalbound(e->u.var, formals))
            return fetchval(e->u.var, formals);
        else if (isvalbound(e->u.var, globals))
            return fetchval(e->u.var, globals);
        else
            error("unbound variable %n", e->u.var);
        assert(0);   /* not reached */
        return 0;   
        /*
         * The call to [[error]] illustrates the convenience of
         * an extensible printer; we use [[ first needing to
         * convert it to a string.
         */

    case SET:
        /*
         * Setting a variable is very similar. Again there are
         * two rules, and again we distinguish by looking at the
         * domain of rho ([[formals]]). Because both rules
         * require the premise \evale ==>\eval[']v, we evaluate
         * the right-hand side first and put the result in 
         * [[v]].
         * <evaluate [[e->u.set]] and return the result>=
         */
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
        /*
         * To evaluate [[ifx]], we again have two rules. Both
         * rules have the same first premise: \evale_1 ==>\eval
         * [']v_1. We can find v_1, xi', and rho' by making the
         * recursive call [[eval(e->u.ifx.cond, globals,
         * functions, formals)]]. This call is safe only because
         * the new environments xi' and rho' are used in the
         * third premises of both rules. If xi' and rho' were
         * not always used, we would have had to make copies and
         * pass the copies to the recursive call.
         * 
         * Once we have v_1, testing it against zero tells us
         * which rule to use, and the third premises of both
         * rules require recursive calls to [[eval]]. The
         * expression e_2 is [[e->u.ifx.true]]; e_3 is [[e->
         * u.ifx.false]]. [*]
         * <evaluate [[e->u.ifx]] and return the result>=
         */
        if (eval(e->u.ifx.cond, globals, functions, formals) != 0)
            return eval(e->u.ifx.true, globals, functions, formals);
        else
            return eval(e->u.ifx.false, globals, functions, formals);
    case WHILEX:
        /*
         * There are also two rules for while loops. A simple
         * translation of \eval['']\while(e_1, e_2) ==>\eval
         * [''']v_3 where it appears as a premise of the first
         * rule would require a recursive call to [[eval(e,
         * ...)]]. Because we know [[e]] is a while loop,
         * however, we can turn the recursion into iteration,
         * writing the interpretation of Impcore's while loop as
         * a [[while]] loop in C. Such an optimization is
         * valuable because it can keep the C stack from
         * overflowing during the execution of a long while loop
         * in Impcore.
         * <evaluate [[e->u.whilex]] and return the result>=
         */
        while (eval(e->u.whilex.cond, globals, functions, formals) != 0)
            eval(e->u.whilex.exp, globals, functions, formals);
        return 0;
    case BEGIN:
        /*
         * For the [[begin]] expression, we use a [[for]] loop
         * to evaluate all the premises in turn. In the
         * pointless case where the [[begin]] doesn't contain
         * any expressions, we have crafted the operational
         * semantics to match the implementation. [*]
         * <evaluate [[e->u.begin]] and return the result>=
         */
        {
            Explist el;
            Value v = 0;
            for (el=e->u.begin; el; el=el->tl)
                v = eval(el->hd, globals, functions, formals);
            return v;
        }
    case APPLY:
        /*
         * There are many rules for applying functions, but we
         * divide them into two classes. One class contains only
         * the rule for user-defined functions; the other class
         * contains the rules for primitives. To apply function 
         * f, the interpreter looks at the form of phi(f).
         * <evaluate [[e->u.apply]] and return the result>=
         */
        {
            Fun f;
            /*
             * If the lookup phi(f) fails, we call [[error]].
             * <make [[f]] the function denoted by [[e->u.apply.name]], or call
                                                                     [[error]]>=
             */
            if (!isfunbound(e->u.apply.name, functions))
                error("call to undefined function %n", e->u.apply.name);
            f = fetchfun(e->u.apply.name, functions);
            switch (f.alt) {
            case USERDEF:
                /*
                 * The premises of the \rulenameApplyUser rule require
                 * that the list of formal parameters to f be the same
                 * length as the list of actual parameters in the call.
                 * We let [[nl]] represent the formals, [[vl]] the
                 * actuals. If the formals and actuals are the same
                 * length, we use [[mkValenv(nl, vl)]] to create a fresh
                 * environment {x_1 |->v_1, ..., x_n |->v_n } in which
                 * to evaluate the body.
                 * <apply [[f.u.userdef]] and return the result>=
                 */
                {
                    Namelist  nl = f.u.userdef.formals;
                    Valuelist vl = evallist(e->u.apply.actuals, globals,
                                                            functions, formals);
                    checkargc(e, lengthNL(nl), lengthVL(vl));
                    return eval(f.u.userdef.body, globals, functions, mkValenv(
                                                                       nl, vl));
                }
            case PRIMITIVE:
                /*
                 * Impcore has few primitive operators, and they are
                 * simple. We handle [[print]] separately from the
                 * arithmetic primitives. More general techniques for
                 * implementing primitives, which are appropriate for
                 * larger languages, are shown as part of the
                 * implementation of micro-Scheme, in Section [->].
                 * <apply [[f.u.primitive]] and return the result>=
                 */
                {
                    Valuelist vl = evallist(e->u.apply.actuals, globals,
                                                            functions, formals);
                    if (f.u.primitive == strtoname("print"))
                        /*
                         * <apply [[print]] to [[vl]] and return>=
                         */
                        {
                            Value v;
                            checkargc(e, 1, lengthVL(vl));
                            v = nthVL(vl, 0);
                            print("%v\n", v);
                            return v;
                        }
                    else
                        /*
                         * Because all the arithmetic primitives are single
                         * letters, we can use a switch on the first character
                         * of the name. This technique is reasonable only
                         * because the set of primitives is small and fixed. The
                         * code also depends on the fact that Impcore shares C's
                         * rules for the values of comparison expressions.
                         * <apply arithmetic primitive to [[vl]] and return>=
                         */
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
/*
 * For each syntactic form, we consult the operational
 * semantics to find the rules that have the form on the
 * left-hand sides of their conclusions.
 */

/*
 * Evaluating definitions
 * 
 * [*] The [[readevalprint]] function processes a whole
 * source of input. It evaluates ordinary definitions
 * and remembers unit tests. [*]
 * <eval.c>=
 */
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
            /*
             * On seeing [[use]], we open the file named by [[use]],
             * build a top-level reader, and through
             * [[readevalprint]], we recursively call [[evaldef]] on
             * all the definitions in that file.[*]
             * <evaluate [[d->u.use]], possibly mutating [[globals]] and
                                                                 [[functions]]>=
             */
            {
                const char *filename = nametostr(d->u.use);
                FILE *fin = fopen(filename, "r");
                if (fin == NULL)
                    error("cannot open file \"%s\"", filename);
                readevalprint(xdefreader(filereader(filename, fin), NO_PROMPTS),
                              globals, functions, SILENT);
                fclose(fin);
            }
            /*
             * As noted in Exercise [->], this code can leak open
             * file descriptors.
             */

            break;
        default:
            assert(0);
        }

    set_error_mode(TESTING);
    /*
     * <run the remembered [[unit_tests]], last one first>=
     */
    {   int npassed = tests_passed(unit_tests, globals, functions);
        int ntests  = lengthUL(unit_tests);
        report_test_results(npassed, ntests);
    }
    set_error_mode(NORMAL);
}

/*
 * Just like [[eval]], [[evaldef]] looks at the
 * conclusions of rules, and it discriminates on the
 * syntactic form of [[t]]. [*]
 * <eval.c>=
 */
void evaldef(Def d, Valenv globals, Funenv functions, Echo echo) {
    switch (d->alt) {
    case VAL:
        /*
         * A variable definition updates xi. The premise shows
         * we must call [[eval]] to get value v and environment 
         * xi'. When we call [[eval]], we must use an empty
         * environment as rho. The rule says the new
         * environment xi' is retained, and the value of the
         * expression, v, is bound to x in it. The
         * implementation may also print v.
         * <evaluate [[d->u.val]], mutating [[globals]]>=
         */
        {
            Value v = eval(d->u.val.exp, globals, functions, mkValenv(NULL, NULL
                                                                             ));
            bindval(d->u.val.name, v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case EXP:
        /*
         * Evaluating a top-level expression is just like
         * evaluating a definition of [[it]].
         * <evaluate [[d->u.exp]] and possibly print the result>=
         */
        {
            Value v = eval(d->u.exp, globals, functions, mkValenv(NULL, NULL));
            bindval(strtoname("it"), v, globals);
            if (echo == ECHOING)
                print("%v\n", v);
        }
        return;
    case DEFINE:
        /*
         * A function definition updates phi. Our implementation
         * also prints the name of the function.
         * <evaluate [[d->u.define]], mutating [[functions]]>=
         */
        /*
         * <fail if [[d->u.define]] has duplicate formal parameters>=
         */
        if (duplicatename(d->u.define.userfun.formals) != NULL)
            error(
         "Formal parameter named %n appears twice in definition of function %n",
                  duplicatename(d->u.define.userfun.formals), d->u.define.name);
        bindfun(d->u.define.name, mkUserdef(d->u.define.userfun), functions);
        if (echo == ECHOING)
            print("%n\n", d->u.define.name);
        /*
         * As happens too often, the error checking is more work
         * than the normal case.
         */

        return;
    }
    assert(0);
}
/*
 * To understand the [[tests_passed]] function, we must
 * remember that the [[UnitTestlist]] that is passed in
 * has been created by adding each new test to the head
 * of the list: the most recently added test is at the
 * beginning. But we want to run the tests in the order
 * they were written, which means running the tail of
 * the list before its head. A convenient way to do this
 * is to make the [[tests_passed]] function recursive,
 * and to place the recursive call before the code that
 * runs the current test. (If the list [[tests]] were
 * very long, we might have to worry about recursion
 * blowing the C stack. But the list is only as long as
 * the number of tests written by hand, so we probably
 * don't have to worry about more than dozens of tests,
 * for which default stack space should be adequate.)
 * <eval.c>=
 */
int tests_passed(UnitTestlist tests, Valenv globals, Funenv functions) {
    if (tests == NULL)
        return 0;
    else {
        int n = tests_passed(tests->tl, globals, functions);
        UnitTest t = tests->hd;
        switch (t->alt) {
        case CHECK_EXPECT:
            /*
             * <run [[check-expect]] test [[t]], returning [[n]] or [[n+1]]>=
             */
            {   Value check;    /* results of evaluating first expression */
                Value expect;   /* results of evaluating second expression */
                Valenv locals = mkValenv(NULL, NULL);
                if (setjmp(testjmp)) {
                    /*
                     * <report that evaluating [[t->u.check_expect.check]]
                                                          failed with an error>=
                     */
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
                    /*
                     * <report that evaluating [[t->u.check_expect.expect]]
                                                          failed with an error>=
                     */
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
                    /*
                     * <report failure because the values are not equal>=
                     */
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
            /*
             * <run [[check-error]] test [[t]], returning [[n]] or [[n+1]]>=
             */
            {   Value check;    /* results of evaluating the expression */
                Valenv locals = mkValenv(NULL, NULL);
                if (setjmp(testjmp)) {
                    return n+1; /* error occurred, so the test passed */
                }
                check = eval(t->u.check_expect.check, globals, functions, locals
                                                                              );
                /*
                 * <report that evaluating [[t->u.check_error]] produced
                                                                     [[check]]>=
                 */
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
