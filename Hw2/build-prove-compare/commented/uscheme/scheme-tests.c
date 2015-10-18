/*
 * <scheme-tests.c>=
 */
#include "all.h"
int tests_passed(UnitTestlist tests, Env rho) {
    if (tests == NULL)
        return 0;
    else {
        int n = tests_passed(tests->tl, rho);
        UnitTest t = tests->hd;
        switch (t->alt) {
        case CHECK_EXPECT:
            /*
             * <run [[check-expect]] test [[t]], returning [[n]] or [[n+1]]>=
             */
            {   Value check;    /* results of evaluating first expression */
                Value expect;   /* results of evaluating second expression */
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
                check = eval(t->u.check_expect.check,  rho);
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
                expect = eval(t->u.check_expect.expect, rho);

                if (!equalpairs(check, expect)) {
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
                if (setjmp(testjmp)) {
                    return n+1; /* error occurred, so the test passed */
                }
                check = eval(t->u.check_expect.check,  rho);
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
        return -1;
    }
}
/*
 * <scheme-tests.c>=
 */
int equalpairs(Value v, Value w) {
    if (v.alt != w.alt)
        return 0;
    else
        switch (v.alt) {
        case PAIR:
            return equalpairs(*v.u.pair.car, *w.u.pair.car) &&
                   equalpairs(*v.u.pair.cdr, *w.u.pair.cdr);
        case NUM:
            return v.u.num  == w.u.num;
        case BOOL:
            return v.u.bool == w.u.bool;
        case SYM:
            return v.u.sym  == w.u.sym;
        case NIL:
            return 1;
        default:
            return 0;
        }
}
