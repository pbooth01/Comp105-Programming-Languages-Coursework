/*
 * Evaluating definitions
 * 
 * The function [[evaldef]] evaluates a definition,
 * updates the store, and returns a new environment. If
 * [[echo]] is nonzero, [[evaldef]] also prints.
 * <evaldef.c>=
 */
#include "all.h"
/*
 * <evaldef.c>=
 */
Env evaldef(Def d, Env env, Echo echo) {
    switch (d->alt) {
    case VAL:
        /*
         * <evaluate [[val]] binding and return new environment>=
         */
        {
            Value v;

            pushcontext(*d->u.val.exp, roots.stack);
            if (find(d->u.val.name, env) == NULL)
                env = bindalloc(d->u.val.name, unspecified(), env);
            *d->u.val.exp = topframe(roots.stack)->context;
            popframe(roots.stack);
            v = eval(d->u.val.exp, env);
            *find(d->u.val.name, env) = v;
            if (echo) {
                if (d->u.val.exp->alt == LAMBDAX)
                    print("%n\n", d->u.val.name);
                else
                    print("%v\n", v);
            }
            return env;
        }
    case EXP:
        /*
         * As in Impcore, evaluating a top-level expression has
         * the same effect on the environment as evaluating a
         * definition of [[it]], except that the interpreter
         * always prints the value, never the name ``it.''
         * <evaluate expression, store the result in [[it]], and return new
                                                                   environment>=
         */
        {
            Value v = eval(d->u.exp, env);
            Value *itloc = find(strtoname("it"), env);
            if (echo == ECHOING)
                print("%v\n", v);
            if (itloc == NULL) {
                return bindalloc(strtoname("it"), v, env);
            } else {
                *itloc = v;
                return env;
            }
        }
    case DEFINE:
        /*
         * We rewrite \xdefine to \xval.
         * <evaluate function definition and return new environment>=
         */
        /*
         * <if [[d->u.define.lambda.formals]] contains a duplicate, call
                                                                     [[error]]>=
         */
        if (duplicatename(d->u.define.lambda.formals) != NULL)
            error(
               "formal parameter %n appears twice in definition of function %n",
                  duplicatename(d->u.define.lambda.formals), d->u.define.name);
        return evaldef(mkVal(d->u.define.name, mkLambdax(d->u.define.lambda)),
                       env, echo);
    }
    assert(0);
    return NULL;
}
/*
 * Function [[readevalprint]] evaluates definitions,
 * updates the environment [[*envp]], and remembers unit
 * tests. [*]
 * <evaldef.c>=
 */
void readevalprint(XDefreader reader, Env *envp, Echo echo) {
    XDef d;
    UnitTestlist unit_tests = NULL;

    while ((d = readxdef(reader)))
    switch (d->alt) {
    case DEF:
        *envp = evaldef(d->u.def, *envp, echo);
        break;
    case USE:
        /*
         * Reading a file is as in Impcore, except that
         * in micro-Scheme, we cannot mutate an environment.
         * We therefore pass [[readevalprint]] a pointer to the
         * environment [[env]], and when [[readevalprint]]
         * evaluates a definition, it writes a new environment
         * in place of the old one.
         * <read in a file and update [[*envp]]>=
         */
        {
            const char *filename = nametostr(d->u.use);
            FILE *fin = fopen(filename, "r");
            if (fin == NULL)
                error("cannot open file \"%s\"", filename);
            readevalprint(xdefreader(filereader(filename, fin), NO_PROMPTS),
                                                                 envp, ECHOING);
            fclose(fin);
        }
        break;
    case TEST:
        unit_tests = mkUL(d->u.test, unit_tests);
        break;
    default:
        assert(0);
    }

    set_error_mode(TESTING);
    /*
     * <run the remembered [[unit_tests]], last one first>=
     */
    {   int npassed = tests_passed(unit_tests, *envp);
        int ntests  = lengthUL(unit_tests);
        report_test_results(npassed, ntests);
    }
    set_error_mode(NORMAL);
}
/*
 * In the [[DEF]] case, the assignment to [[*envp]]
 * ensures that after a successful call to [[evaldef]],
 * the new environment is remembered, even if a later
 * call to [[evaldef]] exits the loop by calling
 * [[error]]. This code is more complicated than the
 * analogous code in Impcore: Impcore's
 * [[readevalprint]] simply mutates the global
 * environment. In micro-Scheme, environments are not
 * mutable, so we mutate a C location instead.
 * 
 */

