/* evaldef.c 147d */
#include "all.h"
/* evaldef.c 147e */
Env evaldef(Def d, Env env, Echo echo) {
    switch (d->alt) {
    case VAL:
        /* evaluate [[val]] binding and return new environment 148a */
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

/* evaluate expression, store the result in [[it]], and return new environment 148b */
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
        /* evaluate function definition and return new environment 148c */

/* if [[d->u.define.lambda.formals]] contains a duplicate, call [[error]] 715d */
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
/* evaldef.c 149b */
void readevalprint(XDefreader reader, Env *envp, Echo echo) {
    XDef d;
    UnitTestlist unit_tests = NULL;

    while ((d = readxdef(reader)))
    switch (d->alt) {
    case DEF:
        *envp = evaldef(d->u.def, *envp, echo);
        break;
    case USE:
        /* read in a file and update [[*envp]] 149a */
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
    /* run the remembered [[unit_tests]], last one first 712d */
    {   int npassed = tests_passed(unit_tests, *envp);
        int ntests  = lengthUL(unit_tests);
        report_test_results(npassed, ntests);
    }
    set_error_mode(NORMAL);
}
