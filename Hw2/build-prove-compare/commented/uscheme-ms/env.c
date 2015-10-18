/*
 * <env.c>=
 */
#include "all.h"
/*
 * We look up a name by following [[tl]] pointers. [*]
 * <env.c>=
 */
Value* find(Name name, Env env) {
    for (; env; env = env->tl)
        if (env->name == name)
            return env->loc;
    return NULL;
}
/*
 * We include [[printenv]] in case it helps you debug
 * your code.
 * <env.c>=
 */
void printenv(FILE *output, va_list_box *box) {
    Env env = va_arg(box->ap, Env);
    char *prefix = " ";

    fprint(output, "{");
    for (; env; env = env->tl) {
        fprint(output, "%s%n -> %v", prefix, env->name, *env->loc);
        prefix = ", ";
    }
    fprint(output, " }");
}
/*
 * <env.c>=
 */
Env bindalloc(Name name, Value val, Env env) {
    Env newenv = malloc(sizeof(*newenv));
    assert(newenv != NULL);

    newenv->name = name;
    pushcontext(mkLetxenvStruct(env), roots.stack);
    newenv->loc  = allocate(val);
    popframe(roots.stack);
    newenv->tl   = env;
    return newenv;
}
/*
 * Please also observe that [[val]] is a parameter
 * passed by value, so we have a fresh copy of it. It
 * contains [[Value*]] pointers, so you might think it
 * needs to be on the root stack for the copying
 * collector (so that the pointers can be updated if
 * necessary). But by the time we get to [[allocate]],
 * our copy of [[val]] is dead—only [[allocate]]'s
 * private copy matters.
 */

/*
 * In [[bindalloclist]], by contrast, when we call
 * [[bindalloc]] with [[vl->hd]], our copy of [[vl->hd]]
 * is dead, as is everything that precedes it. But
 * values reachable from [[vl->tl]] are still live.
 * To make them visible to the garbage collector,
 * we treat them as ``machine registers.''
 * <env.c>=
 */
Env bindalloclist(Namelist nl, Valuelist vl, Env env) {
    Valuelist oldvals = vl;
    pushregs(oldvals);
    for (; nl && vl; nl = nl->tl, vl = vl->tl)
        env = bindalloc(nl->hd, vl->hd, env);
    popregs(oldvals);
    return env;
}
