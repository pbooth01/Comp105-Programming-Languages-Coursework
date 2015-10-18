/* env.c 706a */
#include "all.h"
/* env.c 706b */
Value* find(Name name, Env env) {
    for (; env; env = env->tl)
        if (env->name == name)
            return env->loc;
    return NULL;
}
/* env.c 706c */
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
/* env.c (BUG: software can't tell where this code came from) */
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
/* env.c (BUG: software can't tell where this code came from) */
Env bindalloclist(Namelist nl, Valuelist vl, Env env) {
    Valuelist oldvals = vl;
    pushregs(oldvals);
    for (; nl && vl; nl = nl->tl, vl = vl->tl)
        env = bindalloc(nl->hd, vl->hd, env);
    popregs(oldvals);
    return env;
}
