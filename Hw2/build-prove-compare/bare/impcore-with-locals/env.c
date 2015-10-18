/* env.c 52e */
#include "all.h"
/* env.c 53a */
struct Valenv {
    Namelist  nl;
    Valuelist vl;
};
/* env.c 53b */
Valenv mkValenv(Namelist nl, Valuelist vl) {
    Valenv e = malloc(sizeof(*e));
    assert(e != NULL);
    assert(lengthNL(nl) == lengthVL(vl));
    e->nl = nl;
    e->vl = vl;
    return e;
}
/* env.c 53c */
static Value* findval(Name name, Valenv env) {
    Namelist  nl;
    Valuelist vl;

    for (nl=env->nl, vl=env->vl; nl && vl; nl=nl->tl, vl=vl->tl)
        if (name == nl->hd)
            return &vl->hd;
    return NULL;
}
/* env.c 53d */
int isvalbound(Name name, Valenv env) {
    return findval(name, env) != NULL;
}
/* env.c 53e */
Value fetchval(Name name, Valenv env) {
    Value *vp = findval(name, env);
    assert(vp != NULL);
    return *vp;
}
/* env.c 54 */
void bindval(Name name, Value val, Valenv env) {
    Value *vp = findval(name, env);
    if (vp != NULL)
        *vp = val;              /* safe optimization */
    else {
        env->nl = mkNL(name, env->nl);
        env->vl = mkVL(val,  env->vl);
    }
}
/* env.c 688c */
struct Funenv {
    Namelist nl;
    Funlist  fl;
};
/* env.c 689a */
Funenv mkFunenv(Namelist nl, Funlist fl) {
    Funenv e = malloc(sizeof *e);
    assert(e != NULL);
    assert(lengthNL(nl) == lengthFL(fl));
    e->nl = nl;
    e->fl = fl;
    return e;
}
/* env.c 689b */
static Fun* findfun(Name name, Funenv env) {
    Namelist nl = env->nl;
    Funlist  fl = env->fl;

    for ( ; nl && fl; nl = nl->tl, fl = fl->tl)
        if (name == nl->hd)
            return &fl->hd;
    return NULL;
}
/* env.c 689c */
int isfunbound(Name name, Funenv env) {
    return findfun(name, env) != NULL;
}
/* env.c 689d */
Fun fetchfun(Name name, Funenv env) {
    Fun *fp = findfun(name, env);
    assert(fp != NULL);
    return *fp;
}
/* env.c 689e */
void bindfun(Name name, Fun fun, Funenv env) {
    Fun *fp = findfun(name, env);
    if (fp != NULL)
        *fp = fun;              /* safe optimization */
    else {
        env->nl = mkNL(name, env->nl);
        env->fl = mkFL(fun,  env->fl);
    }
}
