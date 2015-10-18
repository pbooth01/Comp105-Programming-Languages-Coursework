/* loc.c 272 */
#include "all.h"

Value* allocate(Value v) {
    Value *loc;

    pushreg(&v);
    loc = allocloc();
    popreg(&v);
    assert(loc != NULL);
    *loc = v;
    return loc;
}
/* loc.c 745b */
int gammadesired(int defaultval, int minimum) {
    Value *gloc;

    assert(roots.globals != NULL);
    gloc = find(strtoname("&gamma-desired"), *roots.globals);
    if (gloc && gloc->alt == NUM)
        return gloc->u.num > minimum ? gloc->u.num : minimum;
    else
        return defaultval;
}
/* loc.c 749a */
Value validate(Value v) {
    assert(v.alt != INVALID);
    return v;
}
/* loc.c 749b */
extern void printfinalstats(void);
void initallocate(void) {
    gc_debug_init();
    atexit(printfinalstats);
}
