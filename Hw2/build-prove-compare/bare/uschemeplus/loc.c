/* loc.c 156b */
#include "all.h"

Value* allocate(Value v) {
    Value *loc = malloc(sizeof(*loc));
    assert(loc != NULL);
    *loc = v;
    return loc;
}
/* loc.c 156c */
void initallocate() {
}
