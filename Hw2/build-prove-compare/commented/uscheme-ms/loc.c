/*
 * <loc.c>=
 */
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
/*
 * The interesting part is the [[pushreg]] and
 * [[popreg]]. Because if [[v]] happens to be a [[cons]]
 * cell, and if [[allocloc]] happens to call the garbage
 * collector, we have to prevent the garbage collector
 * from reclaiming the locations pointed to by
 * [[v.u.pair.car]] and [[v.u.pair.cdr]]. The call to
 * [[pushreg]] makes [[v]] a ``machine register'' and
 * ensures that the collector treats it as a root. And
 * if the collector happens to move [[v]]'s [[car]] and
 * [[cdr]], it updates [[v]]'s internal pointers to
 * point to the new locations.
 */

/*
 * Access to the desired size of the heap
 * 
 * To control the size of the heap, we might want to use
 * the micro-Scheme variable [[ --- gamma-desired]], as
 * described in Exercises [->] and [->]. This routine
 * gets the value of that variable. [*]
 * <loc.c>=
 */
int gammadesired(int defaultval, int minimum) {
    Value *gloc;

    assert(roots.globals != NULL);
    gloc = find(strtoname("&gamma-desired"), *roots.globals);
    if (gloc && gloc->alt == NUM)
        return gloc->u.num > minimum ? gloc->u.num : minimum;
    else
        return defaultval;
}
/*
 * Function [[validate]] is used freely in the
 * interpreter to make sure all values are good. Calling
 * [[validate(v)]] returns [[v]], unless [[v]] is
 * invalid, in which case it causes an assertion
 * failure.
 * <loc.c>=
 */
Value validate(Value v) {
    assert(v.alt != INVALID);
    return v;
}
/*
 * [max*], which finds the maximum of a non-empty list
 * of integers. Function [[max]] does not have a left or
 * right identity.
 * [gcd*], which finds the greatest common divisor of a
 * non-empty list of integers. As implemented in
 * micro-Scheme, function [[gcd]] has left and right
 * identities, but you can also implement [[gcd*]] in
 * the same way you implement [[max*]].
 * [lcm*], which finds the least common multiple of a
 * non-empty list of integers. Like [[gcd]], [[lcm]] has
 * left and right identities, but again you can write
 * [[lcm*]] in the same way as [[max*]].
 * [sum], which finds the sum of a non-empty list of
 * integers.
 * [product], which finds the product of a non-empty
 * list of integers.
 * <loc.c>=
 */
extern void printfinalstats(void);
void initallocate(void) {
    gc_debug_init();
    atexit(printfinalstats);
}
