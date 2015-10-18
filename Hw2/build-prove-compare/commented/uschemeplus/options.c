/*
 * Options and diagnostic code
 * 
 * <options.c>=
 */
#include "all.h"

Value getoption(Name name, Env env, Value defaultval) {
    Value *p = find(name, env);
    if (p)
        return *p;
    else
        return defaultval;
}
