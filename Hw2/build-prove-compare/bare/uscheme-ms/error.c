/* error.c 685d */
#include "all.h"

jmp_buf errorjmp;
jmp_buf testjmp;

static ErrorMode mode = NORMAL;

void error(const char *fmt, ...) {
    va_list_box box;

    switch (mode) {
    case NORMAL:
        assert(fmt);
        fflush(stdout);
        fprint(stderr, "error: ");
        va_start(box.ap, fmt);
        vprint(stderr, fmt, &box);
        va_end(box.ap);
        fprint(stderr, "\n");
        fflush(stderr);
        longjmp(errorjmp, 1);
        assert(0);
    case TESTING:
        longjmp(testjmp, 1);
        assert(0);
    }
    assert(0);
}
/* error.c 686a */
void set_error_mode(ErrorMode new_mode) {
  assert(new_mode == NORMAL || new_mode == TESTING);
  mode = new_mode;
}
/* error.c 686b */
void checkargc(Exp e, int expected, int actual) {
    if (expected != actual)
        error("expected %d but found %d argument%s in %e",
              expected, actual, actual == 1 ? "" : "s", e);
}
/* error.c 686c */
Name duplicatename(Namelist nl) {
    if (nl != NULL) {
        Name n = nl->hd;
        Namelist tail;
        for (tail = nl->tl; tail; tail = tail->tl)
            if (n == tail->hd)
                return n;
        return duplicatename(nl->tl);
    }
    return NULL;
}
