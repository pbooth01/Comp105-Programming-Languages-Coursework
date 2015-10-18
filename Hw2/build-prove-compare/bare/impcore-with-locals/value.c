/* value.c 701b */
#include "all.h"

void printvalue(FILE *output, va_list_box *box) {
    Value v = va_arg(box->ap, Value);
    fprint(output, "%d", v);
}
