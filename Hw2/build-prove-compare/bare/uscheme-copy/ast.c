/* ast.c 714g */
#include "all.h"
/* ast.c 715e */
void printdef(FILE *output, va_list_box *box) {
    Def d = va_arg(box->ap, Def);
    if (d == NULL) {
        fprint(output, "<null>");
        return;
    }

    switch (d->alt) {
    case VAL:
        fprint(output, "(val %n %e)", d->u.val.name, d->u.val.exp);
        return;
    case EXP:
        fprint(output, "%e", d->u.exp);
        return;
    case DEFINE:
        fprint(output, "(define %n %\\)", d->u.define.name, d->u.define.lambda);
        return;
    }
    assert(0);
}
/* ast.c 716a */
void printxdef(FILE *output, va_list_box *box) {
    XDef d = va_arg(box->ap, XDef);
    if (d == NULL) {
        fprint(output, "<null>");
        return;
    }

    switch (d->alt) {
    case USE:
        fprint(output, "(use %n)", d->u.use);
        return;
    case TEST:
        fprint(output, "CANNOT PRINT UNIT TEST XXX\n");
        return;
    case DEF:
        fprint(output, "%t", d->u.def);
        return;
    }
    assert(0);
}
/* ast.c 716b */
static void printlet(FILE *output, Exp let) {
    Namelist nl;
    Explist el;

    switch (let->u.letx.let) {
    case LET:
        fprint(output, "(let (");
        break;
    case LETSTAR:
        fprint(output, "(let* (");
        break;
    case LETREC:
        fprint(output, "(letrec (");
        break;
    default:
        assert(0);
    }
    for (nl = let->u.letx.nl, el = let->u.letx.el; 
         nl && el;
         nl = nl->tl, el = el->tl)
        fprint(output, "(%n %e)%s", nl->hd, el->hd, nl->tl?" ":"");
    fprint(output, ") %e)", let->u.letx.body);
}   
/* ast.c 717a */
void printexp(FILE *output, va_list_box *box) {
    Exp e = va_arg(box->ap, Exp);
    if (e == NULL) {
        fprint(output, "<null>");
        return;
    }

    switch (e->alt) {
    case LITERAL:
        if (e->u.literal.alt == NUM || e->u.literal.alt == BOOL)
            fprint(output, "%v", e->u.literal);
        else
            fprint(output, "'%v", e->u.literal);
        break;
    case VAR:
        fprint(output, "%n", e->u.var);
        break;
    case IFX:
        fprint(output, "(if %e %e %e)", e->u.ifx.cond, e->u.ifx.true, e->
                                                                   u.ifx.false);
        break;
    case WHILEX:
        fprint(output, "(while %e %e)", e->u.whilex.cond, e->u.whilex.body);
        break;
    case BEGIN:
        fprint(output, "(begin%s%E)", e->u.begin ? " " : "", e->u.begin);
        break;
    case SET:
        fprint(output, "(set %n %e)", e->u.set.name, e->u.set.exp);
        break;
    case LETX:
        printlet(output, e);
        break;
    case LAMBDAX:
        fprint(output, "%\\", e->u.lambdax);
        break;
    case APPLY:
        fprint(output, "(%e%s%E)", e->u.apply.fn,
              e->u.apply.actuals ? " " : "", e->u.apply.actuals);
        break;
    /* extra cases for printing {\uscheme} ASTs 726a */
    /* extra cases for printing {\uscheme} ASTs 736a */
    case BREAKX:
        fprint(output, "(break)");
        break;
    case CONTINUEX:
        fprint(output, "(continue)");
        break;
    case RETURNX:
        fprint(output, "(return %e)", e->u.returnx);
        break;
    case THROW:
        fprint(output, "(throw %e)", e->u.throw);
        break;
    case TRY_CATCH:
        fprint(output, "(try-catch %e %e)", e->u.try_catch.body, e->
                                                           u.try_catch.handler);
        break;
    case HOLE:
        fprint(output, "<*>");
        break;
    case LETXENV:
        fprintf(stderr, "Restore let environment %p", (void*)e->u.letxenv);
        break;
    case CALLENV:
        fprintf(stderr, "Restore caller's environment %p", (void*)e->u.callenv);
        break;
    case WHILE_RUNNING_BODY:
        fprint(output, "(while-running-body %e %e)", e->u.whilex.cond, e->
                                                                 u.whilex.body);
        break;
    }
}
/* ast.c 717b */
void printlambda(FILE *output, va_list_box *box) {
    Lambda l = va_arg(box->ap, Lambda);
    fprint(output, "(lambda (%N) %e)", l.formals, l.body);
}
