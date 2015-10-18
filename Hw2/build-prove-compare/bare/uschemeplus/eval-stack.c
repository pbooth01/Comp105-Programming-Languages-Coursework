/* eval-stack.c 223a */
#include "all.h"

Value eval(Exp e, Env env) {
    Value v;
    Frame *fr;
    /* definition of static [[Exp hole]], which always has a hole 734g */
    static struct Exp holeExp = { HOLE, { { NIL, { 0 } } } };
    static Exp hole = &holeExp;
    static Stack evalstack;

    /* ensure that [[evalstack]] is initialized and empty 728d */
    if (evalstack == NULL)
        evalstack = emptystack();
    else
        clearstack(evalstack);
    /* use the options in [[env]] to initialize the instrumentation 730g */
    high_stack_mark = 0;
    show_high_stack_mark = 
        istrue(getoption(strtoname("&show-high-stack-mark"), env, falsev));
    /* use the options in [[env]] to initialize the instrumentation 731d */
    {   Value *p = find(strtoname("&trace-stack"), env);
        if (p && p->alt == NUM)
            stack_trace_init(&p->u.num);
        else
            stack_trace_init(NULL);
    }
    /* use the options in [[env]] to initialize the instrumentation 734d */
    optimize_tail_calls = 
        istrue(getoption(strtoname("&optimize-tail-calls"), env, truev));

    exp: 
        stack_trace_current_expression(e, env, evalstack);
        /* take a step from a state of the form $\seval e$ 224 */
        switch (e->alt) {
        case LITERAL:

/* start evaluating expression [[e->u.literal]] and transition to the next state 226a */
            v = e->u.literal;
            goto value;
        case VAR:   

/* start evaluating expression [[e->u.var]] and transition to the next state 226b */
            if (find(e->u.var, env) == NULL)
                error("variable %n not found", e->u.var);
            v = *find(e->u.var, env);
            goto value;
        case SET:

/* start evaluating expression [[e->u.set]] and transition to the next state 227a */
            if (find(e->u.set.name, env) == NULL)
                error("set unbound variable %n", e->u.set.name);
            pushcontext(mkSetStruct(e->u.set.name, hole), evalstack);
            e = e->u.set.exp;
            goto exp;
        case IFX:

/* start evaluating expression [[e->u.ifx]] and transition to the next state 227c */
            pushcontext(mkIfxStruct(hole, e->u.ifx.true, e->u.ifx.false),
                                                                     evalstack);
            e = e->u.ifx.cond;
            goto exp;
        case WHILEX:

/* start evaluating expression [[e->u.whilex]] and transition to the next state 236c */
            pushcontext(mkWhilexStruct(e->u.whilex.cond, e->u.whilex.body),
                                                                     evalstack);
            e = e->u.whilex.cond;
            goto exp;
        case BEGIN:

/* start evaluating expression [[e->u.begin]] and transition to the next state 236a */
            pushcontext(mkBeginStruct(e->u.begin), evalstack);
            v = falsev;
            goto value;
        case LETX:
            if (/* [[e->u.letx]] contains no bindings 232a */
                e->u.letx.nl == NULL && e->u.letx.el == NULL) {
                 e = e->u.letx.body; /* continue with the body */
                 goto exp;
            } else {
                switch (e->u.letx.let) {
                   case LET:
/* start evaluating nonempty [[let]] expression [[e->u.letx]] and transition to the next state 232b */

/* if [[e->u.letx.nl]] contains a duplicate, complain of error in [[let]] 715b */
                                 if (duplicatename(e->u.letx.nl) != NULL)
                                     error("bound name %n appears twice in let",
                                                   duplicatename(e->u.letx.nl));
                                 pushcontext(mkLetxStruct(e->u.letx.let, e->
                                                                      u.letx.nl,
                                                          copyEL(e->u.letx.el),
                                                                e->u.letx.body),
                                             evalstack);
                                 fr = topframe(evalstack);
                                 e  = head_replaced_with_hole(fr->
                                                             context.u.letx.el);
                                 assert(e);
                                 goto exp;
                   case LETSTAR:
/* start evaluating nonempty [[let*]] expression [[e->u.letx]] and transition to the next state 233a */
                                 pushenv_opt(env, LETXENV, evalstack);
                                 pushcontext(mkLetxStruct(e->u.letx.let, e->
                                       u.letx.nl, e->u.letx.el, e->u.letx.body),
                                             evalstack);
                                 fr = topframe(evalstack);
                                 assert(fr->context.u.letx.el);
                                 e = fr->context.u.letx.el->hd;
                                 assert(e);
                                 goto exp;
                   case LETREC:
/* start evaluating nonempty [[letrec]] expression [[e->u.letx]] and transition to the next state 232c */

/* if [[e->u.letx.nl]] contains a duplicate, complain of error in [[letrec]] 715c */
                                 if (duplicatename(e->u.letx.nl) != NULL)
                                     error(
          "bound name %n appears twice in letrec", duplicatename(e->u.letx.nl));
                                 pushenv_opt(env, LETXENV, evalstack);
                                 {   Namelist nl;
                                     for (nl = e->u.letx.nl; nl; nl = nl->tl)
                                                                                
                                         env = bindalloc(nl->hd, unspecified(),
                                                                           env);
                                 }
                                 pushcontext(mkLetxStruct(e->u.letx.let, e->
                                                                      u.letx.nl,
                                                          copyEL(e->u.letx.el),
                                                                e->u.letx.body),
                                             evalstack);
                                 fr = topframe(evalstack);
                                 e  = head_replaced_with_hole(fr->
                                                             context.u.letx.el);
                                 assert(e);
                                 goto exp;
                   default:      assert(0);
                }
            }
        case LAMBDAX:

/* start evaluating expression [[e->u.lambdax]] and transition to the next state 226c */

     /* if [[e->u.lambdax.formals]] contains a duplicate, call [[error]] 715a */
            if (duplicatename(e->u.lambdax.formals) != NULL)
                error("formal parameter %n appears twice in lambda",
                      duplicatename(e->u.lambdax.formals));
            v = mkClosure(e->u.lambdax, env);
            goto value;
        case APPLY:

/* start evaluating expression [[e->u.apply]] and transition to the next state 229c */
            pushcontext(mkApplyStruct(mkHole(), copyEL(e->u.apply.actuals)),
                                                                     evalstack);
            topframe(evalstack)->syntax = e;
            e = e->u.apply.fn;
            goto exp;
        case BREAKX:

        /* start evaluating [[(break)]] and transition to the next state 238a */
            fr = topframe(evalstack);
            if (fr == NULL) 
                error("(break) occurred outside any loop");
            else
                switch (fr->context.alt) {
                    case WHILE_RUNNING_BODY:  /* Break-Transfer */
                        popframe(evalstack);
                        v = falsev;
                        goto value;
                    case CALLENV:
                        error("(break) in function outside of any loop");
                    default:                  /* Break-Unwind */
                        popframe(evalstack);
                        goto exp;
                }
        case CONTINUEX:

/* start evaluating [[(continue)]] and transition to the next state ((prototype)) 238b */
            error("The implementation of (continue) is left as an exercise");
        case RETURNX:

/* start evaluating expression [[e->u.returnx]] and transition to the next state 238c */
            pushcontext(mkReturnxStruct(hole), evalstack);
            e = e->u.returnx;
            goto exp;
        case THROW:

/* start evaluating expression [[e->u.throw]] and transition to the next state 238e */
            pushcontext(mkThrowStruct(hole), evalstack);
            e = e->u.throw;
            goto exp;
        case TRY_CATCH:

/* start evaluating expression [[e->u.try_catch]] and transition to the next state 239a */
            pushcontext(mkTryCatchStruct(e->u.try_catch.body, hole), evalstack);
            e = e->u.try_catch.handler;
            goto exp;

/* cases where the current item is an expression form that should appear only on the stack 734f */
        case WHILE_RUNNING_BODY:
        case HOLE:
        case LETXENV:
        case CALLENV:
            assert(0);
        }
        assert(0);
    value: 
        stack_trace_current_value(v, env, evalstack);
        v = validate(v);

/* if [[evalstack]] is empty, return [[v]]; otherwise step from a state of the form $\sevalv {\mathtt{fr} \sconsop S}$ 223b */
        fr = topframe(evalstack);
        if (fr == NULL) {

          /* if [[show_high_stack_mark]] is set, show maximum stack size 730h */
            if (show_high_stack_mark)
                fprintf(stderr, "High stack mark == %d\n", high_stack_mark);
            return v;
        } else {

/* take a step from a state of the form $\sevalv {\mathtt{fr} \sconsop S}$ 225 */
            switch (fr->context.alt) {
            case SET:

/* fill hole in context [[fr->context.u.set]] and transition to the next state 227b */
                assert(fr->context.u.set.exp->alt == HOLE);
                assert(find(fr->context.u.set.name, env) != NULL);
                *find(fr->context.u.set.name, env) = validate(v);
                popframe(evalstack);
                goto value;
            case IFX:

/* fill hole in context [[fr->context.u.ifx]] and transition to the next state 227d */
                assert(fr->context.u.ifx.cond->alt == HOLE);
                e = istrue(v) ? fr->context.u.ifx.true : fr->context.u.ifx.false
                                                                               ;
                popframe(evalstack);
                goto exp;
            case WHILEX:

/* if [[v]] is true, continue with body in context [[fr->context.u.whilex]] 237a */
                if (istrue(validate(v))) {
                                           /* Small-Step-While-Condition-True */
                    fr->context.alt = WHILE_RUNNING_BODY;
                    e = fr->context.u.whilex.body;
                    goto exp;
                } else {
                                          /* Small-Step-While-Condition-False */
                    popframe(evalstack);
                    v = falsev;
                    goto value;
                }
            case WHILE_RUNNING_BODY:

/* transition to [[WHILEX]] and continue with condition in context [[fr->context.u.whilex]] 237b */
                fr->context.alt = WHILEX;
                e = fr->context.u.whilex.cond;
                goto exp;
            case BEGIN:

 /* continue with the next expression in context [[fr->context.u.begin]] 236b */
                if (fr->context.u.begin) {
                                          /* Small-Step-Begin-Next-Expression */
                    e = fr->context.u.begin->hd;
                    fr->context.u.begin = fr->context.u.begin->tl;
                    goto exp;
                } else {                     /* Small-Step-Begin-Exhausted */
                    popframe(evalstack);
                    goto value;
                }    
            case APPLY:

/* fill hole in context [[fr->context.u.apply]] and transition to the next state 230 */
                if (fr->context.u.apply.fn->alt == HOLE) {
                                                /* Small-Step-Apply-First-Arg */
                    *fr->context.u.apply.fn = mkLiteralStruct(v);
                    e = head_replaced_with_hole(fr->context.u.apply.actuals);
                    if (e)
                        goto exp;
                                                /* Small-Step-Apply-First-Arg */
                    else
                        goto apply_last_arg;
                                                   /* empty list of arguments */
                } else {                                    
                    e = transition_explist(fr->context.u.apply.actuals, v); 
                    if (e)
                        goto exp;
                                                 /* Small-Step-Apply-Next-Arg */
                    else goto
                        apply_last_arg;
                                                 /* Small-Step-Apply-Last-Arg */
                }
                apply_last_arg:
                               /* Small-Step-Apply-Last-Arg (or no arguments) */

/* apply [[fr->context]]'s [[fn]] to its [[actuals]]; free memory; transition to next state 231a */
                    {
                        Value     fn = asLiteral (fr->context.u.apply.fn);
                        Valuelist vl = asLiterals(fr->context.u.apply.actuals);
                        free  (fr->context.u.apply.fn);
                        freeEL(fr->context.u.apply.actuals);

                        popframe(evalstack);
                        
                        switch (fn.alt) {
                          case PRIMITIVE:

  /* apply [[fn.u.primitive]] to [[vl]] and transition to the next state 231b */
                              v = fn.u.primitive.function(fr->syntax,
                                                        fn.u.primitive.tag, vl);
                              freeVL(vl);
                              goto value;
                          case CLOSURE:

/* save [[env]], bind [[vl]] to [[fn.u.closure]]'s formals, and transition to evaluation of closure's body 231c */
                              {
                                  Namelist nl = fn.u.closure.lambda.formals;

                                  checkargc(e, lengthNL(nl), lengthVL(vl));
                                  pushenv_opt(env, CALLENV, evalstack);
                                  env = bindalloclist(nl, vl, fn.u.closure.env);
                                  e   = fn.u.closure.lambda.body;
                                  freeVL(vl);
                                  goto exp;
                              }
                          default:
                              error("%e evaluates to non-function %v in %e",
                                    fr->syntax->u.apply.fn, fn, fr->syntax);
                        }
                    }
            case LETX:
                switch (fr->context.u.letx.let) {
                   case LET:
                 /* continue with [[let]] context [[fr->context.u.letx]] 233b */
                                 e = transition_explist(fr->context.u.letx.el, v
                                                                              );
                                 if (e) {         /* Small-Step-Next-Let-Exp */ 
                                     goto exp;
                                 } else {        /* Small-Step-Let-Body */
                                     Namelist xs  = fr->context.u.letx.nl;
                                               /* 1. Remember x's and v's     */
                                     Explist  es  = fr->context.u.letx.el;
                                     Valuelist vs = asLiterals(es);
                                     e = fr->context.u.letx.body;
                                               /* 2. Update e                 */
                                     popframe(evalstack);
                                               /* 3. Pop the LET context      */
                                     pushenv_opt(env, LETXENV, evalstack);
                                               /* 4. Push env                 */
                                     env = bindalloclist(xs, vs, env);
                                               /* 5. Update env               */
                                     freeEL(es);
                                               /* 6. Recover memory           */
                                     freeVL(vs);
                                     goto exp;
                                               /* 7. Transition to next state */
                                 }
                   case LETSTAR:
                /* continue with [[let*]] context [[fr->context.u.letx]] 235a */
                                 assert(fr->context.u.letx.nl != NULL && fr->
                                                     context.u.letx.el != NULL);
                                 env = bindalloc(fr->context.u.letx.nl->hd, v,
                                                                           env);
                                 fr->context.u.letx.nl = fr->context.u.letx.nl->
                                                                             tl;
                                 fr->context.u.letx.el = fr->context.u.letx.el->
                                                                             tl;
                                 if (fr->context.u.letx.el) {
                                               /* Small-Step-Next-Letstar-Exp */
                                     e = fr->context.u.letx.el->hd;
                                     goto exp;
                                 } else {
                                                   /* Small-Step-Letstar-Body */
                                     e = fr->context.u.letx.body;
                                     popframe(evalstack);
                                     goto exp;
                                 }
                   case LETREC:
              /* continue with [[letrec]] context [[fr->context.u.letx]] 234a */
                                 e = transition_explist(fr->context.u.letx.el, v
                                                                              );
                                 if (e) {  /* Small-Step-Next-Letrec-Exp */
                                     goto exp;
                                 } else {  /* Small-Step-Letrec-Body */

/* store values in [[fr->context.u.letx.el]] in locations bound to [[fr->context.u.letx.nl]] 234b */
                                     {
                                         Namelist nl = fr->context.u.letx.nl;
                                         Explist  el = fr->context.u.letx.el;
                                         while (el || nl) { 
                                             assert(el && nl);
                                             assert(find(nl->hd, env));
                                             *find(nl->hd, env) = asLiteral(el->
                                                                            hd);
                                             el = el->tl;
                                             nl = nl->tl;
                                         }
                                     };
                                     freeEL(fr->context.u.letx.el);
                                     e = fr->context.u.letx.body;
                                     popframe(evalstack);
                                     goto exp;
                                 }
                   default:      assert(0);
                }
            case LETXENV:

/* restore [[env]] from [[fr->context.u.letxenv]], pop the stack, and transition to the next state 235b */
                env = fr->context.u.letxenv;
                popframe(evalstack);
                goto value;
            case CALLENV:

/* restore [[env]] from [[fr->context.u.callenv]], pop the stack, and transition to the next state 235c */
                env = fr->context.u.callenv;
                popframe(evalstack);
                goto value;
            case TRY_CATCH:

/* if awaiting handler, install [[v]] and evaluate body, otherwise pop stack and transition to the next state 239b */
                if (fr->context.u.try_catch.handler->alt == HOLE) {
                                                         /* Try-Catch-Handler */
                    if (v.alt != CLOSURE && v.alt != PRIMITIVE) 
                        error(
             "Handler in try-catch is %v, but a handler must be a function", v);
                    e = fr->context.u.try_catch.body;
                    popframe(evalstack);
                    pushenv_opt(env, LETXENV, evalstack);
                    pushcontext(mkTryCatchStruct(hole, mkLiteral(v)), evalstack)
                                                                               ;
                    goto exp;
                } else {
                                                          /* Try-Catch-Finish */
                    assert(fr->context.u.try_catch.body->alt == HOLE);
                    popframe(evalstack);
                    goto value;
                }
            case RETURNX:
                /* return [[v]] from the current function ((prototype)) 238d */
                error("Implementation of (return e) is left as an exercise");
            case THROW:

/* throw [[v]] to the nearest [[try-catch]] that has an installed handler ((prototype)) 238f */
                error("Implementation of (throw e) is left as an exercise");
            case LITERAL:  /* syntactic values never appear as contexts */
            case VAR:
            case LAMBDAX:
            case HOLE:     /* and neither do bare holes */
            case BREAKX:   /* nor do break or continue */
            case CONTINUEX:
                assert(0);
            }
        }

        assert(0);
        return falsev; /* appease a stupid compiler */ /* OMIT */
}
/* eval-stack.c 240 */
static int isenv(Exp e) {
    return e && (e->alt == CALLENV || e->alt == LETXENV);
}
                          
void pushenv_opt(Env env, Expalt context, Stack s) {
    if (optimize_tail_calls && topframe(s) && isenv(&topframe(s)->context)) {
        if (context == CALLENV && topframe(s)->context.alt == LETXENV) 
            topframe(s)->context = mkCallenvStruct(topframe(s)->
                                                             context.u.letxenv);
                           /* subtle and quick to anger */
    } else {
        struct Exp e = context == CALLENV ? mkCallenvStruct(env) :
                                                           mkLetxenvStruct(env);
        pushcontext(e, s);
    }
}
