/* context-stack.c 728a */
#include "all.h"

/* representation of [[struct Stack]] 727 */
struct Stack {
    int size;
    Frame *frames;  /* memory for 'size' frames */
    Frame *sp;      /* points to first unused frame */
};

int optimize_tail_calls = 1;
int high_stack_mark;
                  /* maximum number of phrased used in the current evaluation */
int show_high_stack_mark;
/* context-stack.c 728b */
Stack emptystack(void) {
    Stack s;
    s = malloc(sizeof *s);
    assert(s);
    s->size = 8;
    s->frames = malloc(s->size * sizeof(*s->frames));
    assert(s->frames);
    s->sp = s->frames;
    return s;
}
/* context-stack.c 728c */
void clearstack (Stack s) {
    s->sp = s->frames;
}
/* context-stack.c 728e */
Frame *topframe (Stack s) {
    assert(s);
    if (s->sp == s->frames)
        return NULL;
    else
        return s->sp - 1;
}
/* context-stack.c 729a */
static Frame *pushframe (Frame f, Stack s) {
    assert(s);
    /* if stack [[s]] is full, enlarge it 729b */
    if (s->sp - s->frames == s->size) {
        unsigned newsize = 2 * s->size;
        if (newsize > 10000)
            error("Recursion too deep");
        s->frames = realloc(s->frames, newsize * sizeof(*s->frames));
        assert(s->frames);
        s->sp = s->frames + s->size;
        s->size = newsize;
    }
    *s->sp++ = f;
    /* set [[high_stack_mark]] from stack [[s]] 731a */
    {   int n = s->sp - s->frames;
        if (n > high_stack_mark)
            high_stack_mark = n;
    }
    return s->sp - 1;
}
/* context-stack.c 729c */
void popframe (Stack s) {
    assert(s->sp - s->frames > 0);
    s->sp--;
}
/* context-stack.c 729d */
static Frame mkExpFrame(struct Exp e) {
  Frame fr;
  fr.context = e;
  fr.syntax = NULL;
  return fr;
}

Exp pushcontext(struct Exp e, Stack s) {
  Frame *fr;
  assert(s);
  fr = pushframe(mkExpFrame(e), s);
  return &fr->context;
}
/* context-stack.c 730c */
void printnoenv(FILE *output, va_list_box* box) {
    Env env = va_arg(box->ap, Env);
    fprintf(output, "@%p", (void *)env);
}
/* context-stack.c 730d */
void printstack(FILE *output, va_list_box *box) {
    Stack s = va_arg(box->ap, Stack);
    Frame *fr;

    for (fr = s->sp-1; fr >= s->frames; fr--) {
        fprint(output, "  ");
        printframe(output, fr);
        fprint(output, ";\n");
    }
}
/* context-stack.c 730e */
void printoneframe(FILE *output, va_list_box *box) {
    Frame *fr = va_arg(box->ap, Frame*);
    printframe(output, fr);
}
/* context-stack.c 730f */
void printframe (FILE *output, Frame *fr) {
    fprintf(output, "%p: ", (void *) fr);
    fprint(output, "[%e]", &fr->context);
}
