/* context-lists.c 732b */
#include "all.h"

/* private functions for updating lists of expressions in contexts 732d */
static void fill_hole(Exp e, Value v) {
  assert(e->alt == HOLE);
  e->alt = LITERAL;
  e->u.literal = v;
}
/* private functions for updating lists of expressions in contexts 732e */
static Explist find_explist_hole(Explist el) {
  while (el && el->hd->alt != HOLE)
    el = el->tl;
  return el;
}
/* context-lists.c 732c */
Exp transition_explist(Explist es, Value v) {
  Explist p = find_explist_hole(es);
  assert(p);
  fill_hole(p->hd, v);
  return head_replaced_with_hole(p->tl);
}
/* context-lists.c 733a */
Exp head_replaced_with_hole(Explist el) {
  static struct Exp a_copy; /* overwritten by subsequent calls */
  if (el) {
    a_copy = *el->hd;
    *el->hd = mkHoleStruct();
    return &a_copy;
  } else {
    return NULL;
  }
}
/* context-lists.c 733b */
Explist copyEL(Explist el) {
  if (el == NULL)
    return NULL;
  else {
    Exp e = malloc(sizeof(*e));
    assert(e);
    *e = *el->hd;
    return mkEL(e, copyEL(el->tl));
  }
}
/* context-lists.c 733c */
void freeEL(Explist el) {
  if (el != NULL) {
    freeEL(el->tl);
    free(el->hd);
    free(el);
  }
}
/* context-lists.c 733d */
void freeVL(Valuelist vl) {
  if (vl != NULL) {
    freeVL(vl->tl);
    free(vl);
  }
}
/* context-lists.c 734a */
Valuelist asLiterals(Explist e) {
  if (e == NULL)
    return NULL;
  else
    return mkVL(asLiteral(e->hd), asLiterals(e->tl));

}
/* context-lists.c 734b */
Value asLiteral(Exp e) {
  assert(e->alt == LITERAL);
  return validate(e->u.literal);
}
