#include "all.h"
int lengthSL(Sourcelist l) {
	int n;

	for(n=0; l; n++)
		l=l->tl;

	return n;
}

int lengthEL(Explist l) {
	int n;

	for(n=0; l; n++)
		l=l->tl;

	return n;
}

int lengthUL(UnitTestlist l) {
	int n;

	for(n=0; l; n++)
		l=l->tl;

	return n;
}

int lengthPL(Parlist l) {
	int n;

	for(n=0; l; n++)
		l=l->tl;

	return n;
}

int lengthVL(Valuelist l) {
	int n;

	for(n=0; l; n++)
		l=l->tl;

	return n;
}

int lengthRL(Registerlist l) {
	int n;

	for(n=0; l; n++)
		l=l->tl;

	return n;
}

int lengthNL(Namelist l) {
	int n;

	for(n=0; l; n++)
		l=l->tl;

	return n;
}

Sourcelist mkSL(Source hd, Sourcelist tl) {
	Sourcelist l;

	l = malloc(sizeof *l);
	assert(l != NULL);

	l->hd = hd;
	l->tl = tl;

	return l;
}

Explist mkEL(Exp hd, Explist tl) {
	Explist l;

	l = malloc(sizeof *l);
	assert(l != NULL);

	l->hd = hd;
	l->tl = tl;

	return l;
}

UnitTestlist mkUL(UnitTest hd, UnitTestlist tl) {
	UnitTestlist l;

	l = malloc(sizeof *l);
	assert(l != NULL);

	l->hd = hd;
	l->tl = tl;

	return l;
}

Parlist mkPL(Par hd, Parlist tl) {
	Parlist l;

	l = malloc(sizeof *l);
	assert(l != NULL);

	l->hd = hd;
	l->tl = tl;

	return l;
}

Valuelist mkVL(Value hd, Valuelist tl) {
	Valuelist l;

	l = malloc(sizeof *l);
	assert(l != NULL);

	l->hd = hd;
	l->tl = tl;

	return l;
}

Registerlist mkRL(Register hd, Registerlist tl) {
	Registerlist l;

	l = malloc(sizeof *l);
	assert(l != NULL);

	l->hd = hd;
	l->tl = tl;

	return l;
}

Namelist mkNL(Name hd, Namelist tl) {
	Namelist l;

	l = malloc(sizeof *l);
	assert(l != NULL);

	l->hd = hd;
	l->tl = tl;

	return l;
}

Sourcelist popSL(Sourcelist l) {
	Sourcelist original = l;

	assert(l);
	l = l->tl;
	free(original);
	return l;
}

Explist popEL(Explist l) {
	Explist original = l;

	assert(l);
	l = l->tl;
	free(original);
	return l;
}

UnitTestlist popUL(UnitTestlist l) {
	UnitTestlist original = l;

	assert(l);
	l = l->tl;
	free(original);
	return l;
}

Parlist popPL(Parlist l) {
	Parlist original = l;

	assert(l);
	l = l->tl;
	free(original);
	return l;
}

Valuelist popVL(Valuelist l) {
	Valuelist original = l;

	assert(l);
	l = l->tl;
	free(original);
	return l;
}

Registerlist popRL(Registerlist l) {
	Registerlist original = l;

	assert(l);
	l = l->tl;
	free(original);
	return l;
}

Namelist popNL(Namelist l) {
	Namelist original = l;

	assert(l);
	l = l->tl;
	free(original);
	return l;
}

Source nthSL(Sourcelist l, unsigned n) {
	unsigned i;

	for(i=0; l && i<n; i++)
		l=l->tl;

	assert(l != NULL);
	return l->hd;
}

Exp nthEL(Explist l, unsigned n) {
	unsigned i;

	for(i=0; l && i<n; i++)
		l=l->tl;

	assert(l != NULL);
	return l->hd;
}

UnitTest nthUL(UnitTestlist l, unsigned n) {
	unsigned i;

	for(i=0; l && i<n; i++)
		l=l->tl;

	assert(l != NULL);
	return l->hd;
}

Par nthPL(Parlist l, unsigned n) {
	unsigned i;

	for(i=0; l && i<n; i++)
		l=l->tl;

	assert(l != NULL);
	return l->hd;
}

Value nthVL(Valuelist l, unsigned n) {
	unsigned i;

	for(i=0; l && i<n; i++)
		l=l->tl;

	assert(l != NULL);
	return l->hd;
}

Register nthRL(Registerlist l, unsigned n) {
	unsigned i;

	for(i=0; l && i<n; i++)
		l=l->tl;

	assert(l != NULL);
	return l->hd;
}

Name nthNL(Namelist l, unsigned n) {
	unsigned i;

	for(i=0; l && i<n; i++)
		l=l->tl;

	assert(l != NULL);
	return l->hd;
}

void printsourcelist(FILE *output, va_list_box *box) {
	Sourcelist l;

	for(l=va_arg(box->ap, Sourcelist); l; l=l->tl)
		fprint(output, "%s%s", l->hd, l->tl?" ":"");
}

void printexplist(FILE *output, va_list_box *box) {
	Explist l;

	for(l=va_arg(box->ap, Explist); l; l=l->tl)
		fprint(output, "%e%s", l->hd, l->tl?" ":"");
}

void printunittestlist(FILE *output, va_list_box *box) {
	UnitTestlist l;

	for(l=va_arg(box->ap, UnitTestlist); l; l=l->tl)
		fprint(output, "%u%s", l->hd, l->tl?" ":"");
}

void printparlist(FILE *output, va_list_box *box) {
	Parlist l;

	for(l=va_arg(box->ap, Parlist); l; l=l->tl)
		fprint(output, "%p%s", l->hd, l->tl?" ":"");
}

void printvaluelist(FILE *output, va_list_box *box) {
	Valuelist l;

	for(l=va_arg(box->ap, Valuelist); l; l=l->tl)
		fprint(output, "%v%s", l->hd, l->tl?" ":"");
}

void printregisterlist(FILE *output, va_list_box *box) {
	Registerlist l;

	for(l=va_arg(box->ap, Registerlist); l; l=l->tl)
		fprint(output, "%r%s", l->hd, l->tl?" ":"");
}

void printnamelist(FILE *output, va_list_box *box) {
	Namelist l;

	for(l=va_arg(box->ap, Namelist); l; l=l->tl)
		fprint(output, "%n%s", l->hd, l->tl?" ":"");
}

