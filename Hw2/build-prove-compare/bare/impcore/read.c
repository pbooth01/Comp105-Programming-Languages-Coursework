/* read.c 686d */
#include "all.h"
/* read.c 687a */
Reader stringreader(const char *stringname, const char *s) {
    Reader r = calloc(1, sizeof(*r));
    assert(r);
    r->readername = stringname;
    r->s = s;
    return r;
}
/* read.c 687b */
Reader filereader(const char *filename, FILE *fin) {
    Reader r = calloc(1, sizeof(*r));
    assert(r);
    r->readername = filename;
    r->fin = fin;
    return r;
}
/* read.c 687c */
static void growbuf(Reader r, int n) {
    if (r->nbuf < n) {
        r->buf = realloc(r->buf, n);
        assert(r->buf != NULL);
        r->nbuf = n;
    }
}
/* read.c 687d */
char* readline(Reader r, const char *prompt) {
    if (prompt)
        print("%s", prompt);

    r->line++;
    if (r->fin)

/* set [[r->buf]] to next line from file, or return [[NULL]] if lines are exhausted 688a */
        {
            int n;

            for (n = 0; n == 0 || r->buf[n-1] != '\n'; n = strlen(r->buf)) {
                growbuf(r, n+512);
                if (fgets(r->buf+n, 512, r->fin) == NULL)
                    break;
            }
            if (n == 0)
                return NULL;
            if (r->buf[n-1] == '\n')
                r->buf[n-1] = '\0';
        }
    else if (r->s)

/* set [[r->buf]] to next line from string, or return [[NULL]] if lines are exhausted 688b */
        {
            const char *p;
            int len;

            if ((p = strchr(r->s, '\n')) == NULL)
                return NULL;

            p++;

            len = p - r->s;
            growbuf(r, len);
            strncpy(r->buf, r->s, len);
            r->buf[len-1] = '\0';   /* no newline */

            r->s = p;
        }
    else
        assert(0);

    if (r->buf[0] == ';' && r->buf[1] == '#')
        print("%s\n", r->buf);

    return r->buf;
}
