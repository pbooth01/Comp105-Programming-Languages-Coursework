#
# Makefile for uscheme-copy
#

SOURCES  = ast-code.c ast.c context-lists.c context-stack.c\
           copy.c env.c error.c eval-stack.c evaldef.c\
           gcdebug.c lex.c list-code.c loc.c name.c options.c\
           overflow.c par-code.c parse.c prim.c print.c\
           read.c root.c scheme-stack.c scheme-tests.c\
           stack-debug.c tests.c value-code.c value.c
HEADERS  = all.h prim.h
OBJECTS  = $(SOURCES:.c=.o)
RESULT   = uscheme-copy

CC = gcc -ansi -pedantic -Wall -Werror -Wextra -Wno-overlength-strings
CFLAGS = -g 
LDFLAGS = -g
CPPFLAGS = -I.
RM = rm -f 

.SUFFIXES:
.SUFFIXES: .c .o

$(RESULT): $(OBJECTS)
	$(CC) -o $@ $(LDFLAGS) $(OBJECTS)

.c.o:
	$(CC) $(CFLAGS) $(CPPFLAGS) -c $<

clean:
	$(RM) $(RESULT) *.o *.core core *~

ast.o: ast.c $(HEADERS)
context-lists.o: context-lists.c $(HEADERS)
context-stack.o: context-stack.c $(HEADERS)
env.o: env.c $(HEADERS)
error.o: error.c $(HEADERS)
eval-stack.o: eval-stack.c $(HEADERS)
evaldef.o: evaldef.c $(HEADERS)
gcdebug.o: gcdebug.c $(HEADERS)
lex.o: lex.c $(HEADERS)
loc.o: loc.c $(HEADERS)
copy.o: copy.c $(HEADERS)
name.o: name.c $(HEADERS)
options.o: options.c $(HEADERS)
overflow.o: overflow.c $(HEADERS)
parse.o: parse.c $(HEADERS)
prim.o: prim.c $(HEADERS)
print.o: print.c $(HEADERS)
read.o: read.c $(HEADERS)
root.o: root.c $(HEADERS)
scheme-stack.o: scheme-stack.c $(HEADERS)
scheme-tests.o: scheme-tests.c $(HEADERS)
stack-debug.o: stack-debug.c $(HEADERS)
tests.o: tests.c $(HEADERS)
value.o: value.c $(HEADERS)
value-code.o: value-code.c $(HEADERS)
ast-code.o: ast-code.c $(HEADERS)
par-code.o: par-code.c $(HEADERS)
list-code.o: list-code.c $(HEADERS)
