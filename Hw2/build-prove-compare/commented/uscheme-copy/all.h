/*
 * Interfaces
 * 
 * As in micro-Scheme, we gather all the interfaces into
 * a single C header file.
 * <{\Tt all.h} for \uschemeplus>=
 */
#include <stdarg.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <setjmp.h>
#include <ctype.h>
/*
 * For real implementations, it is convenient to build
 * names from strings. Unlike C strings, names are
 * immutable, and they can be compared using pointer
 * equality.
 * <shared type definitions>=
 */
typedef struct Name *Name;
typedef struct Namelist *Namelist;   /* list of Name */
/*
 * Reading characters
 * 
 * A [[Reader]] encapsulates a source of characters: a
 * string or a file. The [[readline]] function prints a
 * prompt, reads the next line of input from the source,
 * and returns a pointer to the line. The caller needn't
 * worry about how long the line is; [[readline]] always
 * allocates enough memory to hold it. Because
 * [[readline]] reuses the same memory to hold
 * successive lines, it is an unchecked run-time error
 * to retain a pointer returned by [[readline]] after a
 * subsequent call to [[readline]]. A client that needs
 * to save input characters must copy the result of
 * [[readline]] before calling [[readline]] again.
 * <shared type definitions>=
 */
typedef struct Reader *Reader;
/*
 * Reading definitions
 * 
 * An [[XDefreader]] encapsulates a source of (extended)
 * definitions. The [[readxdef]] function returns the
 * next definition. It may also return [[NULL]], if
 * there are no more definitions in the source, or call
 * [[error]] (page [->]), if there is some problem
 * converting the source to abstract syntax.
 * <shared type definitions>=
 */
typedef struct XDefreader *XDefreader;
/*
 * <shared type definitions>=
 */
typedef enum Prompts { NO_PROMPTS, STD_PROMPTS } Prompts;
/*
 * <shared type definitions>=
 */
typedef enum Echo { SILENT, ECHOING } Echo;
/*
 * If you just want to use the functions, without
 * learning how to extend them, you can use only
 * [[print]] and [[eprint]]—ignore [[vprint]] and the
 * details of [[Printer]] and [[installprinter]] that
 * follow.
 * 
 * An extension to [[print]] or [[eprint]] knows how to
 * print one type of C value, like an Impcore expression
 * or value. Such a function has type [[Printer]];
 * a printer consumes and writes one argument from a
 * [[va_list_box]], which holds a list of arguments. [*]
 * <shared type definitions>=
 */
/*
 * Extensible printers are popular with sophisticated
 * C programmers; Hanson (1996, Chapter 14) presents an
 * especially well-crafted example. Unfortunately, some
 * widely used compilers make the construction of
 * extensible printers more difficult than necessary.
 * In particular, not all versions of the GNU C compiler
 * work correctly when values of type [[va_list]] are
 * passed as arguments on the AMD64 platform. [Library
 * functions such as [[vfprintf]] itself are
 * grandfathered; only users cannot write functions that
 * take [[va_list]] arguments. Feh.] \codeindex
 * va-list-box@va_list_box A workaround for this problem
 * is to place the [[va_list]] in a structure and pass a
 * pointer to the structure:
 * <definition of [[va_list_box]]>=
 */
typedef struct va_list_box {
  va_list ap;
} va_list_box;
/*
 * If you are not accustomed to variadic functions and
 * [[stdarg.h]], you may wish to consult Sections
 * 7.2 and 7.3 of \citeNNkernighan:c:2.
 */

typedef void Printer(FILE *output, va_list_box*);
/*
 * <shared type definitions>=
 */
typedef struct Parlist *Parlist; /* list of Par */
/*
 * <shared type definitions>=
 */
typedef struct Par *Par;
typedef enum { ATOM, LIST } Paralt; 
/*
 * <type definitions for \uschemeplus>=
 */
typedef struct Sourcelist *Sourcelist; /* list of Source */
/*
 * <type definitions for \uschemeplus>=
 */
typedef struct Lambda Lambda; 
typedef struct Value Value;
typedef enum {
    NIL, BOOL, NUM, SYM, PAIR, CLOSURE, PRIMITIVE, FORWARD, INVALID
} Valuealt;

/*
 * <type definitions for \uschemeplus>=
 */
typedef struct Def *Def;
typedef enum { VAL, EXP, DEFINE } Defalt; 
typedef struct XDef *XDef;
typedef enum { DEF, USE, TEST } XDefalt; 
typedef struct UnitTest *UnitTest;
typedef enum { CHECK_EXPECT, CHECK_ERROR } UnitTestalt;

typedef struct Exp *Exp;
typedef enum {
    LITERAL, VAR, SET, IFX, WHILEX, BEGIN, LETX, LAMBDAX, APPLY, BREAKX,
    CONTINUEX, RETURNX, THROW, TRY_CATCH, HOLE, WHILE_RUNNING_BODY,
    CALLENV, LETXENV
} Expalt;

/*
 * The stack
 * 
 * The stack is a stack of evaluation contexts, each of
 * which is represented by a [[Frame]].
 * <type definitions for \uschemeplus>=
 */
typedef struct Stack *Stack;
typedef struct Frame Frame;
/*
 * The representation of [[Frame]] is exposed. A frame
 * always includes an evaluation context, which is
 * represented by an expression. If the context is a
 * function application, then we extend the frame by
 * storing the original syntax of the application. That
 * syntax is used in error messages when, e.g., a
 * program tries to apply a value that is not a
 * function. [*]
 */

/*
 * The set of roots includes
 * 
 *  1. The \uschemeplus global environment
 *  2. The set of pending unit tests for any file
 *  currently being read
 *  3. Local variables and actual parameters of any \
 *  uschemeplus function, all of which are found on
 *  the stack of evaluation contexts
 *  4. Local variables and actual parameters of any C
 *  function that calls [[allocate]] or a function
 *  that could allocate (such as [[bindalloc]] or
 *  [[bindalloclist]])
 * 
 * The roots are represented as follows:
 * <type definitions for \uschemeplus>=
 */
typedef Value *Register;  /* pointer to a local variable or a parameter
                             of a C function that could allocate */
typedef struct Registerlist *Registerlist;   /* list of Register */
/*
 * <type definitions for \uscheme>=
 */
typedef enum Letkeyword { LET, LETSTAR, LETREC } Letkeyword;
typedef struct UnitTestlist  *UnitTestlist;  /* list of UnitTest  */
typedef struct Explist  *Explist;            /* list of Exp  */
/*
 * <type definitions for \uscheme>=
 */
typedef struct Valuelist *Valuelist;     /* list of Value */
typedef Value (Primitive)(Exp e, int tag, Valuelist vl);
/*
 * The definition of [[Primitive]] calls for
 * explanation, since it would be simpler to define a
 * primitive function as one that accepts a
 * [[Valuelist]] and returns a [[Value]]. We pass the
 * abstract syntax [[Exp e]] so that if the primitive
 * fails, it can issue a suitable error message. We pass
 * the integer [[tag]] so that we can write a single
 * C function that implements multiple primitives. For
 * example, the arithmetic primitives are implemented by
 * a single function, which makes it possible for them
 * to share the code that ensures both arguments are
 * numbers. Implementations of the primitives appear in
 * Section [->].
 */

/*
 * The Environment and the Store
 * 
 * [*] In the operational semantics, the store sigma is
 * intended to model the machine's memory. It should
 * come as no surprise, then, that we use C pointers (of
 * type [[Value *]]) as locations and the machine's
 * memory as the store. An environment [[Env]] maps
 * names to pointers; find(x, rho) returns rho(x) if x
 * in dom rho; otherwise it returns [[NULL]].
 * <type definitions for \uscheme>=
 */
typedef struct Env *Env;
/*
 * An input reader is just a line-reading function and
 * some extra state.
 * <shared structure definitions>=
 */
struct Reader {
    char *buf;               /* holds the last line read */
    int nbuf;                /* size of buf */
    int line;                /* current line number */
    const char *readername;  /* identifies this reader */

    FILE *fin;               /* filereader */
    const char *s;           /* stringreader */
};
/*
 * <shared structure definitions>=
 */
struct Par { Paralt alt; union { Name atom; Parlist list; } u; }; 
/*
 * The stack holds all the local variables I need to
 * keep from [[readevalprint]]: the reader for
 * definitions, the file read from (so I can close it),
 * the [[echo]] flag, and finally the pending unit
 * tests. [*]
 * <structure definitions for \uschemeplus>=
 */
typedef struct Source {
    XDefreader xdefs;   /* definitions being read */
    FILE *sourcefile;   /* file from which definitions originate, or NULL */
    Echo echo;          /* whether [[evaldef]] should echo */
    UnitTestlist tests; /* pending unit tests */
} Source;
/*
 * <structure definitions for \uschemeplus>=
 */
struct Lambda { Namelist formals; Exp body; }; 
struct Value {
    Valuealt alt;
    union {
        int bool;
        int num;
        Name sym;
        struct { Value *car; Value *cdr; } pair;
        struct { Lambda lambda; Env env; } closure;
        struct { int tag; Primitive *function; } primitive;
        Value *forward;
        const char *invalid;
    } u;
};

/*
 * <structure definitions for \uschemeplus>=
 */
struct Def {
    Defalt alt;
    union {
        struct { Name name; Exp exp; } val;
        Exp exp;
        struct { Name name; Lambda lambda; } define;
    } u;
};

struct XDef {
    XDefalt alt; union { Def def; Name use; UnitTest test; } u;
};

struct UnitTest {
    UnitTestalt alt;
    union {
        struct { Exp check; Exp expect; } check_expect; Exp check_error;
    } u;
};

struct Exp {
    Expalt alt;
    union {
        Value literal;
        Name var;
        struct { Name name; Exp exp; } set;
        struct { Exp cond; Exp true; Exp false; } ifx;
        struct { Exp cond; Exp body; } whilex;
        Explist begin;
        struct { Letkeyword let; Namelist nl; Explist el; Exp body; } letx;
        Lambda lambdax;
        struct { Exp fn; Explist actuals; } apply;
        Exp returnx;
        Exp throw;
        struct { Exp body; Exp handler; } try_catch;
        Env callenv;
        Env letxenv;
    } u;
};

/*
 * <structure definitions for \uschemeplus>=
 */
struct Sourcelist {
	Source hd;
	Sourcelist tl;
};

struct Explist {
	Exp hd;
	Explist tl;
};

struct UnitTestlist {
	UnitTest hd;
	UnitTestlist tl;
};

struct Parlist {
	Par hd;
	Parlist tl;
};

struct Valuelist {
	Value hd;
	Valuelist tl;
};

struct Registerlist {
	Register hd;
	Registerlist tl;
};

struct Namelist {
	Name hd;
	Namelist tl;
};

/*
 * <structure definitions for \uschemeplus>=
 */
struct Frame {
    struct Exp context;     /* mutated in place during evaluation */
    Exp syntax;
                           /* when not NULL, kept pristine for error messages */
};
/*
 * To make [[visitenv]] to work, we have to expose the
 * representation of environments. (In Chapter [->],
 * this representation is private.)
 * <structure definitions for \uschemeplus>=
 */
struct Env {
    Name name;
    Value *loc;
    Env tl;
};
/*
 * Pointer comparison is built into C, but we provide
 * two other operations on names.
 * <shared function prototypes>=
 */
Name strtoname(const char *s);
const char *nametostr(Name n);
/*
 * These functions satisfy the following algebraic laws:
 * 
 *  [[strcmp(s, nametostr(strtoname(s))) == 0]]
 *  [[strcmp(s, t) == 0]] if and only if [[strtoname
 *  (s) == strtoname(t)]]
 * 
 * Informally, the first law says if you build a name
 * from a string, [[nametostr]] returns a copy of your
 * original string. The second law says you can compare
 * names using pointer equality.
 */

/*
 * Function [[printname]] prints names.
 * <shared function prototypes>=
 */
Printer printname;
/*
 * <shared function prototypes>=
 */
char *readline(Reader r, const char *prompt);
/*
 * Each of our interpreters leaves the prompt empty when
 * not reading interactively.
 */

/*
 * To create a reader, we need a string or a file.
 * A reader also gets a name, which we can use in error
 * messages. [*] [*]
 * <shared function prototypes>=
 */
Reader stringreader(const char *stringname, const char *s);
Reader filereader  (const char *filename,   FILE *fin);
/*
 * <shared function prototypes>=
 */
XDef readxdef(XDefreader r);
/*
 * To create a definition reader, we need a source of
 * characters.
 */

/*
 * <shared function prototypes>=
 */
XDefreader xdefreader(Reader r, Prompts prompts);
/*
 * The [[prompts]] parameter controls whether the
 * resulting [[XDefreader]] prompts when reading input.
 */

/*
 * Printing
 * 
 * [*] The standard C function [[printf]] and its
 * siblings [[fprintf]] and [[vfprintf]] convert
 * internal C values to sequences of characters in an
 * output file. Unfortunately, they convert only
 * numbers, characters, and strings. It would be a lot
 * more convenient if [[printf]] also converted [[Name]]
 * s, [[Exp]]s, and so on. We can't change [[printf]],
 * so instead we define new functions. Functions
 * [[print]] and [[fprint]] are a bit like [[printf]]
 * and [[fprintf]].
 * <shared function prototypes>=
 */
void print (const char *fmt, ...);  /* print to standard output */
void fprint(FILE *output, const char *fmt, ...);  /* print to given file */
/*
 * The advantage of our versions is that they are
 * extensible; if we want to print new kinds of values,
 * like Impcore expressions, we can add new printing
 * functions.
 */

/*
 * To tell [[print]] and [[eprint]] about a new
 * extension, we announce the extension by calling
 * [[installprinter]] with the extension and with a
 * character that is used for the extension's
 * ``conversion specification.''
 * <shared function prototypes>=
 */
void installprinter(unsigned char c, Printer *fmt);
/*
 * The printer interface provides printers to format
 * percent signs, strings, and decimal integers.
 * <shared function prototypes>=
 */
Printer printpercent, printstring, printdecimal;
/*
 * If you are a sophisticated C programmer, you may want
 * to use our analog of [[vfprintf]]:
 * <shared function prototypes>=
 */
void vprint(FILE *output, const char *fmt, va_list_box *box);
/*
 * Error handling
 * 
 * Any module can signal an error by calling [[error]].
 * In its normal mode of operation [[error]] acts just
 * like [[eprint]] except that it does not return after
 * printing; instead it [[longjmp]]s to [[errorjmp]]. 
 * [*]
 * <shared function prototypes>=
 */
void error(const char *fmt, ...);
extern jmp_buf errorjmp;        /* longjmp here on error */
/*
 * The [[error]] function also has a testing mode of
 * operation, in which it prints nothing and [[longjmp]]
 * s to [[testingjmp]]. The testing mode is used to
 * implement [[check-expect]] and [[check-error]].
 */

/*
 * <shared function prototypes>=
 */
typedef enum ErrorMode { NORMAL, TESTING } ErrorMode;
void set_error_mode(ErrorMode mode);
extern jmp_buf testjmp;
/*
 * The error mode is initially [[NORMAL]], but it can be
 * changed using [[set_error_mode]].
 */

/*
 * Function [[checkargc]] is used to check the number of
 * arguments to both user-defined and primitive
 * functions. The first argument is an abstract-syntax
 * tree representing the application being checked; if
 * [[expected]] != [[actual]], [[checkargc]] calls
 * [[error]], passing a message that contains [[e]].
 * <shared function prototypes>=
 */
void checkargc(Exp e, int expected, int actual);
/*
 * The [[duplicatename]] function finds a duplicate name
 * on a [[Namelist]] if such a name exists. It is used
 * to check that formal parameters to user-defined
 * functions all have different names. If the name list
 * [[names]] contains a duplicate occurrence of any
 * name, the function returns such a name; otherwise it
 * returns [[NULL]].
 * <shared function prototypes>=
 */
Name duplicatename(Namelist names);
/*
 * <shared function prototypes>=
 */
void report_test_results(int npassed, int ntests);
/*
 * The implementations of these two functions appear in
 * \apprefimpcore. With them in hand, this is how
 * [[readevalprint]] runs the tests:
 */

/*
 * <shared function prototypes>=
 */
Printer printpar;
/*
 * The [[readparlist]] function reads and returns a list
 * of parenthesized expressions from the given
 * [[Reader]], stopping when an end-of-expression and
 * end-of-line coincide. The [[doquote]] flag specifies
 * that an expression like [['(1 2 3)]] should be turned
 * into [[(quote (1 2 3))]]; it is used to implement
 * micro-Scheme. The [[doprompt]] flag specifies that
 * prompts should be printed when reading input lines.
 * <shared function prototypes>=
 */
Parlist readparlist(Reader r, int doquote, Prompts prompts);
/*
 * Stack-overflow detection
 * 
 * The first call to [[checkoverflow]] sets a
 * ``low-water mark'' in the stack. Each later call
 * checks the current stack pointer against that
 * low-water mark. If the distance exceeds [[limit]],
 * [[checkoverflow]] calls [[error]]. Otherwise it
 * returns the distance.
 * <shared function prototypes>=
 */
extern int checkoverflow(int limit);
/*
 * <shared function prototypes>=
 */
Par mkAtom(Name atom);
Par mkList(Parlist list);
struct Par mkAtomStruct(Name atom);
struct Par mkListStruct(Parlist list);
/*
 * <function prototypes for \uscheme>=
 */
int gammadesired(int defaultval, int minimum);
/*
 * <function prototypes for \uscheme>=
 */
Value *find(Name name, Env env);
/*
 * The function [[bindalloc]] binds a name to a freshly
 * allocated location, and it puts a value in that
 * location. Formally, when called with store sigma,
 * bindalloc(x, v, rho) chooses an l\notindom sigma,
 * updates the store to be sigma{l|->v}, and returns the
 * extended environment rho{x |->l}.
 * <function prototypes for \uscheme>=
 */
Env bindalloc    (Name name,   Value v,      Env env);
Env bindalloclist(Namelist nl, Valuelist vl, Env env);
/*
 * Calling bindalloclist(<\ldotsnx>, <\ldotsnv>, rho)
 * does the same job for a list of values, returning rho
 * {x_1 |->l_1, ..., x_n |->l_n}, where \ldotsnl are
 * fresh locations, which [[bindalloclist]] initializes
 * to values \ldotsnv.
 */

/*
 * Allocation
 * 
 * The fresh locations created by [[bindalloc]] and
 * [[bindalloclist]] come from [[allocate]]. Calling
 * [[allocate(v)]] finds a location l\notindom sigma,
 * stores [[v]] in l (thereby updating sigma), and
 * returns l.
 * <function prototypes for \uscheme>=
 */
Value *allocate(Value v);
/*
 * <function prototypes for \uscheme>=
 */
void initallocate(void);
/*
 * Chapter [->] describes allocation in detail.
 */

/*
 * Values
 * 
 * [*] The representation of values appears in chunk 
 * [->] in Section [->]. The value interface also
 * exports predefined values [[truev]] and [[falsev]],
 * which represent [[#t]] and [[#f]].
 * <function prototypes for \uscheme>=
 */
Value truev, falsev;
/*
 * Before executing any code that refers to [[truev]] or
 * [[falsev]], clients must call [[initvalue]].
 */

/*
 * <function prototypes for \uscheme>=
 */
void initvalue(void);
/*
 * Function [[istrue]] takes a value and returns [[1]]
 * if the value should be regarded as true (i.e., is not
 * [[#f]]) and [[0]] otherwise.
 * <function prototypes for \uscheme>=
 */
int istrue(Value v);
/*
 * Function [[unspecified]] returns an unspecified
 * value.
 * <function prototypes for \uscheme>=
 */
Value unspecified(void);
/*
 * For example, \monoboxeval(e, rho), when evaluated
 * with store sigma, finds a v and a sigma' such that \
 * evale ==>\evalr[']v, updates the store to be sigma',
 * and returns v.
 * <function prototypes for \uscheme>=
 */
Value eval   (Exp e, Env rho);
Env   evaldef(Def d, Env rho, Echo echo);
/*
 * Similarly, \monoboxevaldef(e, rho, echo), when
 * evaluated with store sigma, finds a rho' and a sigma'
 * such that \evaldefe -->\evaldefr', updates the store
 * to be sigma', and returns rho'. If [[echo]] is
 * nonzero, [[evaldef]] also prints the name or value of
 * whatever expression is evaluated or added to rho.
 */

/*
 * In our implementation of micro-Scheme,
 * [[readevalprint]] has side effects on an environment
 * pointer that is passed by reference. This technique
 * ensures that in an interactive session, errors don't
 * destroy the results of definitions and [[val]]
 * bindings that precede the error. If [[readevalprint]]
 * simply returned a new environment, then when an error
 * occurred, any partially created environment would be
 * lost.
 * <function prototypes for \uscheme>=
 */
void readevalprint(XDefreader r, Env *envp, Echo echo);
/*
 * Primitives
 * 
 * Compared to Impcore, micro-Scheme has many
 * primitives. The function [[primenv]] returns an
 * environment binding all the primitive operations.
 * <function prototypes for \uscheme>=
 */
Env primenv(void);
/*
 * Here are some of the printing functions used.
 * <function prototypes for \uscheme>=
 */
void printenv    (FILE *output, va_list_box*);
void printvalue  (FILE *output, va_list_box*);
void printclosure(FILE *output, va_list_box*);
void printexp    (FILE *output, va_list_box*);
void printdef    (FILE *output, va_list_box*);
void printlambda (FILE *output, va_list_box*);
/*
 * Running unit tests
 * 
 * The most recent unit test is stored at the head of
 * the list, but they should be run in the order in
 * which they appear. The simplest way is to use a
 * recursive function, which can also count the number
 * of tests passed.
 * <function prototypes for \uscheme>=
 */
int tests_passed(UnitTestlist tests, Env rho);
/*
 * Function [[equalpairs]] tests for equality of atoms
 * and pairs. It is nearly a duplicate of the primitive
 * [[=]] function, except it returns a C Boolean, not a
 * micro-Scheme Boolean.
 * <function prototypes for \uscheme>=
 */
int equalpairs(Value v, Value w);
/*
 * The rest of the interface is just the allocator. The
 * managed-heap function [[allocloc]] provides an
 * uninitialized location; chunk [->] shows how to use
 * [[allocloc]] to implement [[allocate]].
 * <function prototypes for \uscheme>=
 */
Value *allocloc(void);
/*
 * [*] The allocator and [[roots]] structure are related
 * by this precondition: Clients may call [[allocloc]]
 * only when all objects that could lead to live values
 * appear in [[roots]]. The copying collector's
 * implementation of [[allocloc]] also requires that,
 * when called, all pointers to allocated values must be
 * reachable from [[roots]], so that they can be updated
 * when the values move.
 */

/*
 * The interpreter has one more obligation: before
 * calling [[allocloc]], it must call [[initallocate]].
 * <function prototypes for \uscheme>=
 */
void initallocate(void);
/*
 * Checking for cycles in cons
 * 
 * I've left in this early-stage debugging code, which
 * looks for a cycle after every [[cons]].
 * <function prototypes for \uschemeplus>=
 */
void cyclecheck(Value *l);
/*
 * <function prototypes for \uschemeplus>=
 */
Source mkSource(XDefreader defs, FILE *f, Echo echo);
/*
 * <function prototypes for \uschemeplus>=
 */
void addprimitives(Env *envp);
/*
 * <function prototypes for \uschemeplus>=
 */
Lambda mkLambda(Namelist formals, Exp body);
Value mkNil(void);
Value mkBool(int bool);
Value mkNum(int num);
Value mkSym(Name sym);
Value mkPair(Value *car, Value *cdr);
Value mkClosure(Lambda lambda, Env env);
Value mkPrimitive(int tag, Primitive *function);
Value mkForward(Value *forward);
Value mkInvalid(const char *invalid);
/*
 * <function prototypes for \uschemeplus>=
 */
Def mkVal(Name name, Exp exp);
Def mkExp(Exp exp);
Def mkDefine(Name name, Lambda lambda);
struct Def mkValStruct(Name name, Exp exp);
struct Def mkExpStruct(Exp exp);
struct Def mkDefineStruct(Name name, Lambda lambda);
XDef mkDef(Def def);
XDef mkUse(Name use);
XDef mkTest(UnitTest test);
struct XDef mkDefStruct(Def def);
struct XDef mkUseStruct(Name use);
struct XDef mkTestStruct(UnitTest test);
UnitTest mkCheckExpect(Exp check, Exp expect);
UnitTest mkCheckError(Exp check_error);
struct UnitTest mkCheckExpectStruct(Exp check, Exp expect);
struct UnitTest mkCheckErrorStruct(Exp check_error);
Exp mkLiteral(Value literal);
Exp mkVar(Name var);
Exp mkSet(Name name, Exp exp);
Exp mkIfx(Exp cond, Exp true, Exp false);
Exp mkWhilex(Exp cond, Exp body);
Exp mkBegin(Explist begin);
Exp mkLetx(Letkeyword let, Namelist nl, Explist el, Exp body);
Exp mkLambdax(Lambda lambdax);
Exp mkApply(Exp fn, Explist actuals);
Exp mkBreakx(void);
Exp mkContinuex(void);
Exp mkReturnx(Exp returnx);
Exp mkThrow(Exp throw);
Exp mkTryCatch(Exp body, Exp handler);
Exp mkHole(void);
Exp mkWhileRunningBody(void);
Exp mkCallenv(Env callenv);
Exp mkLetxenv(Env letxenv);
struct Exp mkLiteralStruct(Value literal);
struct Exp mkVarStruct(Name var);
struct Exp mkSetStruct(Name name, Exp exp);
struct Exp mkIfxStruct(Exp cond, Exp true, Exp false);
struct Exp mkWhilexStruct(Exp cond, Exp body);
struct Exp mkBeginStruct(Explist begin);
struct Exp mkLetxStruct(Letkeyword let, Namelist nl, Explist el, Exp body);
struct Exp mkLambdaxStruct(Lambda lambdax);
struct Exp mkApplyStruct(Exp fn, Explist actuals);
struct Exp mkBreakxStruct(void);
struct Exp mkContinuexStruct(void);
struct Exp mkReturnxStruct(Exp returnx);
struct Exp mkThrowStruct(Exp throw);
struct Exp mkTryCatchStruct(Exp body, Exp handler);
struct Exp mkHoleStruct(void);
struct Exp mkWhileRunningBodyStruct(void);
struct Exp mkCallenvStruct(Env callenv);
struct Exp mkLetxenvStruct(Env letxenv);
/*
 * <function prototypes for \uschemeplus>=
 */
int        lengthSL (Sourcelist l);
Source     nthSL    (Sourcelist l, unsigned n);
Sourcelist mkSL     (Source hd, Sourcelist tl);
Sourcelist popSL     (Sourcelist l);
Printer    printsourcelist;
/*
 * <function prototypes for \uschemeplus>=
 */
int     lengthEL (Explist l);
Exp     nthEL    (Explist l, unsigned n);
Explist mkEL     (Exp hd, Explist tl);
Explist popEL     (Explist l);
Printer printexplist;
/*
 * <function prototypes for \uschemeplus>=
 */
int          lengthUL (UnitTestlist l);
UnitTest     nthUL    (UnitTestlist l, unsigned n);
UnitTestlist mkUL     (UnitTest hd, UnitTestlist tl);
UnitTestlist popUL     (UnitTestlist l);
Printer      printunittestlist;
/*
 * <function prototypes for \uschemeplus>=
 */
int     lengthPL (Parlist l);
Par     nthPL    (Parlist l, unsigned n);
Parlist mkPL     (Par hd, Parlist tl);
Parlist popPL     (Parlist l);
Printer printparlist;
/*
 * <function prototypes for \uschemeplus>=
 */
int       lengthVL (Valuelist l);
Value     nthVL    (Valuelist l, unsigned n);
Valuelist mkVL     (Value hd, Valuelist tl);
Valuelist popVL     (Valuelist l);
Printer   printvaluelist;
/*
 * <function prototypes for \uschemeplus>=
 */
int          lengthRL (Registerlist l);
Register     nthRL    (Registerlist l, unsigned n);
Registerlist mkRL     (Register hd, Registerlist tl);
Registerlist popRL     (Registerlist l);
Printer      printregisterlist;
/*
 * <function prototypes for \uschemeplus>=
 */
int      lengthNL (Namelist l);
Name     nthNL    (Namelist l, unsigned n);
Namelist mkNL     (Name hd, Namelist tl);
Namelist popNL     (Namelist l);
Printer  printnamelist;
/*
 * Scheme, S-expressions, and first-class functions
 * 
 * [*]
 * 
 */

/*
 * A [[Stack]] is a mutable datatype. A [[Stack]] is
 * created by [[emptystack]], and it is mutated by
 * [[pushcontext]], [[popframe]], and [[clearstack]]
 * (which pops all the remaining frames). [*]
 * <function prototypes for \uschemeplus>=
 */
Stack  emptystack  (void);
Exp    pushcontext (struct Exp e, Stack s);
void   popframe    (Stack s);
void   clearstack  (Stack s);
/*
 * Function [[pushcontext]] pushes a frame in which
 * [[syntax]] is [[NULL]].
 */

/*
 * Function [[topframe]] returns a pointer to the frame
 * on the top of a stack, or if the stack is empty, it
 * returns [[NULL]]. It is OK to mutate a stack [[s]] by
 * writing through [[topframe(s)]].
 * <function prototypes for \uschemeplus>=
 */
Frame *topframe (Stack s);  /* NULL if empty */
/*
 * A special function is used to push a [[CALLENV]] or
 * [[LETXENV]] context. As described in \secref
 * schemes.tail-calls, this function can optimize tail
 * calls.
 * <function prototypes for \uschemeplus>=
 */
void   pushenv_opt (Env env, Expalt context, Stack s);  /* may optimize */
/*
 * As always with tricolor marking, we begin with the
 * roots, making the objects they point to gray. We then
 * turn gray objects black until there aren't any more
 * gray objects. In concrete terms, we first forward all
 * the pointers that are in roots, then forward pointers
 * in gray objects until there are no more.
 * <function prototypes for \uschemeplus>=
 */
void stack_trace_init(int *countp);  /* how many steps to show */
void stack_trace_current_expression(Exp e,   Env rho, Stack s);
void stack_trace_current_value     (Value v, Env rho, Stack s);
/*
 * Function [[getoption]] returns the value of an
 * option, or if the option is not set, it returns
 * [[defaultval]].
 * <function prototypes for \uschemeplus>=
 */
Value getoption(Name name, Env env, Value defaultval);
/*
 * Diagnostic code for \chaprefgc
 * 
 * \chaprefgc, which follows this chapter, builds on
 * this interpreter to implement automatic memory
 * management. The [[validate]] function is used to
 * debug garbage collectors; provided the argument [[v]]
 * represents a valid value, [[validate(v)]] returns 
 * [[v]].
 * <function prototypes for \uschemeplus>=
 */
Value validate(Value v);
/*
 * Updating lists of expressions within contexts
 * 
 * [*] Function application, the \xlet family, and \
 * xbegin all require evaluating expressions in
 * sequence. Looking at the \rulename
 * Small-Step-Apply-Next-Arg rule on page [->],
 * how should we implement the transition from a context
 * like \mathbox\capply(v_f, v_1, ..., v_i-1, \hole,
 * e_i+1, ..., e_n) to a context like \mathbox\capply
 * (v_f, v_1, ..., v_i-1, v, \hole, e_i+2, ..., e_n)?
 * We can represent \mathbox v_1, ..., v_i-1, \hole,
 * e_i+1, ..., e_n as a value of type [[Explist]], where
 * expressions e_i+1...e_n come from the original
 * syntax, and values v_1, ..., v_i-1 are represented as
 * \xliteral expressions. Then, to make the state
 * transition, we can overwrite the hole with v_i, and
 * we can overwrite e_i+1 with a hole. This operation is
 * so common that I have defined a function for it:
 * function [[transition_explist]] overwrites the hole
 * with [[v]], writes a new hole one position to the
 * right, and returns the expression that is overwritten
 * by the new hole. That expression is stored in static
 * memory, so subsequent calls to [[transition_memory]]
 * overwrite previous results.
 * <function prototypes for \uschemeplus>=
 */
Exp transition_explist(Explist es, Value v); /* pointer to static memory */
/*
 * If the hole is in the rightmost position,
 * [[transition_explist]] overwrites the hole with [[v]]
 * and then returns [[NULL]].
 */

/*
 * What about initializing a context by putting a hole
 * in the first position? Function
 * [[head_replaced_with_hole]] works much like
 * [[transition_explist]]: it puts a hole in the initial
 * position and returns a pointer to the expression that
 * was there. If the list is empty, so there is no
 * initial position, it returns null.
 * <function prototypes for \uschemeplus>=
 */
Exp head_replaced_with_hole(Explist es);
                                     /* shares memory with transition_explist */
/*
 * A function like [[transition_explist]] really helps
 * implement \xapply, \xlet, and \xletrec, but we have
 * to be careful: if we are overwriting an [[Explist]],
 * we can't use the original [[Explist]] from the
 * syntax—we have to use a copy. The time to make the
 * copy is when we first push the context that contains
 * the [[Explist]]. For example, when I push a context
 * like \xapply(\hole, \ldotsne), I copy the list of
 * expressions using function [[copyEL]]:
 * 
 *  \monopushcontext(mkApplyStruct(mkHole(), copyEL(\
 *  ldotsne)), evalstack).
 * 
 * Function [[copyEL]] copies a list of expressions, and
 * when I'm finished with the copy, [[freeEL]] recovers
 * the memory.
 * <function prototypes for \uschemeplus>=
 */
Explist copyEL(Explist el);
void    freeEL(Explist el);
/*
 * We'll call [[freeEL]] when popping a context that
 * contains a copied [[Explist]].
 */

/*
 * When an [[Explist]] appears in an \xapply, \xlet, or
 * \xletrec context, each element goes through three
 * states:
 * 
 *  1. Initially it points to fresh memory that contains
 *  a copy of syntax from the original expression.
 *  2. At some point the syntax is copied into static
 *  memory and the element's own memory is
 *  overwritten to contain a hole.
 *  3. Finally the element's own memory is overwritten
 *  with the value that results from evaluating the
 *  original expression.
 * 
 * Once very element has reached the final state, the
 * [[Explist]] contains only literals, and we can
 * convert it to a list of values:
 * <function prototypes for \uschemeplus>=
 */
Valuelist asLiterals(Explist es);
Value     asLiteral (Exp e);
/*
 * Function [[asLiteral]] implements the same
 * conversion, but for a single [[Exp]]. And because
 * function [[asLiterals]] has to allocate, I provide
 * [[freeVL]], which frees the memory allocated by
 * [[asLiterals]].
 */

/*
 * <function prototypes for \uschemeplus>=
 */
void freeVL(Valuelist vl);
/*
 * Armed with these tools, we're ready to interpret
 * forms that evaluate expressions in sequence.
 */

/*
 * Printing the stack
 * 
 * Here are the functions used to print frames and
 * stacks. Function [[printnoenv]] prints the current
 * environment as a C pointer, rather than as a list of
 * (name, value) pairs.
 * <function prototypes for \uschemeplus>=
 */
void printstack   (FILE *, va_list_box*);
void printoneframe(FILE *, va_list_box*);
void printframe   (FILE *, Frame *fr);
void printnoenv   (FILE *, va_list_box*);
/*
 * Register roots are added and removed in last-in,
 * first-out order. For this purpose \apprefgcsa
 * provides functions [[pushreg]] and [[popreg]]. If the
 * pointer passed to [[popreg]] is not equal to the
 * pointer passed to the matching [[pushreg]], it is a
 * checked run-time error.
 * <function prototypes for \uschemeplus>=
 */
#ifndef DEBUG_GC_REGISTERS    /*OMIT*/
void pushreg(Value *reg);
void popreg (Value *reg);
#endif  /*OMIT*/
/*
 * We may also need to push or pop all the registers on
 * a list of values.
 */

/*
 * <function prototypes for \uschemeplus>=
 */
void pushregs(Valuelist regs);
void popregs (Valuelist regs);
/*
 * After a block of memory has been acquired (via
 * [[malloc]] or [[calloc]]) to hold heap objects, but
 * before any object has been delivered to the
 * interpreter via [[allocloc]], code should call
 * [[gc_debug_post_acquire]].
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_post_acquire(Value *mem, unsigned nvalues); 
/*
 * This function must be used carefully; the copying
 * collector can announce the acquisition of an entire
 * block at once, but because the mark-and-sweep
 * collector wraps each [[Value]] in an [[Mvalue]], it
 * must call [[gc_debug_post_acquire]] on one object at
 * a time.
 */

/*
 * When a block of memory that belongs to the collector
 * is no longer needed and is about to be released, code
 * should call [[gc_debug_pre_release]] just before
 * calling [[free]]. As with acquisition, the
 * mark-and-sweep collector must release one object at a
 * time.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_pre_release(Value *mem, unsigned nvalues); 
/*
 * Just before the allocator delivers a heap object to
 * the interpreter, it should call
 * [[gc_debug_pre_allocate]].
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_pre_allocate(Value *mem); 
/*
 * After the garbage collector decides an object is
 * unreachable, it should call function
 * [[gc_debug_post_reclaim]]. This function should be
 * called after the collector has finished writing to
 * any part of the object. The function will mark the
 * object [[INVALID]], and then until the object is
 * allocated again, Valgrind will complain about writes
 * to it. Even if you don't have Valgrind, the function
 * is still useful; for example, if a value like [['(a b
 * c)]] mysteriously turns into [['(a b . <invalid>)]],
 * you've discovered a premature reclamation.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_post_reclaim(Value *mem); 
/*
 * For the convenience of the copying collector, which
 * reclaims an entire semispace at one time, it can call
 * [[gc_debug_post_reclaim_block]]. Be careful if you're
 * reclaiming a semispace because the heap grew—you'll
 * need to pass the old, smaller size, not the new,
 * larger size.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_post_reclaim_block(Value *mem, unsigned nvalues); 
/*
 * Whenever the interpreter uses a value that is
 * obtained by dereferencing a pointer into the heap, it
 * should wrap that value in [[validate]]. If the
 * [[alt]] field of [[v]] is [[INVALID]], [[validate
 * (v)]] halts the program with an error message.
 * Otherwise it returns [[v]]. The interpreter in \
 * chaprefschemes calls [[validate]], and your own code
 * should also call [[validate]] whenever it reads a
 * value that you believe should be good.
 * <function prototypes for \uschemeplus>=
 */
Value validate(Value v);
/*
 * If the tools above aren't enough, you can use
 * functions [[gcprint]] and [[gcprintf]] to write any
 * information you like to standard error. Function
 * [[gcprint]] works like [[print]], and [[gcprintf]]
 * works like [[printf]], but they work only when the
 * environment variable GCVERBOSE is set. If GCVERBOSE
 * is not set, [[gcprint]] and [[gcprintf]] do nothing.
 * <function prototypes for \uschemeplus>=
 */
void gcprint (const char *fmt, ...);  /* print GC debugging info */
void gcprintf(const char *fmt, ...);
/*
 * Finally, the debug code needs to be initialized—but
 * [[initallocate]] in \apprefgcsa does the
 * initialization.
 * <function prototypes for \uschemeplus>=
 */
void gc_debug_init(void);
/*
 * Finally, global variable [[high_stack_mark]] tracks
 * the maximum number of frames held on the stack during
 * an evaluation.
 * <global variables for \uschemeplus>=
 */
extern int high_stack_mark;
/*
 * Instrumentation
 * 
 * An option is a \uschemeplus variable that might be
 * set to influence the behavior of the interpreter.
 * 
 *   • Option [[ --- optimize-tail-calls]], if set to
 *  [[#f]], prevents the interpreter from optimizing
 *  tail calls.
 *   • Option [[ --- show-high-stack-mark]], if set to a
 *  non-[[#f]] value, prints the maximum size of the
 *  stack after each definition is evaluated.
 *   • Option [[ --- trace-stack]], if set to a
 *  nonnegative number n, shows the abstract-machine
 *  state for n steps. If [[ --- trace-stack]] is
 *  negative, all steps are shown. It is an unchecked
 *  run-time error to change [[ --- trace-stack]]
 *  from a number to a non-number.
 * 
 * Options [[ --- optimize-tail-calls]] and [[ ---
 * show-high-stack-mark]] are used to set these global
 * variables:
 * <global variables for \uschemeplus>=
 */
extern int optimize_tail_calls;
extern int show_high_stack_mark;
/*
 * The root type and its variables are visible to all
 * C code.
 * <{\Tt all.h} for \uschemeplus>=
 */
/*
 * <structure definitions used in garbage collection>=
 */
struct Roots {
    Env *globals;            /* global variables from the user's program */
    Sourcelist sources;      /* the definition sources being read from */
    Stack stack;
                        /* the uscheme+ stack, with all parameters and locals */
    Registerlist registers;  /* pointers to 'machine registers' */
};
/*
 * At initialization we create a [[gc_pool]], which
 * stands for all objects allocated using [[allocloc]].
 * The flag [[gc_uses_mark_bits]], if set, tells
 * Valgrind that when memory is first allocated, its
 * contents are zero. We also initialize the
 * [[gcverbose]] flag.
 * <global variables used in garbage collection>=
 */
extern int gc_uses_mark_bits;
/*
 * This is the data structure that is shared between the
 * interpreter and the garbage collector. The
 * interpreter makes sure that before any call to
 * [[allocate]], [[roots]] is up to date and contains
 * pointers to any locations that could affect the rest
 * of the computation. The garbage collector inspects
 * the roots and also updates pointers to any objects
 * that it moves.
 * <global variables used in garbage collection>=
 */
extern struct Roots roots;
