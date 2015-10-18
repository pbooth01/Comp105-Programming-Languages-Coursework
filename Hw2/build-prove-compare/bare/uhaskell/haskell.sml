(* haskell.sml (BUG: software can't tell where this code came from) *)


(*****************************************************************)
(*                                                               *)
(*   CONVENIENCE FUNCTIONS FOR WRITING TO STANDARD OUTPUT AND STANDARD ERROR *)
(*                                                               *)
(*****************************************************************)

(* convenience functions for writing to standard output and standard error 791c *)
fun println  s = (print s; print "\n")
fun eprint   s = TextIO.output (TextIO.stdErr, s)
fun eprintln s = (eprint s; eprint "\n")
(* convenience functions for writing to standard output and standard error 791d *)
fun basisError s = (eprint "while reading initial basis, "; eprintln s)


(*****************************************************************)
(*                                                               *)
(*   ENVIRONMENTS                                                *)
(*                                                               *)
(*****************************************************************)

(* environments 306 *)
type name = string
type 'a env = (name * 'a) list
val emptyEnv = []

(* lookup and assignment of existing bindings *)
exception NotFound of name
fun find (name, []) = raise NotFound name
  | find (name, (n, v)::tail) = if name = n then v else find (name, tail)

(* adding new bindings *)
exception BindListLength
fun bind (name, v, rho) = (name, v) :: rho
fun bindList (n::vars, v::vals, rho) = bindList (vars, vals, bind (n, v, rho))
  | bindList ([], [], rho) = rho
  | bindList _ = raise BindListLength
(* type declarations for consistency checking *)
val _ = op emptyEnv : 'a env
val _ = op find     : name * 'a env -> 'a
val _ = op bind     : name      * 'a      * 'a env -> 'a env
val _ = op bindList : name list * 'a list * 'a env -> 'a env


(*****************************************************************)
(*                                                               *)
(*   HINDLEY-MILNER TYPES                                        *)
(*                                                               *)
(*****************************************************************)

(* Hindley-Milner types ((ml/haskell)) 388 *)
datatype ty = TYVAR  of name                (* type variable alpha *)
            | TYCON  of name                (* type constructor mu *)
            | CONAPP of ty * ty list        (* type-level application *)

datatype type_scheme = FORALL of name list * ty
(* Hindley-Milner types ((ml/haskell)) 391a *)
type subst = ty env
fun varsubst theta = 
  (fn a => find (a, theta) handle NotFound _ => TYVAR a)
(* type declarations for consistency checking *)
type subst = subst
val _ = op varsubst : subst -> (name -> ty)
(* Hindley-Milner types ((ml/haskell)) 391b *)
fun tysubst theta =
  let fun subst (TYVAR a) = varsubst theta a
        | subst (TYCON c) = TYCON c
        | subst (CONAPP (tau, taus)) = CONAPP (subst tau, map subst taus)
(* type declarations for consistency checking *)
val _ = op tysubst : subst -> (ty -> ty)
val _ = op subst   :           ty -> ty
  in  subst
  end
(* Hindley-Milner types ((ml/haskell)) 391c *)
(* sets of free type variables ((ml/haskell)) 415a *)
type 'a set = 'a list
val emptyset = []
fun member x = 
  List.exists (fn y => y = x)
fun insert (x, ys) = 
  if member x ys then ys else x::ys
fun union (xs, ys) = foldl insert ys xs
fun inter (xs, ys) =
  List.filter (fn x => member x ys) xs
fun diff  (xs, ys) = 
  List.filter (fn x => not (member x ys)) xs
(* type declarations for consistency checking *)
type 'a set = 'a set
val _ = op emptyset : 'a set
val _ = op member   : ''a -> ''a set -> bool
val _ = op insert   : ''a     * ''a set  -> ''a set
val _ = op union    : ''a set * ''a set  -> ''a set
val _ = op inter    : ''a set * ''a set  -> ''a set
val _ = op diff     : ''a set * ''a set  -> ''a set
(* sets of free type variables ((ml/haskell)) 415b *)
fun freetyvars t =
  let fun f (TYVAR v,          ftvs) = insert (v, ftvs)
        | f (TYCON _,          ftvs) = ftvs
        | f (CONAPP (ty, tys), ftvs) = foldl f (f (ty, ftvs)) tys
  in  rev (f (t, emptyset))
  end  
(* type declarations for consistency checking *)
val _ = op freetyvars : ty -> name set
fun dom theta = map (fn (a, _) => a) theta
fun compose (theta2, theta1) =
  let val domain  = union (dom theta2, dom theta1)
      val replace = tysubst theta2 o varsubst theta1
  in  map (fn a => (a, replace a)) domain
  end
(* type declarations for consistency checking *)
val _ = op dom     : subst -> name set
val _ = op compose : subst * subst -> subst
(* Hindley-Milner types ((ml/haskell)) 392a *)
exception BugInTypeInference of string

fun instantiate (FORALL (formals, tau), actuals) =
  tysubst (bindList (formals, actuals, emptyEnv)) tau
  handle BindListLength => raise BugInTypeInference
                                              "number of types in instantiation"
(* type declarations for consistency checking *)
val _ = op instantiate : type_scheme * ty list -> ty
(* Hindley-Milner types ((ml/haskell)) 392b *)
val idsubst = emptyEnv
(* Hindley-Milner types ((ml/haskell)) 392c *)
infix 7 |-->
val idsubst = emptyEnv
fun a |--> (TYVAR a') = if a = a' then idsubst else bind (a, TYVAR a', emptyEnv)
  | a |--> tau        = if member a (freetyvars tau) then
                          raise BugInTypeInference "non-idempotent substitution"
                        else
                          bind (a, tau, emptyEnv)
(* type declarations for consistency checking *)
val _ = op idsubst : subst
(* type declarations for consistency checking *)
val _ = op |--> : name * ty -> subst
(* Hindley-Milner types ((ml/haskell)) 393a *)
(* printing types ((ml/haskell)) 815 *)
(* definitions of [[separate]] and [[spaceSep]] 311a *)
fun separate (zero, sep) =  (* print list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^ sep ^ s t
  in  s
end
val spaceSep = separate ("", " ")  (* print separated by spaces *)
local
  (* precedences *)
  val CONp   = 3
  val STARp  = 2
  val ARROWp = 1
  val NONEp  = 0
  
  fun parens s = "(" ^ s ^ ")"
  fun bracket (s, context, prec) = if prec <= context then parens s else s
  fun p (context, CONAPP (TYCON "function", [arg, ret])) = 
          bracket (p (ARROWp, arg) ^ " -> " ^ p (ARROWp, ret), context, ARROWp)
    | p (context, CONAPP (TYCON "argtuple", ts)) = bracket (ptuple ts, context,
                                                                          STARp)
    | p (context, CONAPP (TYCON "tuple",    ts)) = bracket (ptuple ts, context,
                                                                          STARp)
    | p (context, CONAPP (n, []))  = p (context, n)
    | p (context, CONAPP (n, [t])) = p (CONp, t) ^ " " ^ p (CONp, n)
    | p (context, CONAPP (n, ts)) = 
          "(" ^ separate ("", ", ") (map typeString ts) ^ ") " ^ p (CONp, n)
    | p (context, TYCON n) = n
    | p (context, TYVAR v) = v
  and ptuple ts = separate ("unit", " * ") (map (fn t => p (STARp, t)) ts)
  and typeString ty = p (NONEp, ty)
in 
  val typeString = typeString
end
fun typeSchemeString (FORALL ([], ty)) =
      typeString ty
  | typeSchemeString (FORALL (a::a's, ty)) =
      let fun commaFollow (var, vars) = vars ^ ", " ^ var
      in  "forall " ^ foldl commaFollow a a's ^ " . " ^ typeString ty
      end
(* type declarations for consistency checking *)
val _ = op typeString       : ty          -> string
val _ = op typeSchemeString : type_scheme -> string
(* Hindley-Milner types ((ml/haskell)) 393b *)
fun eqType (TYCON c, TYCON c') = c = c'
  | eqType (CONAPP (tau, taus), CONAPP (tau', taus')) =
      eqType (tau, tau') andalso eqTypes (taus, taus')
  | eqType (TYVAR a, TYVAR a') = a = a'
  | eqType _ = false
and eqTypes (t::taus, t'::taus') = eqType (t, t') andalso eqTypes (taus, taus')
  | eqTypes ([], []) = true
  | eqTypes _ = false
(* Hindley-Milner types ((ml/haskell)) 393c *)
val inttype  = TYCON "int"
val booltype = TYCON "bool"
val symtype  = TYCON "sym"
val alpha    = TYVAR "a"
val beta     = TYVAR "b"
fun tupletype taus  = CONAPP (TYCON "tuple", taus)
fun pairtype (x, y) = tupletype [x, y]
val unittype        = tupletype []
fun listtype ty     = CONAPP (TYCON "list", [ty])
fun funtype (args, result) = 
  CONAPP (TYCON "function", [CONAPP (TYCON "argtuple", args), result])
(* type declarations for consistency checking *)
val _ = op eqType : ty * ty -> bool
(* type declarations for consistency checking *)
val _ = op inttype   : ty
val _ = op booltype  : ty
val _ = op symtype   : ty
val _ = op unittype  : ty
val _ = op listtype  : ty -> ty
val _ = op tupletype : ty list -> ty
val _ = op pairtype  : ty * ty -> ty
val _ = op funtype   : ty list * ty -> ty
val _ = op alpha     : ty
val _ = op beta      : ty
(* Hindley-Milner types ((ml/haskell)) 416a *)
local
  val n = ref 1
in
  fun freshtyvar _ = TYVAR ("'t" ^ Int.toString (!n) before n := !n + 1)
(* type declarations for consistency checking *)
val _ = op freshtyvar : 'a -> ty
end
(* Hindley-Milner types ((ml/haskell)) 416b *)
fun canonicalize (FORALL (bound, ty)) =
  let fun canonicalTyvarName n =
        if n < 26 then "'" ^ str (chr (ord #"a" + n))
        else "'v" ^ Int.toString (n - 25)
      val free = diff (freetyvars ty, bound)
      fun unusedIndex n =
        if member (canonicalTyvarName n) free then unusedIndex (n+1) else n
      fun newBoundVars (index, [])                = []
        | newBoundVars (index, oldvar :: oldvars) =
            let val n = unusedIndex index
            in  canonicalTyvarName n :: newBoundVars (n+1, oldvars)
            end
      val newBound = newBoundVars (0, bound)
(* type declarations for consistency checking *)
val _ = op canonicalize : type_scheme -> type_scheme
val _ = op newBoundVars : int * name list -> name list
  in  FORALL (newBound, tysubst (bindList (bound, map TYVAR newBound, emptyEnv))
                                                                             ty)
  end
(* Hindley-Milner types ((ml/haskell)) 416c *)
fun generalize (tau, tyvars) =
  canonicalize (FORALL (diff (freetyvars tau, tyvars), tau))
(* type declarations for consistency checking *)
val _ = op generalize : ty * name set -> type_scheme
(* Hindley-Milner types ((ml/haskell)) 417a *)
fun freshInstance (FORALL (bound, tau)) =
  instantiate (FORALL (bound, tau), map freshtyvar bound)
(* type declarations for consistency checking *)
val _ = op freshInstance : type_scheme -> ty
(* Hindley-Milner types ((ml/haskell)) 417b *)
datatype con = ~  of ty  * ty
             | /\ of con * con
             | TRIVIAL
infix 4 ~
infix 3 /\
(* Hindley-Milner types ((ml/haskell)) 417c *)
(* printing constraints ((ml/haskell)) 816a *)
fun untriviate (c /\ c') = (case (untriviate c, untriviate c')
                              of (TRIVIAL, c) => c
                               | (c, TRIVIAL) => c
                               | (c, c') => c /\ c')
  | untriviate atomic = atomic

fun constraintString (c /\ c') = constraintString c ^ " /\\ " ^ constraintString
                                                                              c'
  | constraintString (t ~  t') = typeString t ^ " ~ " ^ typeString t'
  | constraintString TRIVIAL = "TRIVIAL"
(* type declarations for consistency checking *)
val _ = op constraintString : con -> string
(* Hindley-Milner types ((ml/haskell)) 417d *)
fun freetyvarsConstraint (t ~  t') = union (freetyvars t, freetyvars t')
  | freetyvarsConstraint (c /\ c') = union (freetyvarsConstraint c,
                                             freetyvarsConstraint c')
  | freetyvarsConstraint TRIVIAL    = emptyset
(* Hindley-Milner types ((ml/haskell)) 417e *)
fun consubst theta =
  let fun subst (tau1 ~ tau2) = tysubst theta tau1 ~ tysubst theta tau2
        | subst (c1 /\ c2)      = subst c1 /\ subst c2
        | subst TRIVIAL         = TRIVIAL
  in  subst
  end
(* type declarations for consistency checking *)
val _ = op consubst : subst -> con -> con
(* Hindley-Milner types ((ml/haskell)) 418a *)
fun conjoinConstraints []      = TRIVIAL
  | conjoinConstraints [c]     = c
  | conjoinConstraints (c::cs) = c /\ conjoinConstraints cs
(* type declarations for consistency checking *)
val _ = op conjoinConstraints : con list -> con
(* Hindley-Milner types ((ml/haskell)) 418b *)
exception TypeError of string

fun unsatisfiableEquality (t1, t2) =
  case canonicalize (FORALL (union (freetyvars t1, freetyvars t2), tupletype [t1
                                                                         , t2]))
    of FORALL (_, CONAPP (TYCON "tuple", [t1, t2])) => 
         raise TypeError ("cannot make " ^ typeString t1 ^ " equal to " ^
                                                                  typeString t2)
     | _ => let exception ThisCan'tHappen in raise ThisCan'tHappen end
(* Hindley-Milner types ((ml/haskell)) 418d *)
(* constraint solving ((prototype)) 418c *)
exception LeftAsExercise of string
fun solve c = raise LeftAsExercise "solve"
(* type declarations for consistency checking *)
val _ = op solve : con -> subst
(* constraint solving (BUG: software can't tell where this code came from) *)
(*asdf*)
(* Hindley-Milner types ((ml/haskell)) 419a *)
type type_env = type_scheme env * name set
(* Hindley-Milner types ((ml/haskell)) 419b *)
val emptyTypeEnv = 
      (emptyEnv, emptyset)
fun findtyscheme (v, (Gamma, free)) = find (v, Gamma)
(* type declarations for consistency checking *)
val _ = op emptyTypeEnv : type_env
val _ = op findtyscheme : name * type_env -> type_scheme
(* Hindley-Milner types ((ml/haskell)) 419c *)
fun bindtyscheme (v, sigma as FORALL (bound, tau), (Gamma, free)) = 
  (bind (v, sigma, Gamma), union (diff (freetyvars tau, bound), free))
(* Hindley-Milner types ((ml/haskell)) 419d *)
fun freetyvarsGamma (_, free) = free
(* Hindley-Milner types ((haskell)) (BUG: software can't tell where this code came from) *)
val valtype = TYCON "value"
(* Hindley-Milner types ((haskell)) 946 *)
fun iotype ty  = CONAPP (TYCON "io", [ty])
(* Hindley-Milner types ((haskell)) (BUG: software can't tell where this code came from) *)
fun solve (TRIVIAL)  = idsubst
  | solve (c1 /\ c2) =
      let val theta1 = solve c1
          val theta2 = solve (consubst theta1 c2)
      in  compose (theta2, theta1)
      end
  | solve (tau ~ tau') =
      case (tau, tau')
        of (TYVAR a, tau) => solveTyvarEq (a, tau)
         | (tau, TYVAR a) => solveTyvarEq (a, tau)
         | (TYCON mu, TYCON mu') => if mu = mu' then idsubst
                                    else unsatisfiableEquality (tau, tau')
         | (CONAPP (t, ts), CONAPP (t', ts')) =>
             let fun eqAnd (t, t', c) = t ~ t' /\ c
             in  solve (ListPair.foldlEq eqAnd TRIVIAL (t::ts, t'::ts'))
                 handle ListPair.UnequalLengths => unsatisfiableEquality (tau,
                                                                           tau')
             end
         | _ => unsatisfiableEquality (tau, tau')

and solveTyvarEq (a, tau) = 
      if eqType (TYVAR a, tau) then
        idsubst
      else if member a (freetyvars tau) then
        unsatisfiableEquality (TYVAR a, tau)
      else
        a |--> tau

fun isStandard pairs =
  let fun distinct a' (a, tau) = a <> a' andalso not (member a' (freetyvars tau)
                                                                               )
      fun good (prev', (a, tau)::next) =
            List.all (distinct a) prev' andalso List.all (distinct a) next
            andalso good ((a, tau)::prev', next)
        | good (_, []) = true
  in  good ([], pairs)
  end

val solve =
  fn c => let val theta = solve c
          in  if isStandard theta then theta
              else raise BugInTypeInference "non-standard substitution"
          end


(*****************************************************************)
(*                                                               *)
(*   LEXICAL ANALYSIS                                            *)
(*                                                               *)
(*****************************************************************)

(* lexical analysis ((mlscheme)) 787a *)
datatype token = NAME    of string
               | INT     of int
               | SHARP   of bool
               | BRACKET of char (* ( or ) *)
               | QUOTE
(* lexical analysis ((mlscheme)) 787b *)
fun tokenString (NAME x)    = x
  | tokenString (INT  n)    = Int.toString n
  | tokenString (SHARP b)   = if b then "#t" else "#f"
  | tokenString (BRACKET c) = str c
  | tokenString (QUOTE)     = "'"

fun isLiteral s t = tokenString t = s
(* support for streams, lexical analysis, and parsing 760 *)
(* suspensions 762 *)
datatype 'a action
  = PENDING  of unit -> 'a
  | PRODUCED of 'a

type 'a susp = 'a action ref

fun delay f = ref (PENDING f)
fun force cell =
  case !cell
    of PENDING f =>  let val result = f ()
                     in  (cell := PRODUCED result; result)
                     end
     | PRODUCED v => v
(* type declarations for consistency checking *)
val _ = op delay : (unit -> 'a) -> 'a susp
val _ = op force : 'a susp -> 'a
(* streams 763a *)
datatype 'a stream 
  = EOS
  | :::       of 'a * 'a stream
  | SUSPENDED of 'a stream susp
infixr 3 :::
(* streams 763b *)
fun streamGet EOS = NONE
  | streamGet (x ::: xs)    = SOME (x, xs)
  | streamGet (SUSPENDED s) = streamGet (force s)
(* streams 763c *)
fun streamOfList xs = 
  foldr (op :::) EOS xs
(* type declarations for consistency checking *)
val _ = op streamGet : 'a stream -> ('a * 'a stream) option
(* type declarations for consistency checking *)
val _ = op streamOfList : 'a list -> 'a stream
(* streams 763d *)
fun listOfStream xs =
  case streamGet xs
    of NONE => []
     | SOME (x, xs) => x :: listOfStream xs
(* streams 763e *)
fun delayedStream action = 
  SUSPENDED (delay action)
(* type declarations for consistency checking *)
val _ = op listOfStream : 'a stream -> 'a list
(* type declarations for consistency checking *)
val _ = op delayedStream : (unit -> 'a stream) -> 'a stream
(* streams 763f *)
fun streamOfEffects next =
  delayedStream (fn () => case next () of NONE => EOS
                                        | SOME a => a ::: streamOfEffects next)
(* type declarations for consistency checking *)
val _ = op streamOfEffects : (unit -> 'a option) -> 'a stream
(* streams 763g *)
type line = string
fun streamOfLines infile = 
  streamOfEffects (fn () => TextIO.inputLine infile)
(* type declarations for consistency checking *)
type line = line
val _ = op streamOfLines : TextIO.instream -> line stream
(* streams 764a *)
fun streamRepeat x =
  delayedStream (fn () => x ::: streamRepeat x)
(* type declarations for consistency checking *)
val _ = op streamRepeat : 'a -> 'a stream
(* streams 764b *)
fun streamOfUnfold next state =
  delayedStream (fn () => case next state
                            of NONE => EOS
                             | SOME (a, state) => a ::: streamOfUnfold next
                                                                          state)
(* type declarations for consistency checking *)
val _ = op streamOfUnfold : ('b -> ('a * 'b) option) -> 'b -> 'a stream
(* streams 764c *)
fun preStream (pre, xs) = 
  streamOfUnfold (fn xs => (pre (); streamGet xs)) xs
(* streams 765a *)
fun postStream (xs, post) =
  streamOfUnfold (fn xs => case streamGet xs
                             of NONE => NONE
                              | head as SOME (x, _) => (post x; head)) xs
(* type declarations for consistency checking *)
val _ = op preStream : (unit -> unit) * 'a stream -> 'a stream
(* type declarations for consistency checking *)
val _ = op postStream : 'a stream * ('a -> unit) -> 'a stream
(* streams 765b *)
fun streamMap f xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => f x ::: streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamMap : ('a -> 'b) -> 'a stream -> 'b stream
(* streams 765c *)
fun streamFilter p xs =
  delayedStream (fn () => case streamGet xs
                            of NONE => EOS
                             | SOME (x, xs) => if p x then x ::: streamFilter p
                                                                              xs
                                               else streamFilter p xs)
(* type declarations for consistency checking *)
val _ = op streamFilter : ('a -> bool) -> 'a stream -> 'a stream
(* streams 765d *)
fun streamFold f z xs =
  case streamGet xs of NONE => z
                     | SOME (x, xs) => streamFold f (f (x, z)) xs
(* type declarations for consistency checking *)
val _ = op streamFold : ('a * 'b -> 'b) -> 'b -> 'a stream -> 'b

(* streams 765e *)
fun streamZip (xs, ys) =
  delayedStream
  (fn () => case (streamGet xs, streamGet ys)
              of (SOME (x, xs), SOME (y, ys)) => (x, y) ::: streamZip (xs, ys)
               | _ => EOS)
(* streams 766a *)
fun streamConcat xss =
  let fun get (xs, xss) =
        case streamGet xs
          of SOME (x, xs) => SOME (x, (xs, xss))
           | NONE => case streamGet xss
                       of SOME (xs, xss) => get (xs, xss)
                        | NONE => NONE
  in  streamOfUnfold get (EOS, xss)
  end
(* type declarations for consistency checking *)
val _ = op streamZip : 'a stream * 'b stream -> ('a * 'b) stream
(* type declarations for consistency checking *)
val _ = op streamConcat : 'a stream stream -> 'a stream
(* streams 766b *)
fun streamConcatMap f xs = streamConcat (streamMap f xs)
(* type declarations for consistency checking *)
val _ = op streamConcatMap : ('a -> 'b stream) -> 'a stream -> 'b stream
(* streams 766c *)
infix 5 @@@
fun xs @@@ xs' = streamConcat (streamOfList [xs, xs'])
(* type declarations for consistency checking *)
val _ = op @@@ : 'a stream * 'a stream -> 'a stream
(* error handling 767a *)
datatype 'a error = OK of 'a | ERROR of string
(* error handling 767b *)
infix 1 >>=
fun (OK x)      >>= k  =  k x
  | (ERROR msg) >>= k  =  ERROR msg
(* type declarations for consistency checking *)
val _ = op >>= : 'a error * ('a -> 'b error) -> 'b error
(* error handling 768a *)
infix 1 >>=+
fun e >>=+ k'  =  e >>= OK o k'
(* type declarations for consistency checking *)
val _ = op >>=+ : 'a error * ('a -> 'b) -> 'b error
(* error handling 768b *)
fun errorList es =
  let fun cons (OK x, OK xs) = OK (x :: xs)
        | cons (ERROR m1, ERROR m2) = ERROR (m1 ^ "; " ^ m2)
        | cons (ERROR m, OK _) = ERROR m
        | cons (OK _, ERROR m) = ERROR m
  in  foldr cons (OK []) es
  end
(* type declarations for consistency checking *)
val _ = op errorList : 'a error list -> 'a list error
(* parsing combinators 768c *)
type ('a, 'b) xformer = 
  'a stream -> ('b error * 'a stream) option
(* type declarations for consistency checking *)
type ('a, 'b) xformer = ('a, 'b) xformer
(* parsing combinators 769a *)
fun pure y = fn xs => SOME (OK y, xs)
(* type declarations for consistency checking *)
val _ = op pure : 'b -> ('a, 'b) xformer
(* parsing combinators 769b *)
infix 3 <*>
fun tx_f <*> tx_b =
  fn xs => case tx_f xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK f, xs) =>
                  case tx_b xs
                    of NONE => NONE
                     | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
                     | SOME (OK y, xs) => SOME (OK (f y), xs)
(* type declarations for consistency checking *)
val _ = op <*> : ('a, 'b -> 'c) xformer * ('a, 'b) xformer -> ('a, 'c) xformer
(* parsing combinators 769c *)
infixr 4 <$>
fun f <$> p = pure f <*> p
(* type declarations for consistency checking *)
val _ = op <$> : ('b -> 'c) * ('a, 'b) xformer -> ('a, 'c) xformer
(* parsing combinators 770a *)
fun id x = x
fun fst (x, y) = x
fun snd (x, y) = y
fun pair x y = (x, y)
fun curry  f x y   = f (x, y)
fun curry3 f x y z = f (x, y, z)
(* type declarations for consistency checking *)
val _ = op fst    : ('a * 'b) -> 'a
val _ = op snd    : ('a * 'b) -> 'b
val _ = op pair   : 'a -> 'b -> 'a * 'b
val _ = op curry  : ('a * 'b -> 'c) -> ('a -> 'b -> 'c)
val _ = op curry3 : ('a * 'b * 'c -> 'd) -> ('a -> 'b -> 'c -> 'd)
(* parsing combinators 770b *)
infix 1 <|>
fun t1 <|> t2 = (fn xs => case t1 xs of SOME y => SOME y | NONE => t2 xs) 
(* type declarations for consistency checking *)
val _ = op <|> : ('a, 'b) xformer * ('a, 'b) xformer -> ('a, 'b) xformer
(* parsing combinators 771a *)
infix 3 <* *>
fun p1 <*  p2 = curry fst <$> p1 <*> p2
fun p1  *> p2 = curry snd <$> p1 <*> p2

infixr 4 <$
fun v <$ p = (fn _ => v) <$> p
(* type declarations for consistency checking *)
val _ = op <*  : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'b) xformer
val _ = op  *> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
val _ = op <$  : 'b               * ('a, 'c) xformer -> ('a, 'b) xformer
(* parsing combinators 771b *)
fun one xs = case streamGet xs
               of NONE => NONE
                | SOME (x, xs) => SOME (OK x, xs)
(* type declarations for consistency checking *)
val _ = op one : ('a, 'a) xformer
(* parsing combinators 771c *)
fun eos xs = case streamGet xs
               of NONE => SOME (OK (), EOS)
                | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op eos : ('a, unit) xformer
(* parsing combinators 771d *)
fun peek tx xs =
  case tx xs of SOME (OK y, _) => SOME y
              | _ => NONE
(* type declarations for consistency checking *)
val _ = op peek : ('a, 'b) xformer -> 'a stream -> 'b option
(* parsing combinators 772a *)
fun rewind tx xs =
  case tx xs of SOME (ey, _) => SOME (ey, xs)
              | NONE => NONE
(* type declarations for consistency checking *)
val _ = op rewind : ('a, 'b) xformer -> ('a, 'b) xformer
(* parsing combinators 772b *)
fun sat p tx xs =
  case tx xs
    of answer as SOME (OK y, xs) => if p y then answer else NONE
     | answer => answer
(* type declarations for consistency checking *)
val _ = op sat : ('b -> bool) -> ('a, 'b) xformer -> ('a, 'b) xformer
(* parsing combinators 772c *)
fun oneEq x = sat (fn x' => x = x') one
(* type declarations for consistency checking *)
val _ = op oneEq : ''a -> (''a, ''a) xformer
(* parsing combinators 773a *)
infixr 4 <$>?
fun f <$>? tx =
  fn xs => case tx xs
             of NONE => NONE
              | SOME (ERROR msg, xs) => SOME (ERROR msg, xs)
              | SOME (OK y, xs) =>
                  case f y
                    of NONE => NONE
                     | SOME z => SOME (OK z, xs)
(* type declarations for consistency checking *)
val _ = op <$>? : ('b -> 'c option) * ('a, 'b) xformer -> ('a, 'c) xformer
(* parsing combinators 773b *)
infix 3 <&>
fun t1 <&> t2 = fn xs =>
  case t1 xs
    of SOME (OK _, _) => t2 xs
     | SOME (ERROR _, _) => NONE    
     | NONE => NONE
(* type declarations for consistency checking *)
val _ = op <&> : ('a, 'b) xformer * ('a, 'c) xformer -> ('a, 'c) xformer
(* parsing combinators 773c *)
fun notFollowedBy t xs =
  case t xs
    of NONE => SOME (OK (), xs)
     | SOME _ => NONE
(* type declarations for consistency checking *)
val _ = op notFollowedBy : ('a, 'b) xformer -> ('a, unit) xformer
(* parsing combinators 773d *)
fun many t = 
  curry (op ::) <$> t <*> (fn xs => many t xs) <|> pure []
(* type declarations for consistency checking *)
val _ = op many  : ('a, 'b) xformer -> ('a, 'b list) xformer
(* parsing combinators 774a *)
fun many1 t = 
  curry (op ::) <$> t <*> many t
(* type declarations for consistency checking *)
val _ = op many1 : ('a, 'b) xformer -> ('a, 'b list) xformer
(* parsing combinators 774b *)
fun optional t = 
  SOME <$> t <|> pure NONE
(* type declarations for consistency checking *)
val _ = op optional : ('a, 'b) xformer -> ('a, 'b option) xformer
(* parsing combinators 774c *)
infix 2 <*>!
fun tx_ef <*>! tx_x =
  fn xs => case (tx_ef <*> tx_x) xs
             of NONE => NONE
              | SOME (OK (OK y),      xs) => SOME (OK y,      xs)
              | SOME (OK (ERROR msg), xs) => SOME (ERROR msg, xs)
              | SOME (ERROR msg,      xs) => SOME (ERROR msg, xs)
infixr 4 <$>!
fun ef <$>! tx_x = pure ef <*>! tx_x
(* type declarations for consistency checking *)
val _ = op <*>! : ('a, 'b -> 'c error) xformer * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
val _ = op <$>! : ('b -> 'c error)             * ('a, 'b) xformer -> ('a, 'c)
                                                                         xformer
(* support for lexical analysis 775a *)
type 'a lexer = (char, 'a) xformer
(* type declarations for consistency checking *)
type 'a lexer = 'a lexer
(* support for lexical analysis 775b *)
fun isDelim c =
  Char.isSpace c orelse Char.contains "();" c
(* type declarations for consistency checking *)
val _ = op isDelim : char -> bool
(* support for lexical analysis 775c *)
val whitespace = many (sat Char.isSpace one)
(* type declarations for consistency checking *)
val _ = op whitespace : char list lexer
(* support for lexical analysis 776a *)
fun intChars isDelim = 
  (curry (op ::) <$> oneEq #"-" <|> pure id) <*> many1 (sat Char.isDigit one) <*
                                                                                
  notFollowedBy (sat (not o isDelim) one)
(* type declarations for consistency checking *)
val _ = op intChars     : (char -> bool) -> char list lexer
(* support for lexical analysis 776b *)
fun intFromChars (#"-" :: cs) = 
      intFromChars cs >>=+ Int.~
  | intFromChars cs =
      (OK o valOf o Int.fromString o implode) cs
      handle Overflow => ERROR
                        "this interpreter can't read arbitrarily large integers"
(* type declarations for consistency checking *)
val _ = op intFromChars : char list -> int error
(* support for lexical analysis 776c *)
fun intToken isDelim =
  intFromChars <$>! intChars isDelim
(* type declarations for consistency checking *)
val _ = op intToken : (char -> bool) -> int lexer
(* support for parsing 776d *)
(* token, isLiteral, and
   tokenString can be defined
   differently in each language *)
(* support for parsing 777a *)
type srcloc = string * int
fun srclocString (source, line) =
  source ^ ", line " ^ Int.toString line
(* support for parsing 777b *)
fun errorAt msg loc =
  ERROR (msg ^ " in " ^ srclocString loc)
(* support for parsing 777c *)
type 'a located = srcloc * 'a
(* type declarations for consistency checking *)
type token = token
val _ = op isLiteral : string -> token -> bool
val _ = op tokenString : token -> string
(* type declarations for consistency checking *)
type srcloc = srcloc
val _ = op srclocString : srcloc -> string
(* type declarations for consistency checking *)
val _ = op errorAt : string -> srcloc -> 'a error
(* type declarations for consistency checking *)
type 'a located = 'a located
(* support for parsing 777d *)
fun locatedStream (streamname, inputs) =
  let val locations = streamZip (streamRepeat streamname,
                                 streamOfUnfold (fn n => SOME (n, n+1)) 1)
  in  streamZip (locations, inputs)
  end
(* type declarations for consistency checking *)
val _ = op locatedStream : string * line stream -> line located stream
(* support for parsing 778a *)
datatype 'a inline
  = EOL of int (* number of the line that ends here *)
  | INLINE of 'a

fun drainLine EOS = EOS
  | drainLine (SUSPENDED s)     = drainLine (force s)
  | drainLine (EOL _    ::: xs) = xs
  | drainLine (INLINE _ ::: xs) = drainLine xs
(* parsing utilities 778b *)
type 'a parser = (token located inline, 'a) xformer
(* parsing utilities 779a *)
local 
  fun asEol (EOL n) = SOME n
    | asEol (INLINE _) = NONE
  fun asInline (INLINE x) = SOME x
    | asInline (EOL _)    = NONE
in
  fun eol    xs = (asEol <$>? one) xs
  fun inline xs = (many eol *> asInline <$>? one) xs
end

val token    =         snd <$> inline  : token parser
val srcloc   = rewind (fst <$> inline) : srcloc parser
val noTokens = notFollowedBy token : unit parser
(* type declarations for consistency checking *)
type 'a inline = 'a inline
val _ = op drainLine : 'a inline stream -> 'a inline stream
(* type declarations for consistency checking *)
type 'a parser = 'a parser
(* type declarations for consistency checking *)
val _ = op eol      : ('a inline, int) xformer
val _ = op inline   : ('a inline, 'a)  xformer
val _ = op token    : token parser
val _ = op srcloc   : srcloc parser
val _ = op noTokens : unit parser
(* parsing utilities 779b *)
fun @@ p = pair <$> srcloc <*> p
(* type declarations for consistency checking *)
val _ = op @@ : 'a parser -> 'a located parser
(* parsing utilities 779c *)

infix 0 <?>
fun p <?> expected = p <|> errorAt ("expected " ^ expected) <$>! srcloc
(* type declarations for consistency checking *)
val _ = op <?> : 'a parser * string -> 'a parser
(* parsing utilities 780a *)
infix 4 <!>
fun p <!> msg =
  fn tokens => (case p tokens
                  of SOME (OK _, unused) =>
                       (case peek srcloc tokens
                          of SOME loc => SOME (errorAt msg loc, unused)
                           | NONE => NONE)
                   | _ => NONE)
(* parsing utilities 780b *)
fun literal s =
  ignore <$> sat (isLiteral s) token
(* type declarations for consistency checking *)
val _ = op <!> : 'a parser * string -> 'b parser
(* type declarations for consistency checking *)
val _ = op literal : string -> unit parser
(* parsing utilities 780c *)
infix  6 --<
infixr 7 >-- 
    (* if we want to mix these operators, they can't have equal precedence *)
fun (a >-- p) = literal a *> p
fun (p --< a) = p <* literal a
(* type declarations for consistency checking *)
val _ = op >-- : string    * 'a parser -> 'a parser
val _ = op --< : 'a parser * string    -> 'a parser
(* parsing utilities 781 *)

fun bracket keyword expected p = 
  "(" >-- literal keyword *> (p --< ")" <|>
                              errorAt ("expected " ^ expected) <$>!
                                                               scanToCloseParen)
and scanToCloseParen tokens = 
  let val loc = getOpt (peek srcloc tokens, ("end of stream", 9999))
      fun scan lpcount tokens =
        (* lpcount is the number of unmatched left parentheses *)
        case tokens
          of EOL _         ::: tokens => scan lpcount tokens
           | INLINE (_, t) ::: tokens =>
                                  if isLiteral "(" t then scan (lpcount+1)
                                                                          tokens
                                  else if isLiteral ")" t then
                                      if lpcount = 0 then SOME (OK loc, tokens)
                                      else scan (lpcount-1) tokens
                                  else scan lpcount tokens
           | EOS         => SOME (errorAt "unmatched (" loc, EOS)
           | SUSPENDED s => scan lpcount (force s)
  in  scan 0 tokens
  end
(* type declarations for consistency checking *)
val _ = op bracket          : string -> string -> 'a parser -> 'a parser
val _ = op scanToCloseParen : srcloc parser
(* parsing utilities 782a *)
fun nodups (what, where') (loc, names) =
  let fun dup [] = OK names
        | dup (x::xs) = if List.exists (fn y : string => y = x) xs then
                          errorAt (what ^ " " ^ x ^ " appears twice in " ^
                                                                     where') loc
                        else
                          dup xs
  in  dup names
  end
(* type declarations for consistency checking *)
val _ = op nodups : string * string -> srcloc * name list -> name list error
(* code used to debug parsers 782b *)
val safeTokens : token located inline stream -> token list =
  let fun tokens (seenEol, seenUnforced) =
            let fun get (EOL _         ::: ts) = if seenUnforced then []
                                                 else tokens (true, false) ts
                  | get (INLINE (_, t) ::: ts) = t :: get ts
                  | get  EOS                   = []
                  | get (SUSPENDED (ref (PRODUCED ts))) = get ts
                  | get (SUSPENDED s) = if seenEol then []
                                        else tokens (false, true) (force s)
            in   get
            end
  in  tokens (false, false)
  end
(* type declarations for consistency checking *)
val _ = op safeTokens : token located inline stream -> token list
(* code used to debug parsers 783a *)
fun wrap what p tokens =
  let fun t tok = " " ^ tokenString tok
      val _ = app print ["Looking for ", what, " at"]
      val _ = app (print o t) (safeTokens tokens)
      val _ = print "\n"
      val answer = p tokens
      val _ = app print [case answer of NONE => "Didn't find " | SOME _ =>
                                                                       "Found ",
                         what, "\n"]
  in  answer
  end handle e => ( app print ["Search for ", what, " raised ", exnName e, "\n"]
                  ; raise e)
(* type declarations for consistency checking *)
val _ = op wrap : string -> 'a parser -> 'a parser
(* an interactive reader 783b *)
fun echoTagStream lines = 
  let fun echoIfTagged line =
        if (String.substring (line, 0, 2) = ";#" handle _ => false) then
          print line
        else
          ()
  in  postStream (lines, echoIfTagged)
  end
(* type declarations for consistency checking *)
val _ = op echoTagStream : line stream -> line stream 
(* an interactive reader 783c *)
fun errorln s = TextIO.output (TextIO.stdErr, s ^ "\n")
(* type declarations for consistency checking *)
val _ = op errorln : string -> unit
(* an interactive reader 784a *)
fun stripErrors xs =
  let fun next xs =
        case streamGet xs
          of SOME (ERROR msg, xs) => (errorln ("error: " ^ msg); next xs)
           | SOME (OK x, xs) => SOME (x, xs)
           | NONE => NONE
  in  streamOfUnfold next xs
  end
(* type declarations for consistency checking *)
val _ = op stripErrors : 'a error stream -> 'a stream
(* an interactive reader 784b *)
fun lexLineWith lexer =
  stripErrors o streamOfUnfold lexer o streamOfList o explode
(* type declarations for consistency checking *)
val _ = op lexLineWith : token lexer -> line -> token stream
(* an interactive reader 784c *)
fun parseWithErrors parser =
  let fun adjust (SOME (ERROR msg, tokens)) = SOME (ERROR msg, drainLine tokens)
        | adjust other = other
  in  streamOfUnfold (adjust o parser)
  end
(* type declarations for consistency checking *)
val _ = op parseWithErrors : 'a parser -> token located inline stream -> 'a
                                                                    error stream
(* an interactive reader 784d *)
type prompts   = { ps1 : string, ps2 : string }
val stdPrompts = { ps1 = "-> ", ps2 = "   " }
val noPrompts  = { ps1 = "", ps2 = "" }
(* type declarations for consistency checking *)
type prompts = prompts
val _ = op stdPrompts : prompts
val _ = op noPrompts  : prompts
(* an interactive reader 785 *)
fun 'a reader (lexer, parser) prompts (name, lines) =
  let val { ps1, ps2 } = prompts
      val thePrompt = ref ps1
      fun setPrompt ps = fn _ => thePrompt := ps

      val lines = preStream (fn () => print (!thePrompt), echoTagStream lines)

      fun lexAndDecorate (loc, line) =
        let val tokens = postStream (lexLineWith lexer line, setPrompt ps2)
        in  streamMap INLINE (streamZip (streamRepeat loc, tokens)) @@@
            streamOfList [EOL (snd loc)]
        end

      val edefs : 'a error stream = 
        (parseWithErrors parser o streamConcatMap lexAndDecorate o locatedStream
                                                                               )
        (name, lines)
(* type declarations for consistency checking *)
val _ = op reader : token lexer * 'a parser -> prompts -> string * line stream
                                                                    -> 'a stream
val _ = op lexAndDecorate : srcloc * line -> token located inline stream
  in  
      stripErrors (preStream (setPrompt ps1, edefs))
  end 
(* lexical analysis ((mlscheme)) 788a *)
local
  (* functions used in the lexer for \uscheme 788b *)
  fun atom "#t" = SHARP true
    | atom "#f" = SHARP false
    | atom x    = NAME x
  (* functions used in the lexer for \uscheme 788c *)
  fun noneIfLineEnds chars =
    case streamGet chars
      of NONE => NONE (* end of line *)
       | SOME (#";", cs) => NONE (* comment *)
       | SOME (c, cs) => 
           let val msg = "invalid initial character in `" ^
                         implode (c::listOfStream cs) ^ "'"
           in  SOME (ERROR msg, EOS)
           end
  (* type declarations for consistency checking *)
  val _ = op noneIfLineEnds : 'a lexer
in
  val schemeToken =
    whitespace *> (   BRACKET <$> oneEq #"("
                  <|> BRACKET <$> oneEq #")"
                  <|> QUOTE   <$  oneEq #"'"
                  <|> INT     <$> intToken isDelim
                  <|> (atom o implode) <$> many1 (sat (not o isDelim) one)
                  <|> noneIfLineEnds
                  )
(* type declarations for consistency checking *)
val _ = op schemeToken : token lexer
val _ = op atom : string -> token
end


(*****************************************************************)
(*                                                               *)
(*   ABSTRACT SYNTAX AND VALUES                                  *)
(*                                                               *)
(*****************************************************************)

(* abstract syntax and values ((haskell)) (BUG: software can't tell where this code came from) *)
datatype thunk_contents 
  = VALUE       of value
  | IN_PROGRESS
  | UNEVAL       of thunk
and value (* a value in weak head normal form *)
  = VCON of name * loc list   (* fully saturated constructor app *)
  | SYM  of name
  | NUM  of int
  | CLOSURE   of lambda * loc env
  | PRIMITIVE of primitive
  | IO        of unit -> loc  (* monadic action *)
and exp 
  = LITERAL of value
  | VAR     of name
  | APPLY   of exp * exp list
  | LETX    of let_kind * (name * exp) list * exp
  | DO      of            (name * exp) list * exp
  | LAMBDA  of lambda
  | CASE    of exp * match list
  | IFX     of exp * exp * exp
and let_kind = LET | LETREC | LETSTAR
 withtype loc       = thunk_contents ref
      and lambda    = name list * exp
      and thunk     = exp * thunk_contents ref env
      and match     = (name * name list) * exp
      and primitive = thunk_contents ref list -> value
exception RuntimeError of string (* error message *)
exception Unimp of string
(* abstract syntax and values ((haskell)) (BUG: software can't tell where this code came from) *)
datatype def  = VAL    of name * exp
              | VALREC of name * exp
              | EXP    of exp
              | DEFINE of name * lambda
              | CONDEF of name * type_scheme
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test
and unit_test = BOGUS


(*****************************************************************)
(*                                                               *)
(*   VALUES                                                      *)
(*                                                               *)
(*****************************************************************)

(* values ((haskell)) (BUG: software can't tell where this code came from) *)
fun allocThunk (e, rho) = ref (UNEVAL (e, rho))
fun allocValue v        = ref (VALUE v)
(* type declarations for consistency checking *)
val _ = op allocThunk : exp * loc env -> loc
val _ = op allocValue : value         -> loc
(* values ((haskell)) (BUG: software can't tell where this code came from) *)
(* definitions of [[separate]] and [[spaceSep]] 311a *)
fun separate (zero, sep) =  (* print list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^ sep ^ s t
  in  s
end
val spaceSep = separate ("", " ")  (* print separated by spaces *)
fun locString (ref IN_PROGRESS) = "_|_"
  | locString (ref (UNEVAL _))  = "..."
  | locString (ref (VALUE v))  = valueString v
and valueString (VCON ("cons", [l, ls])) =
(* convert [[(cons l ls)]] to string (BUG: software can't tell where this code came from) *)
                                           let fun tail (VCON ("cons", [l, ls]))
                                                = " " ^ locString l ^ tailLoc ls
                                                 | tail (VCON ("()", []))
                                                                           = ")"
                                                 | tail v
                                                   = " . " ^ valueString v ^ ")"
                                               and tailLoc (ref (VALUE v))   =
                                                                          tail v
                                                 | tailLoc (ref IN_PROGRESS) =
                                                                         " _|_)"
                                                 | tailLoc (ref (UNEVAL _))  =
                                                                       " . ...)"
                                           in  "(" ^ locString l ^ tailLoc ls
                                           end
  | valueString (VCON (c, []))  = c
  | valueString (VCON (c, ls))  = "(" ^ c ^ " " ^ spaceSep (map locString ls) ^
                                                                             ")"
  | valueString (NUM n      )   = String.map (fn #"~" => #"-" | c => c) (
                                                                 Int.toString n)
  | valueString (SYM v      )   = v
  | valueString (CLOSURE   _)   = "<procedure>"
  | valueString (PRIMITIVE _)   = "<procedure>"
  | valueString (IO _       )   = "<I/O action>"
(* type declarations for consistency checking *)
val _ = op locString   : loc   -> string
val _ = op valueString : value -> string


(*****************************************************************)
(*                                                               *)
(*   PARSING                                                     *)
(*                                                               *)
(*****************************************************************)

(* parsing ((haskell)) (BUG: software can't tell where this code came from) *)
val lambdaDups   = nodups ("formal parameter", "lambda")
val conDups      = nodups ("argument of constructor", "case expression")

fun letDups LETSTAR (loc, bindings) = OK bindings
  | letDups kind    (loc, bindings) =
      let val names    = map (fn (n, _) => n) bindings
          val kindName = case kind of LET => "let" | LETREC => "letrec" | _ =>
                                                                            "??"
      in  nodups ("bound name", kindName) (loc, names) >>=+ (fn _ => bindings)
      end
(* parsing ((haskell)) (BUG: software can't tell where this code came from) *)
fun embedList []     = VCON ("()", [])
  | embedList (h::t) = VCON ("cons", [allocValue h, allocValue (embedList t)])
(* type declarations for consistency checking *)
val _ = op embedList : value list -> value
(* parsing ((haskell)) (BUG: software can't tell where this code came from) *)
val name    = (fn (NAME  n) => SOME n  | _ => NONE) <$>? token
val booltok = (fn (SHARP b) => SOME b  | _ => NONE) <$>? token
val int     = (fn (INT   n) => SOME n  | _ => NONE) <$>? token
val quote   = (fn (QUOTE)   => SOME () | _ => NONE) <$>? token

fun boolcon p = VCON (if p then "#t" else "#f", [])

fun exp tokens = (
     VAR              <$> name
 <|> LITERAL <$> NUM  <$> int
 <|> LITERAL <$> boolcon <$> booltok
 <|> LITERAL          <$> (quote *> sexp)
 <|> bracket "if"     "(if e1 e2 e3)"            (curry3 IFX <$> exp <*> exp <*>
                                                                            exp)
 <|> bracket "lambda" "(lambda (names) body)"    (     lambda  <$> @@ formals
                                                                       <*>! exp)
 <|> bracket "let"    "(let (bindings) body)"    (letx LET     <$> @@ bindings
                                                                       <*>! exp)
 <|> bracket "letrec" "(letrec (bindings) body)" (letx LETREC  <$> @@ bindings
                                                                       <*>! exp)
 <|> bracket "let*"   "(let* (bindings) body)"   (letx LETSTAR <$> @@ bindings
                                                                       <*>! exp)
 <|> bracket "case"   "(case exp matches)"       (curry CASE   <$> exp <*> many
                                                                          match)
 <|> "(" >-- literal ")" <!> "empty application"
 <|> curry APPLY <$> "(" >-- exp <*> many exp --< ")"
) tokens
and lambda xs exp =
  nodups ("formal parameter", "lambda") xs >>=+ (fn xs => LAMBDA (xs, exp))
and letx kind bs exp = letDups kind bs >>=+ (fn bs => LETX (kind, bs, exp))
and formals  ts = ("(" >-- many name --< ")") ts
and bindings ts = ("(" >-- (many binding --< ")" <?> "(x e)...")) ts
and binding  ts = ("(" >-- (pair <$> name <*> exp --< ")" <?>
                                                        "(x e) in bindings")) ts

and match ts = ("(" >-- (pair <$> matchlhs <*> exp --< ")" <?>
                                                        "(pat exp) in case")) ts
and matchlhs ts = 
   (  (fn n => (n, [])) <$> name
   <|> ("()", []) <$ "(" >-- literal ")"
   <|> pair <$> "(" >-- name <*> (conDups <$>! @@ (many name)) --< ")"
   ) ts
                
and sexp tokens = (
     SYM <$> (notDot <$>! name)
 <|> NUM          <$> int
 <|> boolcon      <$> booltok
 <|> (fn v => embedList [SYM "quote", v]) <$> (quote *> sexp)
 <|> embedList    <$> "(" >-- many sexp --< ")"
) tokens
and notDot "." = ERROR
                      "this interpreter cannot handle . in quoted S-expressions"
  | notDot s   = OK s
(* parsing ((haskell)) (BUG: software can't tell where this code came from) *)
fun unexpected msg (loc, _) = errorAt loc msg
val tyvar = curry op ^ "'" <$> (quote *> (name <?> "type variable"))

fun keyword syntax words =
  let fun isKeyword s = List.exists (fn s' => s = s') words
  in  (fn (NAME n) => if isKeyword n then SOME n else NONE | _ => NONE) <$>?
                                                                           token
  end

val expKeyword = keyword "type"       ["if", "lambda",
                                       "type-lambda", "let", "let*", "letrec"]

fun checkedForall tyvars tau =
  nodups ("quantified type variable", "forall") tyvars >>=+ 
  (fn a's => FORALL (a's, tau))

fun ty tokens = (
     TYCON <$> name
 <|> TYVAR <$> tyvar
 <|> bracket "function" "(function (types) type)"
                            (curry funtype <$> "(" >-- many ty --< ")" <*> ty)
 <|> badExpKeyword <$>! ("(" >-- @@ expKeyword <* scanToCloseParen)
 <|> curry CONAPP <$> "(" >-- ty <*> many ty --< ")" 
 <|> "(" >-- literal ")" <!> "empty type ()"
 <|> int     <!> "expected type; found integer"
 <|> booltok <!> "expected type; found Boolean literal"
) tokens
and badExpKeyword (loc, bad) =
      errorAt ("looking for type but found `" ^ bad ^ "'") loc

fun tyscheme tokens = (
     bracket "forall"    "(forall (tyvars) type)" 
                               (curry FORALL <$> "(" >-- forallNames --< ")" <*>
                                                                             ty)
 <|> curry FORALL [] <$> ty
) tokens
and forallNames ts = (
  nodups ("quantified type variable", "forall") <$>! @@ (many tyvar)
) ts
(* parsing ((haskell)) (BUG: software can't tell where this code came from) *)
fun define f formals body =
  nodups ("formal parameter", "definition of function " ^ f) formals >>=+
  (fn xs => DEFINE (f, (xs, body)))

val xdef = 
 DEF <$> (
     bracket "define"  "(define f (args) body)" (define <$> name <*> @@ formals
                                                                        <*>!exp)
 <|> bracket "val"     "(val x e)"              (curry VAL    <$> name <*> exp)
 <|> bracket "val-rec" "(val-rec x e)"          (curry VALREC <$> name <*> exp)
 <|> bracket "vcon"    "(vcon C ty)"            (curry CONDEF <$> name <*>
                                                                       tyscheme)
 )
 <|> bracket "use"     "(use filename)"         (USE          <$> name)
 <|> literal ")" <!> "unexpected right parenthesis"
 <|> DEF <$> EXP <$> exp
 <?> "definition"
(* parsing ((haskell)) (BUG: software can't tell where this code came from) *)
val haskellReader = reader (schemeToken, xdef)
exception TypeError of string
exception BlackHole


(*****************************************************************)
(*                                                               *)
(*   GENERIC READ-EVAL-PRINT LOOP                                *)
(*                                                               *)
(*****************************************************************)

(* generic read-eval-print loop 314b *)
(* type [[echo]] and function [[echoes]], for controlling whether definitions are echoed 313b *)
datatype echo = ECHOING | SILENT
fun echoes ECHOING = true
  | echoes SILENT  = false
(* type declarations for consistency checking *)
val _ = op echoes : echo -> bool
fun 'envs readEvalPrint runTests read errmsg processDef =
  let fun processXDefs (xdefs, envs) =
        let val unitTests = ref []

(* definition of [[processXDef]], which can modify [[unitTests]] and call [[errmsg]] 315a *)
            fun processXDef (xd, envs) =
              let fun useFile filename =
                    let val fd = TextIO.openIn filename
                        val xdefs = read (filename, streamOfLines fd)
                    in  processXDefs (xdefs, envs)
                        before TextIO.closeIn fd
                    end
                  fun continue msg = (errmsg msg; envs)
            (* type declarations for consistency checking *)
            val _ = op read       : string * string stream -> xdef stream
            val _ = op errmsg     : string -> unit
            val _ = op processDef : def  * 'envs -> 'envs
              in  (case xd
                     of USE filename => useFile filename
                      | TEST t       => (unitTests := t :: !unitTests; envs)
                      | DEF def      => processDef (def, envs)
                  ) handle IO.Io {name, ...} => continue ("I/O error: " ^ name)

(* more read-eval-print handlers (BUG: software can't tell where this code came from) *)
                    | BlackHole => continue "error: evaluation hit a black hole"
                    (* more read-eval-print handlers 315b *)
                    | Div               => continue "Division by zero"
                    | Overflow          => continue "Arithmetic overflow"
                    | RuntimeError msg  => continue ("run-time error: " ^ msg)
                    | NotFound n        => continue ("variable " ^ n ^
                                                                   " not found")
                    (* more read-eval-print handlers 427b *)
                    | TypeError          msg => continue ("type error: " ^ msg)
                    | BugInTypeInference msg => continue (
                                                "bug in type inference: " ^ msg)
              end 
            val envs = streamFold processXDef envs xdefs
            val _ = runTests (!unitTests, envs)
(* type declarations for consistency checking *)
val _ = op processXDefs : xdef stream    * 'envs -> 'envs
val _ = op processXDef  : xdef           * 'envs -> 'envs
val _ = op runTests     : unit_test list * 'envs -> unit
        in  envs
        end
  in  processXDefs
  end


(*****************************************************************)
(*                                                               *)
(*   TYPE INFERENCE                                              *)
(*                                                               *)
(*****************************************************************)

(* type inference ((prototype)) (BUG: software can't tell where this code came from) *)
exception LeftAsExercise of string
(* type inference ((haskell)) (BUG: software can't tell where this code came from) *)
fun typeof (e, Gamma) =
  let
(* function [[typesof]], to infer the types of a list of expressions ((ml/haskell)) 421a *)
      fun typesof ([],    Gamma) = ([], TRIVIAL)
        | typesof (e::es, Gamma) =
            let val (tau,  c)  = typeof  (e,  Gamma)
                val (taus, c') = typesof (es, Gamma)
            in  (tau :: taus, c /\ c')
            end

(* function [[literal]], to infer the type of a literal constant ((prototype)) 421c *)
      fun literal _ = raise LeftAsExercise "literal"

(* function [[literal]], to infer the type of a literal constant ((prototype)) (BUG: software can't tell where this code came from) *)
      fun literal _ = (valtype, idsubst)

(* function [[ty]], to infer the type of an expression, given [[Gamma]] ((prototype)) (BUG: software can't tell where this code came from) *)
      fun addvar (x, g) = bindtyscheme (x, FORALL([], valtype), g)
      val valtyc = (valtype, TRIVIAL)
      fun ty (LITERAL v) = valtyc
        | ty (VAR x) = (findtyscheme (x, Gamma); valtyc)
        | ty (APPLY (e, es)) = check (e::es)
        | ty (LETX (LETSTAR, [], body)) = ty body
        | ty (LETX (LETSTAR, (b :: bs), body)) =
                         ty (LETX (LET, [b], LETX (LETSTAR, bs, body)))
        | ty (LETX (LET, bs, body)) =
             let val (xs, es) = ListPair.unzip bs
             in  ty (APPLY (LAMBDA (xs, body), es))
             end
        | ty (LETX (LETREC, bs, body)) =
             let val (xs, es) = ListPair.unzip bs
                 val Gamma' = foldl addvar Gamma xs
             in  (map (fn e => typeof(e, Gamma')) es; typeof(body, Gamma'))
             end
        | ty (DO (bs, body)) = ty (LETX (LETSTAR, bs, body))
        | ty (LAMBDA (xs, e)) = typeof(e, foldl addvar Gamma xs)
        | ty (IFX (e1, e2, e3)) =
             ty (CASE (e1, [(("#t", []), e2), (("#f", []), e3)]))
        | ty (CASE (e, matches)) =
             let val _ = map (matchtype (ty e)) matches
             in  valtyc
             end
      and check es = (map ty es; valtyc)
      and matchtype tau_case ((con, xs), e) = typeof (e, foldl addvar Gamma xs)
  in  ty e
  end


(*****************************************************************)
(*                                                               *)
(*   CHECKING AND EVALUATION                                     *)
(*                                                               *)
(*****************************************************************)

(* checking and evaluation ((haskell)) (BUG: software can't tell where this code came from) *)
exception BugInTypeInference of string
(* evaluation ((haskell)) (BUG: software can't tell where this code came from) *)
(* definition of [[showforce]] (BUG: software can't tell where this code came from) *)
(* definitions of [[expString]] and [[prenv]] (BUG: software can't tell where this code came from) *)
(* definitions of [[separate]] and [[spaceSep]] 311a *)
fun separate (zero, sep) =  (* print list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^ sep ^ s t
  in  s
end
val spaceSep = separate ("", " ")  (* print separated by spaces *)
val rec expString = fn LITERAL v => valueString v
                     | VAR name => name
                     | APPLY (e, es) => prexps (e::es)
                     | LETX (lk, bs, e) => prWithBindings (prLetKind lk, bs, e)
                     | DO (bs, e) => prWithBindings ("do", bs, e)
                     | LAMBDA (xs, body) =>
                          "(lambda " ^ prnames xs ^ " " ^ expString body ^ ")"
                     | IFX (e1, e2, e3) => prexps [VAR "if", e1, e2, e3]
                     | CASE (e, ms) => foldl addMatch ("(case " ^ expString e)
                                                                              ms
and addMatch = fn ((m, b), pfx) => pfx ^ " (" ^ prApp m ^ " " ^ expString b ^
                                                                             ")"
and prApp = fn (c, []) => c
             | (c, xs) => prnames (c::xs)
and prexps  = fn l => "(" ^ spaceSep (map expString l) ^ ")"
and prnames = fn xs => "(" ^ spaceSep xs ^ ")"
and prWithBindings = fn (keyword, bs, e) =>
      "(" ^ keyword ^ " (" ^ prBindings bs ^ ") " ^ expString e ^ ")"
and prBindings = fn bs => separate ("", " ") (map prBinding bs)
and prBinding = fn (x, e) => "(" ^ x ^ " " ^ expString e ^ ")"
and prLetKind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
val showingforce = ref false
val forceindent = ref 0
fun indent 0 = ()
  | indent n = (print "  "; indent (n-1))
fun startforce exp =
  if !showingforce then
     ( indent (!forceindent)
     ; app print ["Forcing <|", expString exp, ", rho|>\n"]
     ; forceindent := !forceindent + 1
     )
  else ()
    
fun showforce(exp, v) =
  if !showingforce then
     ( forceindent := !forceindent - 1
     ; indent (!forceindent)
     ; app print ["Forced <|", expString exp, ", rho|> to ", valueString  v,
                                                                           "\n"]
     )
  else ()
exception LeftAsExercise of string
fun force (ref (VALUE v)) = v
  | force (ref IN_PROGRESS) = raise BlackHole
  | force (thunk as ref (UNEVAL (exp, rho))) =
       let val () = thunk := IN_PROGRESS
           val () = startforce exp
           val v  = eval (exp, rho)
           val () = thunk := VALUE v
           val () = showforce (exp, v)
(* type declarations for consistency checking *)
val _ = op eval  : thunk -> value
val _ = op force : loc   -> value
       in  v
       end
(* evaluation ((haskell)) (BUG: software can't tell where this code came from) *)
and eval (e, rho) =
  let fun toThunk e = allocThunk (e, rho)
      fun ev (LITERAL v) = v
        | ev (VAR     x) = force (find (x, rho))
        | ev (CASE (e, arms)) =
            (case ev e
               of VCON (c, args) =>
                    (case List.filter (fn ((c', _), _) => c = c') arms
                       of [((_, formals), body)] =>
                            (eval(body, bindList (formals, args, rho))
                             handle BindListLength =>

(* bleat about [[c]] with [[args]] and [[formals]] (BUG: software can't tell where this code came from) *)
                                 raise RuntimeError ("Constructor " ^ c ^
                                                            " was applied to " ^
                                                     Int.toString (length args)
                                          ^ " arguments, but case expression " ^
                                                     "expected " ^ Int.toString
                                              (length formals) ^ " arguments "))
                        | [] => raise RuntimeError ("Case expression had no " ^

                                              "alternative for constructor " ^ c
 ^ "; alternatives were: " ^ separate ("<none>", ", ") (map (fn ((c, _), _) => c
                                                                        ) arms))
                        | _ :: _ :: _ =>
                            raise RuntimeError ("Case expression had multiple "
                                                                               ^
                                                "alternatives for constructor "
                                                                            ^ c)
                    )
                | v => raise RuntimeError (
                                     "Case discrimination on non-constructor " ^
                                           valueString v)
             )

(* cases for evaluation of [[do]] notation (BUG: software can't tell where this code came from) *)
        | ev (DO ([], body)) = ev body
        | ev (DO ((x, e) :: bindings, body)) =
           ev (APPLY (LITERAL (PRIMITIVE monadicBindList),
                      [e, LAMBDA([x], DO (bindings, body))]))

(* other cases for internal function [[ev]] ((prototype)) (BUG: software can't tell where this code came from) *)
        | ev _ = raise LeftAsExercise "evaluating thunks"
  in  ev e
  end
(* [[and]] definitions of monadic functions (BUG: software can't tell where this code came from) *)
and monadicBind (mLoc, kLoc) =
  IO (fn () => let val a = runIO (force mLoc)
               in  runIO (force (apply (kLoc, [a])))
               end)
and runIO (IO f) = f ()
  | runIO _      = raise BugInTypeInference "expected I/O action"
and apply (f, args) =
  allocThunk (APPLY (LITERAL (force f), map (LITERAL o force) args), emptyEnv)
and monadicBindList [m, k] = monadicBind (m, k)
  | monadicBindList _ = let exception ThisCan'tHappen in raise ThisCan'tHappen
                                                                             end
(* evaluation ((haskell)) (BUG: software can't tell where this code came from) *)
fun fullyEval v =
  let fun full (VCON (c, args)) = app (full o force) args
        | full (NUM n)       = ()
        | full (SYM v)       = ()
        | full (CLOSURE   _) = ()
        | full (PRIMITIVE _) = ()
        | full (IO _       ) = ()
      val _ = full v
(* type declarations for consistency checking *)
val _ = op fullyEval : value -> value
  in  v
  end
(* evaluation ((haskell)) (BUG: software can't tell where this code came from) *)
fun evaldef (d, rho : loc env) =
  case d
    of VALREC (name, e)      => let val cell = ref IN_PROGRESS
                                    val rho  = bind (name, cell, rho)
                                    val _    = cell := UNEVAL (e, rho)
                                in  (rho, locString cell)
                                end
     | VAL    (name, e)      => let val cell = ref IN_PROGRESS
                                    val _    = cell := UNEVAL (e, rho)
                                    val rho  = bind (name, cell, rho)
                                in  (rho, locString cell)
                                end
     | EXP e                 =>
(* evaluate [[e]] and run or bind the result (BUG: software can't tell where this code came from) *)
                                (case eval (e, rho)
                                   of IO action => (action () ; (rho, ""))
                                    | v => let val cell = allocValue v
                                           in  (bind ("it", cell, rho),
                                                                 locString cell)
                                           end)
     | DEFINE (name, lambda) => evaldef (VALREC (name, LAMBDA lambda), rho)
     | CONDEF (name, ty)     => let val cell = allocValue (vconValue (name, ty))
                                    val rho  = bind (name, cell, rho)
                                in  (rho, name)
                                end
and vconValue (name, sigma) =
  if takesArgs sigma then
    PRIMITIVE (fn args => VCON (name, args))
  else
    VCON (name, [])
and takesArgs (FORALL (_, CONAPP (TYCON "function", _))) = true
  | takesArgs (FORALL (_, _))                            = false
(* evaluation ((haskell)) (BUG: software can't tell where this code came from) *)
fun elabdef (d, Gamma) =
  case d
    of VALREC (x, e)      =>
                            (* infer and bind type for [[VALREC (x, e)]] 422c *)
                             let val alpha    = freshtyvar ()
                                 val Gamma'   = bindtyscheme (x, FORALL ([],
                                                                  alpha), Gamma)
                                 val (tau, c) = typeof (e, Gamma')
                                 val theta    = solve (c /\ alpha ~ tau)
                                 val sigma    = generalize (tysubst theta alpha,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | VAL    (x, e)      =>
                            (* infer and bind type for [[VAL    (x, e)]] 422b *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme (x, sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | EXP e              =>
(* infer and bind type for [[EXP e]] ((haskell)) (BUG: software can't tell where this code came from) *)
                             let val (tau, c) = typeof (e, Gamma)
                                 val theta    = solve c
                                 val sigma    = generalize (tysubst theta tau,
                                                          freetyvarsGamma Gamma)
                             in  (bindtyscheme ("it", sigma, Gamma),
                                                         typeSchemeString sigma)
                             end
     | DEFINE (x, lambda) => elabdef (VALREC (x, LAMBDA lambda), Gamma)
     | CONDEF (c, sigma)  =>
(* bind constructor [[c]] to type [[sigma]] (BUG: software can't tell where this code came from) *)
                             (* XXX missing kind check on sigma *)
                             (bindtyscheme (c, sigma, Gamma), typeSchemeString
                                                                          sigma)
type env_bundle = type_env * loc env
fun elabEvalDef echo (d, envs as (Gamma, rho)) =
  let val (Gamma, tystring)  = elabdef (d, Gamma)
      val (rho,   valstring) = evaldef (d, rho)
      val _ = if echoes echo andalso size valstring > 0 then
                println (valstring ^ " : " ^ tystring)
              else
                ()
(* type declarations for consistency checking *)
val _ = op evaldef : def * loc env -> loc env * string
(* type declarations for consistency checking *)
val _ = op elabEvalDef : echo -> def * env_bundle -> env_bundle
        in  (Gamma, rho)
        end


(*****************************************************************)
(*                                                               *)
(*   PRIMITIVES                                                  *)
(*                                                               *)
(*****************************************************************)

(* primitives ((haskell)) (BUG: software can't tell where this code came from) *)
fun strict f = f o map force
fun arityError n args =
  raise RuntimeError ("primitive function expected " ^ Int.toString n ^
                      "arguments; got " ^ Int.toString (length args))
fun binary_valop f = strict (fn [a, b] => f(a, b) | args => arityError 2 args)
fun unary_valop  f = strict (fn [a]    => f a     | args => arityError 1 args)
(* type declarations for consistency checking *)
val _ = op strict : (value list -> value) -> primitive
val _ = op unary_valop : (value -> value) -> primitive
(* primitives ((haskell)) (BUG: software can't tell where this code came from) *)
fun arithop f = binary_valop (fn (NUM n1, NUM n2) => NUM (f (n1, n2))
                               | _ => raise RuntimeError "integers expected")
val arithtype    = funtype ([inttype, inttype], inttype)
fun comptype tau = funtype ([tau, tau], booltype)
(* primitives ((haskell)) (BUG: software can't tell where this code came from) *)
fun inject_bool x =
      VCON (if x then "#t" else "#f", [])
fun project_bool (VCON ("#t", [])) = true
  | project_bool (VCON ("#f", [])) = false
  | project_bool _ = raise RuntimeError "projected non-boolean"

fun inject_predicate f = fn x => inject_bool (f x)
fun predop f = unary_valop  (inject_predicate f)

fun comparison f = binary_valop (inject_predicate f)
fun intcompare f = comparison (
                     fn (NUM n1, NUM n2) => f (n1, n2)
                      | _ => raise BugInTypeInference "integers expected")
(* type declarations for consistency checking *)
val _ = op inject_bool  : bool -> value
val _ = op project_bool : value -> bool
(* primitives ((haskell)) (BUG: software can't tell where this code came from) *)
fun ns_unary  f = (fn [a]    => f a      | args => arityError 1 args)
fun ns_binary f = (fn [a, b] => f (a, b) | args => arityError 2 args)
(* primitives ((haskell)) (BUG: software can't tell where this code came from) *)
val unitval   = VCON ("unit", [])
val unitthunk = allocValue unitval
(* type declarations for consistency checking *)
val _ = op apply : loc * loc list -> loc
val _ = op monadicBind : loc * loc -> value


(*****************************************************************)
(*                                                               *)
(*   UNIT TESTING                                                *)
(*                                                               *)
(*****************************************************************)

(* unit testing ((haskell)) (BUG: software can't tell where this code came from) *)
fun checkAndRunTests ([], envs) = ()
  | checkAndRunTests _ =
      let exception UnitTestingUnimplemented in raise UnitTestingUnimplemented
                                                                             end


(*****************************************************************)
(*                                                               *)
(*   INITIALIZATION                                              *)
(*                                                               *)
(*****************************************************************)

(* initialization ((haskell)) (BUG: software can't tell where this code came from) *)
val initialEnvs =
  let fun addPrim ((name, prim, tau), (Gamma, rho)) = 
        ( bindtyscheme (name, generalize (tau, freetyvarsGamma Gamma), Gamma)
        , bind (name, allocValue (PRIMITIVE prim), rho)
        )
      val envs  = foldl addPrim (emptyTypeEnv, emptyEnv) (
(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ("full", unary_valop
                                         fullyEval, funtype ([alpha], alpha)) ::

(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ( "showforce"
                                                          , unary_valop (fn x =>
                                            (showingforce := project_bool x; x))
                                                          , funtype ([booltype],
                                                                       unittype)
                                                          ) ::

(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ("+", arithop op +,
                                                                   arithtype) ::
                                                          ("-", arithop op -,
                                                                   arithtype) ::
                                                          ("*", arithop op * ,
                                                                   arithtype) ::
                                                          ("/", arithop op div,
                                                                   arithtype) ::

(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ("<", intcompare op <,
                                                            comptype inttype) ::
                                                          (">", intcompare op >,
                                                            comptype inttype) ::
                                                          ("=", comparison (fn (
                                                      NUM n1, NUM n2) => n1 = n2
                                                                             | (
                                                      SYM v1, SYM v2) => v1 = v2
                                                                             |
                      _ => raise RuntimeError "equality used on non-base type"),
                                                                comptype alpha)
                                                                              ::
                                                          (*
                                                          ("number?",  predop (
                                           fn (NUM  _) => true | _ => false)) ::
                                                          ("symbol?",  predop (
                                           fn (SYM  _) => true | _ => false)) ::
                                                          *)

(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ("error", unary_valop
                       (fn v => raise RuntimeError (valueString (fullyEval v))),
                                                                      funtype ([
                                                               alpha], beta)) ::
                                                          ("show", unary_valop (
                                                             SYM o valueString),
                                                                    funtype ([
                                                            alpha], symtype)) ::
                                                          ("symwidth",
                                         unary_valop (fn (SYM s) => NUM (size s)

                     | _ => raise BugInTypeInference "symwidth got non-symbol"),
                                                                    funtype ([
                                                          symtype], inttype)) ::

(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ("cons", ns_binary (fn
                                                (a, b) => VCON("cons", [a, b])),

                           funtype ([alpha, listtype alpha], listtype alpha)) ::

                                 (* ("tuple", fn es => VCON ("tuple", es)) :: *)

(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ("trace", let fun
                                       bleat s = TextIO.output(TextIO.stdErr, s)
                                                                    in
                                                       ns_binary (fn (msg, v) =>

                                                      (app bleat ["**TRACE**: ",

                                      valueString (fullyEval (force msg)), "\n"]

                                                                     ; force v))
                                                                    end,

                                               funtype ([alpha, beta], beta)) ::

(* primitives [[::]] ((haskell)) (BUG: software can't tell where this code came from) *)
                                                          ("print", unary_valop
                (fn v => IO (fn () => ( print (valueString (fullyEval v) ^ "\n")

                                                                 ; unitthunk))),
                                                                    funtype ([
                                                    alpha], iotype unittype)) ::
                                                          ("return", ns_unary (
                                               fn thunk => IO (fn () => thunk)),
                                                                    funtype ([
                                                       alpha], iotype alpha)) ::
                                                          (">>=", ns_binary
                                                                    monadicBind,
                                                                    funtype ([
            iotype alpha, funtype ([alpha], iotype beta)], iotype beta)) :: nil)
      val basis  =
                (* ML representation of initial basis (generated by a script) *)

                    [ "(define map (f xs)"
                    , "  (case xs"
                    , "     (() '())"
                    , "     ((cons y ys) (cons (f y) (map f ys)))))"
                    ,
                  "(vcon pair (forall ('a 'b) (function ('a 'b) (pair 'a 'b))))"
                    , "(define fst (p)"
                    , "   (case p ((pair x y) x)))"
                    , "(define snd (p)"
                    , "   (case p ((pair x y) y)))"
                    , "(define list1 (x) (cons x '()))"
                    , "(define bind (x y alist)"
                    , "  (case alist"
                    , "       (() (list1 (pair x y)))"
                    , "       ((cons p ps)"
                    , "          (if (= x (fst p))"
                    , "              (cons (pair x y) ps)"
                    , "              (cons p (bind x y ps))))))"
                    , "(define null? (xs)"
                    , "   (case xs (() #t)"
                    , "            ((cons y ys) #f)))"
                    , "(define car (xs)"
                    , "   (case xs (() (error 'car-of-empty-list))"
                    , "            ((cons x _) x)))"
                    , "(define cdr (xs)"
                    , "   (case xs (() (error 'cdr-of-empty-list))"
                    , "            ((cons _ xs) xs)))"
                    , "(define isbound? (x alist)"
                    , "  (if (null? alist) "
                    , "    #f"
                    , "    (if (= x (fst (car alist)))"
                    , "      #t"
                    , "      (isbound? x (cdr alist)))))"
                    , "(define find (x alist)"
                    , "  (if (null? alist) "
                    , "    (error 'not-found)"
                    , "    (if (= x (fst (car alist)))"
                    , "      (snd (car alist))"
                    , "      (find x (cdr alist)))))"
                    , "(vcon unit unit)"
                    , "(define >> (m1 m2) (>>= m1 (lambda (_) m2)))"
                    , "(define mapM_ (f xs)"
                    , "  (case xs (() (return unit))"
                    , "           ((cons y ys) (>> (f y) (mapM_ f ys)))))"
                    , "(vcon some (forall ('a) (function ('a) (option 'a))))"
                    , "(vcon none (forall ('a) (option 'a)))"
                    , "(define tails (xs)"
                    , "  (cons xs (if (null? xs) '() (tails (cdr xs)))))"
                    , "(define caar (xs) (car (car xs)))"
                    , "(define cadr (xs) (car (cdr xs)))"
                    , "(define cdar (xs) (cdr (car xs)))"
                    , "(define cddr (xs) (cdr (cdr xs)))"
                    , "(define cdddr (xs) (cdr (cddr xs)))"
                    , "(define length (xs)"
                    , "  (if (null? xs) 0"
                    , "    (+ 1 (length (cdr xs)))))"
                    , "(define and (b c) (if b  c  b))"
                    , "(define or  (b c) (if b  b  c))"
                    , "(define not (b)   (if b #f #t))"
                    , "(define append (xs ys)"
                    , "  (if (null? xs)"
                    , "     ys"
                    , "     (cons (car xs) (append (cdr xs) ys))))"
                    , "(define revapp (xs ys)"
                    , "  (if (null? xs)"
                    , "     ys"
                    , "     (revapp (cdr xs) (cons (car xs) ys))))"
                    , "(define reverse (xs) (revapp xs '()))"
                    , "(define o (f g) (lambda (x) (f (g x))))"
                    , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
                    , "(define uncurry (f) (lambda (x y) ((f x) y)))"
                    , "(define filter (p? l)"
                    , "  (if (null? l)"
                    , "    '()"
                    , "    (if (p? (car l))"
                    , "      (cons (car l) (filter p? (cdr l)))"
                    , "      (filter p? (cdr l)))))"
                    , "(define exists? (p? l)"
                    , "  (if (null? l)"
                    , "    #f"
                    , "    (if (p? (car l)) "
                    , "      #t"
                    , "      (exists? p? (cdr l)))))"
                    , "(define all? (p? l)"
                    , "  (if (null? l)"
                    , "    #t"
                    , "    (if (p? (car l))"
                    , "      (all? p? (cdr l))"
                    , "      #f)))"
                    , "(define foldr (op zero l)"
                    , "  (if (null? l)"
                    , "    zero"
                    , "    (op (car l) (foldr op zero (cdr l)))))"
                    , "(define foldl (op zero l)"
                    , "  (if (null? l)"
                    , "    zero"
                    , "    (foldl op (op (car l) zero) (cdr l))))"
                    , "(define <= (x y) (not (> x y)))"
                    , "(define >= (x y) (not (< x y)))"
                    , "(define != (x y) (not (= x y)))"
                    , "(define max (x y) (if (> x y) x y))"
                    , "(define min (x y) (if (< x y) x y))"
                    , "(define mod (m n) (- m (* n (/ m n))))"
                    , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
                    , "(define lcm (m n) (* m (/ n (gcd m n))))"
                    , "(define min* (l) (foldr min (car l) (cdr l)))"
                    , "(define max* (l) (foldr max (car l) (cdr l)))"
                    , "(define gcd* (l) (foldr gcd (car l) (cdr l)))"
                    , "(define lcm* (l) (foldr lcm (car l) (cdr l)))"
                    , "(define list1 (x)               (cons x '()))"
                    , "(define list2 (x y)             (cons x (list1 y)))"
                    , "(define list3 (x y z)           (cons x (list2 y z)))"
                    , "(define list4 (x y z a)         (cons x (list3 y z a)))"
                    ,
                     "(define list5 (x y z a b)       (cons x (list4 y z a b)))"
                    ,
                   "(define list6 (x y z a b c)     (cons x (list5 y z a b c)))"
                    ,
                 "(define list7 (x y z a b c d)   (cons x (list6 y z a b c d)))"
                    ,
               "(define list8 (x y z a b c d e) (cons x (list7 y z a b c d e)))"
                    , "(define takewhile (p? l)"
                    , "  (if (null? l)"
                    , "     '()"
                    , "     (if (p? (car l))"
                    , "         (cons (car l) (takewhile p? (cdr l)))"
                    , "         '())))"
                    , "(define dropwhile (p? l)"
                    , "  (if (null? l)"
                    , "     '()"
                    , "     (if (p? (car l))"
                    , "         (dropwhile p? (cdr l))"
                    , "         l)))"
                    , "(define drop (n l)"
                    , "  (if (null? l)"
                    , "     '()"
                    , "     (if (= n 0)"
                    , "         l"
                    , "         (drop (- n 1) (cdr l)))))"
                    , "(define nth (xs n)"
                    , "   (car (drop n xs)))"
                    , "(define take (n xs)"
                    , "  (if (or (= n 0) (null? xs))"
                    , "      '()"
                    , "      (cons (car xs) (take (- n 1) (cdr xs)))))"
                     ]
      fun writeln s = app print [s, "\n"]
      fun bleat s = TextIO.output(TextIO.stdErr, s ^ "\n")
      val defs = haskellReader noPrompts ("initial basis", streamOfList basis)
      val repl = readEvalPrint checkAndRunTests (haskellReader noPrompts)
                 basisError (elabEvalDef SILENT) 
  in  repl (defs, envs)
  end
(* initialization ((haskell)) (BUG: software can't tell where this code came from) *)
fun runInterpreter prompts = 
  let val defs = haskellReader prompts ("standard input", streamOfLines
                                                                   TextIO.stdIn)
      val repl = readEvalPrint checkAndRunTests (haskellReader noPrompts)
                 eprintln (elabEvalDef ECHOING) 
  in  ignore (repl (defs, initialEnvs))
  end 
(* type declarations for consistency checking *)
val _ = op runInterpreter : prompts -> unit


(*****************************************************************)
(*                                                               *)
(*   COMMAND LINE                                                *)
(*                                                               *)
(*****************************************************************)

(* command line 316c *)
fun main ["-q"] = runInterpreter noPrompts
  | main []     = runInterpreter stdPrompts
  | main _      =
      TextIO.output (TextIO.stdErr, "Usage: " ^ CommandLine.name () ^ " [-q]\n")
val _ = main (CommandLine.arguments ())
