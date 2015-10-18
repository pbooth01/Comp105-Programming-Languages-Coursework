(* mlscheme.sml 317 *)


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

(* abstract syntax and values ((mlscheme)) 307 *)
datatype exp = LITERAL of value
             | VAR     of name
             | SET     of name * exp
             | IFX     of exp * exp * exp
             | WHILEX  of exp * exp
             | BEGIN   of exp list
             | APPLY   of exp * exp list
             | LETX    of let_kind * (name * exp) list * exp
             | LAMBDA  of lambda
and let_kind = LET | LETREC | LETSTAR
and    value = NIL
             | BOOL      of bool   
             | NUM       of int
             | SYM       of name
             | PAIR      of value * value
             | CLOSURE   of lambda * value ref env
             | PRIMITIVE of primitive
withtype primitive = value list -> value (* raises RuntimeError *)
     and lambda    = name list * exp

exception RuntimeError of string (* error message *)
(* abstract syntax and values ((mlscheme)) 308a *)
datatype unit_test = CHECK_EXPECT of exp * exp
                   | CHECK_ERROR  of exp
(* abstract syntax and values ((mlscheme)) 308b *)
datatype def  = VAL    of name * exp
              | EXP    of exp
              | DEFINE of name * lambda
datatype xdef = DEF    of def
              | USE    of name
              | TEST   of unit_test


(*****************************************************************)
(*                                                               *)
(*   VALUES                                                      *)
(*                                                               *)
(*****************************************************************)

(* values ((mlscheme)) 308c *)



fun embedList []     = NIL
  | embedList (h::t) = PAIR (h, embedList t)
fun embedPredicate f = fn x => BOOL (f x)
fun bool (BOOL b) = b
  | bool _        = true
(* type declarations for consistency checking *)
val _ = op embedList      : value list -> value
val _ = op embedPredicate : ('a -> bool) -> ('a -> value)
val _ = op bool           : value -> bool
(* values ((mlscheme)) 309a *)
fun valueString (NIL)    = "()"
  | valueString (BOOL b) = if b then "#t" else "#f"
  | valueString (NUM n)  = String.map (fn #"~" => #"-" | c => c) (Int.toString n
                                                                               )
  | valueString (SYM v)  = v
  | valueString (PAIR (car, cdr))  = 
      let fun tail (PAIR (car, cdr)) = " " ^ valueString car ^ tail cdr
            | tail NIL = ")"
            | tail v = " . " ^ valueString v ^ ")"
      in  "(" ^ valueString car ^ tail cdr
      end
  | valueString (CLOSURE   _) = "<procedure>"
  | valueString (PRIMITIVE _) = "<procedure>"
(* type declarations for consistency checking *)
val _ = op valueString : value -> string
(* values ((mlscheme)) 309b *)
fun equalatoms (NIL,     NIL    ) = true
  | equalatoms (NUM  n1, NUM  n2) = (n1 = n2)
  | equalatoms (SYM  v1, SYM  v2) = (v1 = v2)
  | equalatoms (BOOL b1, BOOL b2) = (b1 = b2)
  | equalatoms  _                 = false
(* values ((mlscheme)) 309c *)
fun equalpairs (PAIR (car1, cdr1), PAIR (car2, cdr2)) =
      equalpairs (car1, car2) andalso equalpairs (cdr1, cdr2)
  | equalpairs (v1, v2) = equalatoms (v1, v2)


(*****************************************************************)
(*                                                               *)
(*   PARSING                                                     *)
(*                                                               *)
(*****************************************************************)

(* parsing ((mlscheme)) 789a *)
val name    = (fn (NAME  n) => SOME n  | _ => NONE) <$>? token
val booltok = (fn (SHARP b) => SOME b  | _ => NONE) <$>? token
val int     = (fn (INT   n) => SOME n  | _ => NONE) <$>? token
val quote   = (fn (QUOTE)   => SOME () | _ => NONE) <$>? token
(* parsing ((mlscheme)) 789b *)
fun sexp tokens = (
     SYM          <$> (notDot <$>! name)
 <|> NUM          <$> int
 <|> BOOL         <$> booltok
 <|> embedList    <$> "(" >-- many sexp --< ")"
 <|> (fn v => embedList [SYM "quote", v]) 
                  <$> (quote *> sexp)
) tokens
and notDot "." = ERROR
                      "this interpreter cannot handle . in quoted S-expressions"
  | notDot s   = OK s
(* type declarations for consistency checking *)
val _ = op sexp : value parser
(* parsing ((mlscheme)) 789c *)
val formals = 
  "(" >-- many name --< ")"
fun lambda xs exp =
  nodups ("formal parameter", "lambda") xs >>=+ (fn xs => LAMBDA (xs, exp))
(* type declarations for consistency checking *)
val _ = op formals : name list parser
val _ = op lambda  : name list located -> exp -> exp error
(* parsing ((mlscheme)) 789d *)
local
  fun letDups LETSTAR (loc, bindings) = OK bindings
    | letDups kind    (loc, bindings) =
        let val names    = map (fn (n, _) => n) bindings
            val kindName = case kind of LET => "let" | LETREC => "letrec" | _ =>
                                                                            "??"
        in  nodups ("bound name", kindName) (loc, names) >>=+ (fn _ => bindings)
        end
in
  fun letx kind bs exp = letDups kind bs >>=+ (fn bs => LETX (kind, bs, exp))
end
(* type declarations for consistency checking *)
val _ = op letx : let_kind -> (name * exp) list located -> exp -> exp error
(* parsing ((mlscheme)) 790a *)
val br = bracket
fun exp tokens = (
     VAR              <$> name
 <|> LITERAL <$> NUM  <$> int
 <|> LITERAL <$> BOOL <$> booltok
 <|> LITERAL          <$> (quote *> sexp)
 <|> br "if"     "(if e1 e2 e3)"            (curry3 IFX     <$> exp  <*> exp <*>
                                                                            exp)
 <|> br "while"  "(while e1 e2)"            (curry  WHILEX  <$> exp  <*> exp)
 <|> br "set"    "(set x e)"                (curry  SET     <$> name <*> exp)
 <|> br "begin"  ""                         (       BEGIN   <$> many exp)
 <|> br "lambda" "(lambda (names) body)"    (       lambda  <$> @@ formals  <*>!
                                                                            exp)
 <|> br "let"    "(let (bindings) body)"    (letx   LET     <$> @@ bindings <*>!
                                                                            exp)
 <|> br "letrec" "(letrec (bindings) body)" (letx   LETREC  <$> @@ bindings <*>!
                                                                            exp)
 <|> br "let*"   "(let* (bindings) body)"   (letx   LETSTAR <$> @@ bindings <*>!
                                                                            exp)
 <|> "(" >-- literal ")" <!> "empty application"
 <|> curry APPLY <$> "(" >-- exp <*> many exp --< ")"
) tokens
and bindings ts = ("(" >-- (many binding --< ")" <?> "(x e)...")) ts
and binding  ts = ("(" >-- (pair <$> name <*> exp --< ")" <?>
                                                        "(x e) in bindings")) ts
(* type declarations for consistency checking *)
val _ = op exp      : exp parser
val _ = op bindings : (name * exp) list parser
(* parsing ((mlscheme)) 790b *)
fun dfn f formals body =
  nodups ("formal parameter", "definition of function " ^ f) formals >>=+
  (fn xs => DEFINE (f, (xs, body)))
(* type declarations for consistency checking *)
val _ = op dfn : name -> name list located -> exp -> def error
(* parsing ((mlscheme)) 790c *)
val unit_test =
      br "check-expect" "(check-expect e1 e2)" (curry CHECK_EXPECT <$> exp <*>
                                                                            exp)
  <|> br "check-error"  "(check-error e)"      (      CHECK_ERROR  <$> exp)
(* type declarations for consistency checking *)
val _ = op unit_test : unit_test parser
(* parsing ((mlscheme)) 791a *)
val xdef = 
  DEF <$> (
     bracket "define" "(define f (args) body)" (dfn <$> name <*> @@ formals <*>!
                                                                            exp)
 <|> bracket "val"    "(val x e)"              (curry VAL <$> name <*> exp)
 )
 <|> bracket "use"    "(use filename)"         (USE       <$> name)
 <|> TEST <$> unit_test
 <|> literal ")" <!> "unexpected right parenthesis"
 <|> DEF <$> EXP <$> exp
 <?> "definition"

(* type declarations for consistency checking *)
val _ = op xdef : xdef parser
(* parsing ((mlscheme)) 791b *)
val schemeReader = reader (schemeToken, xdef)
(* type declarations for consistency checking *)
val _ = op schemeReader : prompts -> string * line stream -> xdef stream


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
                    (* more read-eval-print handlers 315b *)
                    | Div               => continue "Division by zero"
                    | Overflow          => continue "Arithmetic overflow"
                    | RuntimeError msg  => continue ("run-time error: " ^ msg)
                    | NotFound n        => continue ("variable " ^ n ^
                                                                   " not found")
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
(*   EVALUATION                                                  *)
(*                                                               *)
(*****************************************************************)

(* evaluation ((mlscheme)) 309d *)
(* definitions of [[separate]] and [[spaceSep]] 311a *)
fun separate (zero, sep) =  (* print list with separator *)
  let fun s []     = zero
        | s [x]    = x
        | s (h::t) = h ^ sep ^ s t
  in  s
end
val spaceSep = separate ("", " ")  (* print separated by spaces *)
fun eval (e, rho) =
  let fun ev (LITERAL n) = n
        (* more alternatives for [[ev]] ((mlscheme)) 310a *)
        | ev (VAR v) = !(find(v, rho))
        | ev (SET (n, e)) = 
            let val v = ev e
            in  find (n, rho) := v;
                v
            end
        (* more alternatives for [[ev]] ((mlscheme)) 310b *)
        | ev (IFX (e1, e2, e3)) = ev (if bool (ev e1) then e2 else e3)
        | ev (WHILEX (guard, body)) = 
            if bool (ev guard) then 
              (ev body; ev (WHILEX (guard, body)))
            else
              NIL
        | ev (BEGIN es) =
            let fun b (e::es, lastval) = b (es, ev e)
                  | b (   [], lastval) = lastval
            in  b (es, BOOL false)
            end
        (* more alternatives for [[ev]] ((mlscheme)) 310c *)
        | ev (LAMBDA l) = CLOSURE (l, rho)
        | ev (APPLY (f, args))  = 
               (case ev f
                  of PRIMITIVE prim => prim (map ev args)
                   | CLOSURE clo    =>
                       (* apply closure [[clo]] to [[args]] ((mlscheme)) 310d *)
                                       let val ((formals, body), savedrho) = clo
                                           val actuals = map ev args
                                       in  eval (body, bindList (formals, map
                                                         ref actuals, savedrho))
                                           handle BindListLength => 
                                               raise RuntimeError (
                                      "Wrong number of arguments to closure; " ^
                                                                   "expected ("
                                                       ^ spaceSep formals ^ ")")
                                       end
                   | v => raise RuntimeError ("Applied non-function " ^
                                                                  valueString v)
               )
        (* more alternatives for [[ev]] ((mlscheme)) 311b *)
        | ev (LETX (LET, bs, body)) =
            let val (names, values) = ListPair.unzip bs
        (* type declarations for consistency checking *)
        val _ = ListPair.unzip : ('a * 'b) list -> 'a list * 'b list
            in  eval (body, bindList (names, map (ref o ev) values, rho))
            end
        | ev (LETX (LETSTAR, bs, body)) =
            let fun step ((n, e), rho) = bind (n, ref (eval (e, rho)), rho)
            in  eval (body, foldl step rho bs)
            end
        | ev (LETX (LETREC, bs, body)) =
            let val (names, values) = ListPair.unzip bs
                val rho' = bindList (names, map (fn _ => ref NIL) values, rho)
                val bs = map (fn (n, e) => (n, eval (e, rho'))) bs
            in  List.app (fn (n, v) => find (n, rho') := v) bs; 
                eval (body, rho')
            end
  in  ev e
  end
(* type declarations for consistency checking *)
val _ = op equalatoms : value * value -> bool
(* type declarations for consistency checking *)
val _ = op equalpairs : value * value -> bool
(* type declarations for consistency checking *)
val _ = op eval : exp * value ref env -> value
(* evaluation ((mlscheme)) 313c *)
fun evaldef echoLevel (d, rho) =
  let (* definitions of [[addName]] and [[showVal]] 314a *)
      fun addName (name, rho) = (find (name, rho); rho)
                                handle NotFound _ => bind (name, ref NIL, rho)
      fun showVal name (LAMBDA _) _ = name
        | showVal name _          v = valueString v
      val echo = if echoes echoLevel then println else ignore
(* type declarations for consistency checking *)
val _ = op evaldef : echo -> def  * value ref env -> value ref env
val _ = op addName : name * value ref env           -> value ref env
val _ = op showVal : name -> exp -> value -> string
  in  case d
        of VAL (name, e)         => let val rho = addName (name, rho)
                                        val v   = eval (e, rho)
                                    in  ( find (name, rho) := v
                                        ; echo (showVal name e v)
                                        ; rho
                                        )
                                    end
         | EXP e                 => let val v   = eval (e, rho)
                                        val rho = addName ("it", rho)
                                    in  ( find ("it", rho) := v
                                        ; echo (valueString v)
                                        ; rho
                                        )
                                    end
         | DEFINE (name, lambda) => evaldef echoLevel (VAL (name, LAMBDA lambda)
                                                                          , rho)
  end


(*****************************************************************)
(*                                                               *)
(*   PRIMITIVES                                                  *)
(*                                                               *)
(*****************************************************************)

(* primitives ((mlscheme)) 311c *)
fun arityError n args =
  raise RuntimeError ("primitive function expected " ^ Int.toString n ^
                      " arguments; got " ^ Int.toString (length args))
fun binaryOp f = (fn [a, b] => f (a, b) | args => arityError 2 args)
fun unaryOp  f = (fn [a]    => f a      | args => arityError 1 args)
(* type declarations for consistency checking *)
val _ = op unaryOp  : (value         -> value) -> (value list -> value)
val _ = op binaryOp : (value * value -> value) -> (value list -> value)
(* primitives ((mlscheme)) 312a *)
fun arithOp f = binaryOp (fn (NUM n1, NUM n2) => NUM (f (n1, n2)) 
                           | _ => raise RuntimeError "integers expected")
(* type declarations for consistency checking *)
val _ = op arithOp: (int * int -> int) -> (value list -> value)
(* primitives ((mlscheme)) 312c *)
fun predOp f     = unaryOp  (embedPredicate f)
fun comparison f = binaryOp (embedPredicate f)
fun intcompare f = comparison (fn (NUM n1, NUM n2) => f (n1, n2)
                                | _ => raise RuntimeError "integers expected")
(* type declarations for consistency checking *)
val _ = op predOp     : (value         -> bool) -> (value list -> value)
val _ = op comparison : (value * value -> bool) -> (value list -> value)
val _ = op intcompare : (int   * int   -> bool) -> (value list -> value)


(*****************************************************************)
(*                                                               *)
(*   UNIT TESTING                                                *)
(*                                                               *)
(*****************************************************************)

(* unit testing ((mlscheme)) 793d *)
val testEqual = equalpairs
(* unit-testing utilities 792a *)
(* definition of function [[expString]] ((mlscheme)) 794 *)
fun expString e =
  let fun bracket s = "(" ^ s ^ ")"
      val bracketSpace = bracket o spaceSep
      fun exps es = map expString es
      fun withBindings (keyword, bs, e) =
        bracket (spaceSep [keyword, bindings bs, expString e])
      and bindings bs = bracket (spaceSep (map binding bs))
      and binding (x, e) = bracket (x ^ " " ^ expString e)
      val letkind = fn LET => "let" | LETSTAR => "let*" | LETREC => "letrec"
  in  case e
        of LITERAL v => valueString v
         | VAR name => name
         | SET (x, e) => bracketSpace ["set", x, expString e]
         | IFX (e1, e2, e3) => bracketSpace ("if" :: exps [e1, e2, e3])
         | WHILEX (cond, body) => bracketSpace ["while", expString cond,
                                                                 expString body]
         | BEGIN es => bracketSpace ("begin" :: exps es)
         | APPLY (e, es) => bracketSpace (exps (e::es))
         | LETX (lk, bs, e) => bracketSpace [letkind lk, bindings bs, expString
                                                                              e]
         | LAMBDA (xs, body) => bracketSpace ("lambda" :: xs @ [expString body])
  end
      
fun whatWasExpected (LITERAL v, _) = valueString v
  | whatWasExpected (e, OK v) =
      valueString v ^ " (from evaluating " ^ expString e ^ ")"
  | whatWasExpected (e, ERROR _) = "the result of evaluating " ^ expString e
(* unit-testing utilities ((mlscheme)) 792b *)
fun testsPassed (tests, rho) =
  let fun fail strings = (app eprint strings; eprint "\n"; false)
      fun outcome e = OK (eval (e, rho)) handle _ => ERROR "evaluation failed"
      (* unit-testing utilities that depend on [[outcome]] 792c *)
      val cxfailed = "check-expect failed: "
      fun checkExpectPasses (checkx, expectx) =
        case (outcome checkx, outcome expectx)
          of (OK check, OK expect) => 
               testEqual (check, expect) orelse
               fail [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                     whatWasExpected (expectx, OK expect), ", but it's ",
                     valueString check, "."]
           | (ERROR _, tried) =>
               fail [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                     whatWasExpected (expectx, tried), ", but evaluating ",
                     expString checkx, " caused an error."]
           | (_, ERROR msg) =>
               fail  [cxfailed, " expected ", expString checkx,
                                                             " to evaluate to ",
                      whatWasExpected (expectx, ERROR msg), ", but evaluating ",
                      expString expectx, " caused an error."]
      (* type declarations for consistency checking *)
      val _ = op checkExpectPasses : exp * exp -> bool
      (* unit-testing utilities that depend on [[outcome]] 793a *)
      val cefailed = "check-error failed: "
      fun checkErrorPasses checkx =
            case outcome checkx
              of ERROR _ => true
               | OK check =>
                   fail [cefailed, " expected evaluating ", expString checkx,
                         " to cause an error, but evaluation produced ",
                         valueString check]
      (* type declarations for consistency checking *)
      val _ = op checkErrorPasses : exp -> bool
      (* definition of function [[passes]] ((mlscheme)) 793b *)
      fun passes (CHECK_EXPECT (c, e)) = checkExpectPasses (c, e)
        | passes (CHECK_ERROR c)       = checkErrorPasses  c
(* type declarations for consistency checking *)
val _ = op whatWasExpected : exp * value error -> string
(* type declarations for consistency checking *)
val _ = op outcome : exp -> value error
  in  foldr (fn (test, n) => if passes test then n + 1 else n) 0 tests
  end
(* unit-testing utilities 793c *)
fun reportTestResults (npassed, ntests) =
  case (npassed, ntests)
    of (_, 0) => ()  (* no report *)
     | (0, 1) => print "The test failed.\n"
     | (1, 1) => print "The test passed.\n"
     | (0, 2) => print "Both tests failed.\n"
     | (1, 2) => print "One of two tests passed.\n"
     | (2, 2) => print "Both tests passed.\n"
     | _ => if npassed = ntests then
               app print ["All ", Int.toString ntests, " tests passed.\n"]
            else if npassed = 0 then
               app print ["All ", Int.toString ntests, " tests failed.\n"]
            else
               app print [Int.toString npassed, " of ", Int.toString ntests,
                          " tests passed.\n"]
fun runTests (tests, rho) =
      reportTestResults (testsPassed (tests, rho), length tests)


(*****************************************************************)
(*                                                               *)
(*   INITIALIZATION                                              *)
(*                                                               *)
(*****************************************************************)

(* initialization ((mlscheme)) 316a *)
fun initialEnv () =
  let val rho =
        foldl (fn ((name, prim), rho) => bind (name, ref (PRIMITIVE prim), rho))
              emptyEnv ((* primitives [[::]] ((mlscheme)) 312b *)
                        ("+", arithOp op +  ) :: 
                        ("-", arithOp op -  ) :: 
                        ("*", arithOp op *  ) :: 
                        ("/", arithOp op div) ::
                        (* primitives [[::]] ((mlscheme)) 312d *)
                        ("<", intcompare op <) :: 
                        (">", intcompare op >) ::
                        ("=", comparison equalatoms) ::
                        ("null?",    predOp (fn (NIL   ) => true | _ => false))
                                                                              ::
                        ("boolean?", predOp (fn (BOOL _) => true | _ => false))
                                                                              ::
                        ("number?",  predOp (fn (NUM  _) => true | _ => false))
                                                                              ::
                        ("symbol?",  predOp (fn (SYM  _) => true | _ => false))
                                                                              ::
                        ("pair?",    predOp (fn (PAIR _) => true | _ => false))
                                                                              ::
                        ("procedure?",
                                     predOp (fn (PRIMITIVE _) => true | (CLOSURE
                                                    _) => true | _ => false)) ::
                        (* primitives [[::]] ((mlscheme)) 312e *)
                        ("cons", binaryOp (fn (a, b) => PAIR (a, b))) ::
                        ("car",  unaryOp  (fn (PAIR (car, _)) => car 
                                            | v => raise RuntimeError
                                                           (
                                "car applied to non-list " ^ valueString v))) ::
                        ("cdr",  unaryOp  (fn (PAIR (_, cdr)) => cdr 
                                            | v => raise RuntimeError
                                                           (
                                "cdr applied to non-list " ^ valueString v))) ::
                        (* primitives [[::]] ((mlscheme)) 313a *)
                        ("print", unaryOp (fn v => (print (valueString v^"\n");
                                                                      v)))    ::
                        ("error", unaryOp (fn v => raise RuntimeError (
                                                        valueString v))) :: nil)
      val basis  =
                (* ML representation of initial basis (generated by a script) *)

                    [ "(define caar (xs) (car (car xs)))"
                    , "(define cadr (xs) (car (cdr xs)))"
                    , "(define cdar (xs) (cdr (car xs)))"
                    , "(define list1 (x)     (cons x '()))"
                    , "(define list2 (x y)   (cons x (list1 y)))"
                    , "(define list3 (x y z) (cons x (list2 y z)))"
                    , "(define length (xs)"
                    , "  (if (null? xs) 0"
                    , "    (+ 1 (length (cdr xs)))))"
                    , "(define and (b c) (if b  c  b))"
                    , "(define or  (b c) (if b  b  c))"
                    , "(define not (b)   (if b #f #t))"
                    ,
"(define atom? (x) (or (number? x) (or (symbol? x) (or (boolean? x) (null? x)))))"
                    , "(define equal? (s1 s2)"
                    , "  (if (or (atom? s1) (atom? s2))"
                    , "    (= s1 s2)"
                    ,
             "    (and (equal? (car s1) (car s2)) (equal? (cdr s1) (cdr s2)))))"
                    , "(define append (xs ys)"
                    , "  (if (null? xs)"
                    , "     ys"
                    , "     (cons (car xs) (append (cdr xs) ys))))"
                    , "(define revapp (xs ys)"
                    , "  (if (null? xs)"
                    , "     ys"
                    , "     (revapp (cdr xs) (cons (car xs) ys))))"
                    , "(define reverse (xs) (revapp xs '()))"
                    , "(define mk-alist-pair (k a) (list2 k a))"
                    , "(define alist-pair-key        (pair)  (car  pair))"
                    , "(define alist-pair-attribute  (pair)  (cadr pair))"
                    ,
     "(define alist-first-key       (alist) (alist-pair-key       (car alist)))"
                    ,
     "(define alist-first-attribute (alist) (alist-pair-attribute (car alist)))"
                    , "(define bind (k a alist)"
                    , "  (if (null? alist)"
                    , "    (list1 (mk-alist-pair k a))"
                    , "    (if (equal? k (alist-first-key alist))"
                    , "      (cons (mk-alist-pair k a) (cdr alist))"
                    , "      (cons (car alist) (bind k a (cdr alist))))))"
                    , "(define find (k alist)"
                    , "  (if (null? alist) '()"
                    , "    (if (equal? k (alist-first-key alist))"
                    , "      (alist-first-attribute alist)"
                    , "      (find k (cdr alist)))))"
                    , "(define o (f g) (lambda (x) (f (g x))))"
                    , "(define curry   (f) (lambda (x) (lambda (y) (f x y))))"
                    , "(define uncurry (f) (lambda (x y) ((f x) y)))"
                    , "(define filter (p? xs)"
                    , "  (if (null? xs)"
                    , "    '()"
                    , "    (if (p? (car xs))"
                    , "      (cons (car xs) (filter p? (cdr xs)))"
                    , "      (filter p? (cdr xs)))))"
                    , "(define map (f xs)"
                    , "  (if (null? xs)"
                    , "    '()"
                    , "    (cons (f (car xs)) (map f (cdr xs)))))"
                    , "(define exists? (p? xs)"
                    , "  (if (null? xs)"
                    , "    #f"
                    , "    (if (p? (car xs)) "
                    , "      #t"
                    , "      (exists? p? (cdr xs)))))"
                    , "(define all? (p? xs)"
                    , "  (if (null? xs)"
                    , "    #t"
                    , "    (if (p? (car xs))"
                    , "      (all? p? (cdr xs))"
                    , "      #f)))"
                    , "(define foldr (op zero xs)"
                    , "  (if (null? xs)"
                    , "    zero"
                    , "    (op (car xs) (foldr op zero (cdr xs)))))"
                    , "(define foldl (op zero xs)"
                    , "  (if (null? xs)"
                    , "    zero"
                    , "    (foldl op (op (car xs) zero) (cdr xs))))"
                    , "(define <= (x y) (not (> x y)))"
                    , "(define >= (x y) (not (< x y)))"
                    , "(define != (x y) (not (= x y)))"
                    , "(define max (x y) (if (> x y) x y))"
                    , "(define min (x y) (if (< x y) x y))"
                    , "(define mod (m n) (- m (* n (/ m n))))"
                    , "(define gcd (m n) (if (= n 0) m (gcd n (mod m n))))"
                    , "(define lcm (m n) (if (= m 0) 0 (* m (/ n (gcd m n)))))"
                    , "(define caar  (sx) (car (car  sx)))"
                    , "(define cdar  (sx) (cdr (car  sx)))"
                    , "(define cadr  (sx) (car (cdr  sx)))"
                    , "(define cddr  (sx) (cdr (cdr  sx)))"
                    , "(define caaar (sx) (car (caar sx)))"
                    , "(define cdaar (sx) (cdr (caar sx)))"
                    , "(define caadr (sx) (car (cadr sx)))"
                    , "(define cdadr (sx) (cdr (cadr sx)))"
                    , "(define cadar (sx) (car (cdar sx)))"
                    , "(define cddar (sx) (cdr (cdar sx)))"
                    , "(define caddr (sx) (car (cddr sx)))"
                    , "(define cdddr (sx) (cdr (cddr sx)))"
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
                     ]
      val defs = schemeReader noPrompts ("initial basis", streamOfList basis)
      val repl =
        readEvalPrint runTests (schemeReader noPrompts) basisError (evaldef
                                                                         SILENT)
  in  repl (defs, rho)
  end
(* initialization ((mlscheme)) 316b *)
fun runInterpreter prompts = 
  let val rho = initialEnv ()
      val defs = schemeReader prompts ("standard input", streamOfLines
                                                                   TextIO.stdIn)
      val repl =
        readEvalPrint runTests (schemeReader noPrompts) eprintln (evaldef
                                                                        ECHOING)
  in  ignore (repl (defs, rho))
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
