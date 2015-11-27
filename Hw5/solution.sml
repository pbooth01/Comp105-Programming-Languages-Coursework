(* Phillip Booth Tufts Comp105 assignment 5*)

(* Question A  Part 1: Compound: recursively applies a binary operator f to a var x N times *)

fun compound (op f, N, x) = 
	if N>=1 
		then op f(x, compound(op f, N-1, x)) 
	else x;

(* Question A Part 2: exp: Using compound with multiplication, exp calculates x^y *)
fun exp x y = 
	if y=0 
		then 1 
	else compound(op *, y-1, x);


(* Question B Part 1: myrev reverses the order of a list*)

fun myrev nil = nil
	| myrev x = foldl op:: nil x;

(* Question B Part 2: minlist returns the smallest value in an int list*)

fun mincheck (x, y) =
	if x < y
		then x
	else y;

fun minlist nil = raise Empty
	| minlist (x::xs) = foldl mincheck x xs;

(* Question C Part 1: myfoldl recursive implementation of the foldl function*)

fun myfoldl f x nil = x
	| myfoldl f x (y::ys) = myfoldl f (f(y,x)) ys; 

(* Question C Part 2: myfoldr recursive implementation of the foldr function*)

fun myfoldr f x nil = x
	| myfoldr f x (y::ys) = f(y, (myfoldr f x ys)); 

(* Question D flatten takes a list of lists and produces a single list containing
all the elements in the correct order*)

fun listacc (xs, ys) = xs@ys;

fun flatten nil = nil
	| flatten xs = foldr listacc nil xs; 


(* Question E mynth returns the nth value in a list*)

fun mynth x (y::nil) = if x = 0 then y else raise Domain
	| mynth x [] = raise Empty
	| mynth x (y::ys) = if x < 0 then raise Domain
						else if x = 0 then y 
						else mynth (x - 1) ys; 

(* Question F pairfoldr applies a three-argument function to a pair of lists of equal length, using the same order as foldr*)

fun pairfoldr f z (nil, nil) = z
	| pairfoldr f z (x::xs, y::ys) = f(x,y, pairfoldr f z (xs, ys))
	| pairfoldr _ _ _ = raise Match;