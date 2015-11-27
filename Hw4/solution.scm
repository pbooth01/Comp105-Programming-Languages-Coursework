;;
;;Part A
;;
(define f-imperative (y)
	(letrec(
		(x e)
		(recurse (lambda (p x y) 
					(if (not (p x y))
						(h x y)
						(recurse p (g x y) y)
					)
				)
		)
	)(recurse p x y))
)

;;
;;Problem 10 b solution
;;

;; Function takes in two interger and return the largest value of the two
(define max (a b)
	(if (> a b)
		a
		b
	)
)

;; Function takes in a list and returns the largest value in the list
(define max* (xs)
	(foldl max 0 xs)
)

;;check to make sure that the correct value of 8 is retured from the list
(check-expect (max* '(1 3 4 7 6 8)) 8)
;;check to make sure that the correct value of 10 is reruned from the list even though
;;there are repeats
(check-expect (max* '(1 3 10 7 10 8)) 10)

;;Problem 10 c solution
;; Function takes in a list and returns the greatest common denonminator of every value in the list
(define gcd* (xs)
	(foldl gcd (car xs) xs)
)
;;checks to make sure that the gcd value is returned
(check-expect (gcd* '(6 48 2 10 8 12)) 2)
(check-expect (gcd* '(7 5 23 7 9)) 1)

;;Problem 10 d
;; Function takes in a list and returns the greatest common denonminator of every value in the list
(define lcm* (xs)
	(foldl lcm (car xs) xs)
)
;;checks to make sure that the lcm value is returned
(check-expect (lcm* '(1 2 3 4)) 12)
(check-expect (lcm* '(6 8)) 24)

;;Problem 10 e
;; Function takes in a list and returns the greatest common denonminator of every value in the list
(define sum (xs)
	(foldl + 0 xs)
)
;;checks to make sure that the sum is returned
(check-expect (sum '(1 2 3 4)) 10)
(check-expect (sum '(6 8)) 14)

;;Problem 10 f
;; Function takes in a list and returns the greatest common denonminator of every value in the list
(define product (xs)
	(foldl * 1 xs)
)
;;checks to make sure that the product is returned
(check-expect (product '(1 2 3 4)) 24)
(check-expect (product '(6 8)) 48)

;;Problem 10 g
;;Function takes in 

;;Function takes in a list and returns the greatest common denonminator of every value in the list
(define append (xs ys)
	(foldr cons ys xs)
)
;;checks to make sure that the sum is returned
(check-expect (append '(1) '(2)) '(1 2))

;;Problem 10 i 

;;Function takes in a list and returns that list in reverse
(define reverse (xs)
	(foldl cons '() xs)
)
;;checks to make sure that the sum is returned
(check-expect (reverse '(1 2)) '(2 1))
(check-expect (reverse '(3 '(1 2))) '('(1 2) 3))

;;Problem 10 j
;;Function takes in an integer and a list and inserts the
;;integer in the right place in the list

(define insert (x xs)
	(if (null? xs)
		(list1 x)
		(if (< x (car xs))
			(cons x xs)
			(cons (car xs) (insert x (cdr xs)))
		)
	)
) 

;;Function takes in a list and returns that list sorted from highest to lowest
(define insertion-sort (xs)
	(foldl insert '() xs)
)
;;checks to make sure that the sorted list is returned
(check-expect (insertion-sort '(8 3 9 1 4 2)) '(1 2 3 4 8 9))

;;Problem 11 Solution
;;Function takes in an integer and a list and inserts the
;;integer in the right place in the list

;;LENGTH FUNCTION
;;evaluates a lambda to produce a closure
(val count (lambda (x)
				(lambda (y z) (set x (+ x 1))))
)
;;sets the closure value to 0
(val count+1 (count 0))

;;Function takes in a list and returns the length
(define length (xs)
	(foldl count+1 0 xs)
)

;;checks to make sure that the correct length of the sorted list is returned
(check-expect (length '(8 3 9 1 4 2)) 6)

;;Filter FUNCTION
;;Function takes in an x and return true if its even and false if its odd
(define even? (x)
	(if (= 0 (mod x 2))
		#t
		#f
	)
)
;;evaluates a lambda to produce a closure
(val closure (lambda (f)
				(lambda (y z) 
					(if (f y)
						(cons y z)
						(append z '())
					)
				)
					
			)
)
;;Function takes in a function and a list and returns a list of values from the list that evaluate to true
;;When the function is applied to them
(define filter (f xs)
	(foldr (closure f) '() xs)
)
;;checks to make sure that the correct length of the sorted list is returned
(check-expect (filter even? '(1 2 3 4)) '(2 4))

;;Problem 16 Solution
;;add-element

;;Takes in an int and returns true if even and false if odd
(define even? (x)
	(if (= 0 (mod x 2))
		#t
		#f
	)
)
;;Takes in an int and returns true if less than 0 and false if greater than or equal to 0
(define negative? (x)
		(if (< x 0)
			#t
			#f
		)
)
;;Add-Element
;;Function takes in an element and a set and returns a function that represents the set of values containing x
;;the original set
(define add-element (x s1 userFun)
	(if (s1 x) 
        s1               
        (lambda (y) (or (s1 y) (userFun x y)))
    )
	
)
;;Union
;;Function takes in two sets and returns a function that is a set that contains both sets
(define union (s1 s2)
	(lambda (y) (or (s1 y) (s2 y)))
)
;;Inter
;;Function takes in two sets and returns a function that is a set of elements in both sets
(define inter (s1 s2)
	(lambda (y) (and (s1 y) (s2 y)))
)

;;Diff
;;Function takes in two sets and returns a function that is a set that contains elements in the first set but not in the second
(define diff (s1 s2)
	(lambda (y) (and (s1 y) (not (s2 y)))) 
)
;;takes in an x and returns false
(val emptyset (lambda (x) #f))

;;takes in an elelemnt and a set and returns true if the element is a member of the set and false otherwise
(define member? (x s)
  	(exists? ((curry =) x) s)
)

;;Part B
(val mk-set-ops
	(lambda (userFun)
		(list6
			(lambda (x) (emptyset x))
			(lambda (x s1) (member? x s1))
			(lambda (x s1 userFun) (add-element x s1 userFun)) 
			(lambda (s1 s2) (union s1 s2)) 
			(lambda (s1 s2) (inter s1 s2))
			(lambda (s1 s2) (diff s1 s2)) 
		)
	) 
)

;;
;;Problem 23
;;

;;23 Doesnt Work.. Please be gentle!!! Thanks

(define make-formula-true (formula fail succeed)
	(letrec(
				;;Function checks to make sure the an atom does not already exist in cur, and if it
				;;does then it checks to make sure that its bool assignment is the same. If not then
				;;it fails
				(check-valid-atom (lambda (formula bool cur fail succeed)
										(if (null? (find formula cur))
											(succeed (bind formula bool cur) fail)
											(if (equal? (find formula cur) bool)
												(succeed (bind formula bool cur) fail)
												fail
											)
										)
								  )
				)
				;;Functinon is continuously called with (formula bool cur fail succeed) and determins what
				;;The right course of action is based on the car of the formula being passed in
				(make-formula (lambda (formula bool cur fail succeed)
					 		   	(if (atom? formula)
					 		   		(check-valid-atom formula bool cur fail succeed)
					 		   		(if (equal? 'not (car formula)) 
					 					(make-formula (cadr formula) (not bool) '() fail succeed)
										(if (equal? 'and (car formula))
											(make-all (cdr formula) bool cur fail succeed)
											(if (equal? 'or (car formula))
												(make-any (cdr formula) bool cur fail succeed)
												(print 0)
											)
										)
					 				)
					 		   	)
					 		  )
				)
				(make-all (lambda (formulas bool cur fail succeed)
								(if (null? formulas)
									(succeed cur fail)
									(make-formula (car formulas) bool cur 
										fail ;;Failure
										(lambda (cur resume) (make-all (cdr formulas) bool cur resume succeed));;Success
									)
								)
						  )
				
				)
				;;(make-any (lambda (formulas bool cur fail succeed) ))
			)
		(make-formula formula #t '() fail succeed)
	) 
)

;;Wrapper around the make-formula-true function that was used for testinf purposes
(define one-solution (formula)
	(make-formula-true formula (lambda () 'no-solution) (lambda (cur resume) cur))
)
(val x '((and x y) (and (not x) (not y))))
;;(val y '(not y))
(one-solution x)

