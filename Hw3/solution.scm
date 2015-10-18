;
;PHillip Booth (pbooth01)
;
;
;Helper function to test set membership
;
(define member? (xs y)
        (if (null? xs)
                #f
                (if (equal? (car xs) y)
                        #t
                        (member? (cdr xs) y)
                )
        )
)

;
;Solution to Question 2 part (a)
;
;When x is an atom it returns the number of top level elements of xs that are
;equal to x 
(define count (x xs)
        (if (null? xs)
                0
                (+ (if (equal? x (car xs))
                        1
                        0    
                   )
                   (count x (cdr xs))
                )
        )
)
;check to see if it picks up the only top level 'a'
(check-expect (count 'a '(1 b a (c a))) 1) 

;
;Solution to Question 2 part (b)
;
;when x is an atom returns the number of elements of xs that are equal to
;x regardless of whether they are a top level element.
(define countall (x xs)
        (if (null? xs)
                0
                (+ (if (atom? (car xs))
                        (if (equal? x (car xs)) 
                               1
                               0
                        )
                        (countall x (car xs));     
                   )
                   (countall x (cdr xs))
                )
        )
)
;check to see if it picks up all of the a's in the list (not just top level)
(check-expect (countall 'a '(1 b a (c a))) 2)

;
;Solution to Question 2 part (c)
;
;returns a list that is recursively mirrored. Uses an accumulator
(define mirroracc (xs ys)
        (if (null? xs)
                ys
                (mirroracc 
                        (cdr xs) 
                        (if (atom? (car xs))
                               (cons (car xs) ys)
                               (cons (mirroracc (car xs) '()) ys)
                        )
                )
        )
)
;Passes the original list and an empty list as an accumulator to mirroracc
(define mirror (xs) 
        (mirroracc xs '())
)
;checks to make sure elements get mirroed in the top and lower levels of the
;list and makes sure that the elements of inner lists stay together 
(check-expect (mirror '((a (b 5)) (c d) e)) '(e (d c) ((5 b) a)))

;
;Solution to Question 2 part (d)
;
;erases all internal parentheses
(define flattenacc (xs ys)
        (if (null? xs)
                ys
                (flattenacc 
                        (cdr xs)
                        (if (atom? (car xs))
                                (append (list1 (car xs)) ys)
                                (append (flattenacc (car xs) '()) ys)
                        )
                )
        )
)
;calls flattenacc with the list that gets passed in and an empty list that get
;used as an accululator. The accumulator value gets reversed on return
(define flatten (xs)
        (reverse (flattenacc xs '()))
)
;Makes sure all internal parenthesis get removed
(check-expect (flatten '((a b) ((c d) e))) '(a b c d e))

;
;Solution to Question 2 part (e)
;
;checks to make sure a contiguous sublist exists 
(define check-next (xsm xlg)
        (if (null? xsm)
                #t
                (if (null? xlg)
                        #f
                        (if (equal? (car xsm) (car xlg))
                                (check-next (cdr xsm) (cdr xlg))
                                #f
                        )
                )
        )        
)
;recursivly goes through the list to find where a sublist starts and then calls 
;check-next with the first list and the second list with all of the elements
;up to where the sublist starts removed.
(define contig-sublist? (xsm xlg)
        (if (null? xlg)
                #f
                (if (equal? (car xsm) (car xlg))
                        (check-next xsm xlg)
                        (contig-sublist? xsm (cdr xlg))
                )
        ) 
)

;CHecks to see if the function returns true only if the sbublist is contoguous
;not just inside the larger list
(check-expect (contig-sublist? '(a b c) '(x a y b z c)) #f)
(check-expect (contig-sublist? '(a y b) '(x a y b z c)) #t)

;
;Solution to Question 2 part (f)
;
;checks to see if every element in the first list exists in the second list
;returns true if true and false otherwise
(define sublist? (xsm xlg)
        (if (null? xsm)
                #t
                (if (null? xlg)
                        #f
                        (if (equal? (car xsm) (car xlg))
                                (sublist? (cdr xsm) (cdr xlg))
                                (sublist?  xsm (cdr xlg))
                        )
                )
        )
       
)
;check to make sure the function returns true and false when appropriate
(check-expect (sublist? '(a y b) '(x a y b z c)) #t)
(check-expect (sublist? '(a u c) '(x a y b z c)) #f)

;
;Solution to PRoblem 6
;

(define even? (x) (= (mod x 2) 0))
;function iterates through every element in a list and applies a function
;to them and returns a list containing the largest prefix of elements that caused that 
;function to evaluate to true
(define takewhileacc (func lst acc)
        (if (null? lst)
                acc
                (if (func (car lst))
                      (cons (car lst) (takewhileacc func (cdr lst) acc))
                       acc
                )
        )
)
;takes in a function and list and passes them both along with an empty list
;as an accumulator to the function takewhileacc
(define takewhile (func lst)
        (takewhileacc func lst '())
)
;making sure the function returned the longest prefix
(check-expect (takewhile even? '(2 4 6 7 8 10 12)) '(2 4 6))

;function iterates through eery element in the list, applying
;the function to them and removes that largest prefix of elements
;that made the function evaluate to true
(define dropwhile (func lst)
        (if (null? lst)
                lst
                (if (func (car lst))
                        (dropwhile func (cdr lst))
                        lst 
                )
        )
)
;making sure the function removed the largest prefix
(check-expect (dropwhile even? '(2 4 6 7 8 10 12)) '(7 8 10 12))

;
;Solution to Problem 15
;
;Two local variables (* +) are introduced using the let binding.
;They are bound and used to stand for the primitive variables (+ *),
;Which are then used as the value for the function (vector-length)


;
;Solution to Problem A
;
;retuens a list containgin the first n elements of the list
(define takeacc (n lst acc)
        (if (null? lst)
             acc
             (if (> n 0)
                (cons (car lst) (takeacc (- n 1) (cdr lst) acc))
                 acc
             )
        )
)
;checks to make sure that n is greater than 0 and passes n and an empty
;list to takeacc
(define take (n lst)
        (if (> n 0)
                (takeacc n lst '())
                '()
        )
)
;checks to make sure that the correct sized list gets returned
(check-expect (take 3 '(2 4 6 7 8 10 12)) '(2 4 6))

;removes the first n elements of the list
(define dropacc (n lst)
        (if (null? lst)
                lst
                (if (> n 0)
                        (dropacc (- n 1) (cdr lst))
                         lst
                )
        )
)
;checks to make sure n is greater than 0
(define drop (n lst)
        (if (> n 0)
                (dropacc n lst)
                '()
        )
)
;checks to make sure the correct sized list gets returned
(check-expect (drop 3 '(2 4 6 7 8 10 12)) '(7 8 10 12))

;
;solution to problem B
;
;tkaes in 3 lists. Each iteration it takes the car of each list and cons that
;into a list that gets stored in the accumulator that gets returned at the base
;case
(define zipacc (lst1 lst2 acc)
        (if (<= (length lst1) 0)
                acc
          (zipacc (cdr lst1) (cdr lst2) (cons (cons (car lst1) (list1(car lst2))) acc))
        )
)
;takes in two lists and passes them along with an empty list to zipacc. Reverses
;the result
(define zip (lst1 lst2)
        (reverse (zipacc lst1 lst2 '()))                
)
;check to make sure zip works
(check-expect (zip '(1 2 3) '(a b c)) '((1 a) (2 b) (3 c)))

;takes in 4 lists. Each list inside of the main list is split
;so the first element goes into acc1 and the second element 
;goes into acc2 anf then the lists are combinesd and returned 
;using acc3
(define unzipacc (lst1 acc1 acc2 acc3)
        (if (<= (length lst1) 0)
               (cons acc1 (cons acc2 acc3))
    (unzipacc (cdr lst1) (append (list1(caar lst1)) acc1) (append (list1(cadar lst1)) acc2) acc3)
        )
)
;takes in one list passes that along with two accumulator lists to unzipacc.
;Mirrors and reverses the result
(define unzip (lst1)
        (reverse (mirror (unzipacc lst1 '() '() '())))
)
;check to make sure unzip is working
(check-expect (unzip '((1 a) (2 b) (3 c))) '((1 2 3) (a b c)))


;
;Solution to problem c
;
;takes in a func and two lists and returns the eleman that causes
;the ufunction to evaluate to the highest value
(define arg-max-acc (func lst acc)
        (if (null? acc)
                (arg-max-acc func (cdr lst) (cons (car lst) acc))
                (if (null? lst) 
                        (car acc)
                        (if (> (func (car lst)) (func (car acc)))
                                (arg-max-acc func (cdr lst) (cons (car lst) (cdr acc)))
                                (arg-max-acc func (cdr lst) acc)
                        )
                )
        )
) 
;takes in a func and a list and passes them and an empty list
;to arg-max-acc
(define arg-max (func lst)
        (arg-max-acc func lst '())
)
;function used to test arg-max
(define square (a) (* a a))
;check to make sure arg-max works
(check-expect (arg-max square '(1 2 3 4 5)) 5)
;
;;Solution to problem e
;takes in two lists and merges them into one list and returns this
;list using an accumulator
(define mergeacc (lst1 lst2 acc)
        (if (null? lst1)
                (append acc lst2)
                (if (null? lst2)
                        (append acc lst1)
                        (if (< (car lst1) (car lst2))
                              (mergeacc (cdr lst1) lst2 (append  acc (list1(car lst1))))
                              (mergeacc lst1 (cdr lst2) (append  acc (list1(car lst2))))
                        )
                )
        )
)
;takes in two lists and passes them along with an empty list to megeacc
(define merge (lst1 lst2)
        (mergeacc lst1 lst2 '())       
)
;checks to make sure that merge is working correctly
(check-expect (merge '(1 2 3) '(4 5 6)) '(1 2 3 4 5 6))
(check-expect (merge '(4 6 7) '(2 5 8)) '(2 4 5 6 7 8))

;takes in 3 lists and alternates putting the value at the head of the 
;each list into a third accumulator list. returns accumulator list
(define interleaveacc (lst1 lst2 acc)
        (if (null? lst1)
                (append acc lst2)
                (if (null? lst2)
                        (append acc lst1)
                        (interleaveacc lst2 (cdr lst1) (append acc (list1(car lst1))))
                )
        )
)
;takes in two lists and passes them, along with a third empty list to
;;interleaveacc
(define interleave (lst1 lst2) 
        (interleaveacc lst1 lst2 '())
)
;check to make sure that interleave is working correctly
(check-expect (interleave '(1 2 3) '(4 5 6)) '(1 4 2 5 3 6))
;
;returns a list of the inividual nodes that are connected to the node
;of name key
;
;Solution to PRoblem D
(define get-individ-nodes (key inputlst acc)
        (if (null? inputlst)
                acc
                (get-individ-nodes 
                        key 
                        (cdr inputlst)
                        (if (equal? key (caar inputlst))
                                (append acc (list1(cadar inputlst)))
                                acc
                        )
                )
        )
)
;Retunrs a list of lists, each containing a distinct node in the original
;inputlist
(define find-distinctacc (lst1 acc)
        (if (null? lst1)
                acc
                (find-distinctacc
                        (cdr lst1) 
                        (if (member? acc (list1(car lst1)))
                                acc
                                (append acc (list1(list1(car lst1))))
                                
                        )
                
                )
        )
)
;Reurns an acculuator which contains lists of each distinct node with 
;the nodes its connected to
(define collect-connected-nodes (totlst inputlst acc)
        (if (null? totlst)
                acc
                (collect-connected-nodes 
                        (cdr totlst)
                        inputlst
                        (append acc (list1(append (car totlst) (list1(get-individ-nodes (caar totlst) inputlst '())))))
                )
        )
)
;takes in a list of atoms and returns a list of lists containing the distinct
;atoms
(define find-distinct (lst)
        (find-distinctacc lst '())
)
;takes in a list in edge-list representation and returns the same list in 
;successor map representation
(define successors-map-of-edge-list (lst1)
        (collect-connected-nodes (find-distinct (flatten lst1)) lst1 '())       
)
(check-expect (successors-map-of-edge-list '((a b) (b c) (a c))) '((a (b c)) (b (c)) (c ())))
;;retunrs a list containing all of the possible combinations of the given
;;edges for a specific node
(define edges (lst1 lst2 acc) 
        (if (null? lst2)
                acc
                (edges lst1 (cdr lst2) (cons (append (list1(car lst1)) (list1(car lst2))) acc))
        )
)
;;creates and retunrs alist containing all of the edges in pairs
(define make-edge-list (lst acc)
        (if (null? lst)
                acc
                (make-edge-list (cdr lst) (append (edges (list1 (caar lst)) (cadar lst) '()) acc))
        )       
)
;;takes in a list in successor map representation and returns the same list in 
;;edgelist representation
(define edge-list-of-successors-map (lst)
        (make-edge-list lst '())
)
;Check to make sure that edge-list-of-successors-map was wprking correctly
(check-expect (edge-list-of-successors-map '((a (b c)) (b (c)) (c ()))) '((b c) (a c) (a b)))
