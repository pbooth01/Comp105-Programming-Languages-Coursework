(val e1 ((@ cons int) #t '(#t #f)))
; type error

(val e2 (@ '() int))
; (list int)

(val e3 (type-lambda ('a) (lambda (('a x)) x)))
; (forall ('a) ('a -> 'a))

(val e4 (+ 1 '()))
; type error

(val e5 (lambda ((int x)) (cons x x)))
; type error