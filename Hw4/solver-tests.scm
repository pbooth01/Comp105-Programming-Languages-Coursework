;;This tests to make sure that the make-all function is working correctly
;;There is no solution to this problem
(val f1 '(and (and x y) (and (not x) (not y)))) 
(val s1 'no-solution)

;;Funciton will require backtracking so it will test if that functionality works
(val f2 '(and (or x y z) (and (not x) (not y)))) 
(val s2 '((x #f) (y #f) (z #t)))

;;Funtion tests to make sure values that dont matter arent included
(val f3 '(and (or x y z) (or (not x) (not y) (not z))))
(val s3 '((x #t) (y #f)))