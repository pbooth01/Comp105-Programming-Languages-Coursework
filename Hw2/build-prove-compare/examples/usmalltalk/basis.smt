;;;usm.nw:4147
(class Boolean Object ()
    (method ifTrue:ifFalse: (trueBlock falseBlock) (subclassResponsibility self))
  
    (method ifFalse:ifTrue: (falseBlock trueBlock) 
        (ifTrue:ifFalse: self trueBlock falseBlock))
    (method ifTrue:  (trueBlock)  (ifTrue:ifFalse: self trueBlock []))
    (method ifFalse: (falseBlock) (ifTrue:ifFalse: self [] falseBlock))
    
    (method not ()          (ifTrue:ifFalse: self [false]          [true]))
    (method eqv: (aBoolean) (ifTrue:ifFalse: self [aBoolean]       [(not aBoolean)]))
    (method xor: (aBoolean) (ifTrue:ifFalse: self [(not aBoolean)] [aBoolean]))

    (method & (aBoolean) (ifTrue:ifFalse: self [aBoolean] [self]))
    (method | (aBoolean) (ifTrue:ifFalse: self [self] [aBoolean]))
  
    (method and: (alternativeBlock) (ifTrue:ifFalse: self alternativeBlock [self]))
    (method or:  (alternativeBlock) (ifTrue:ifFalse: self [self] alternativeBlock))

    (method if (trueBlock falseBlock) (ifTrue:ifFalse: self trueBlock falseBlock))
)
;;;usm.nw:4179
(class True Boolean ()
  (method ifTrue:ifFalse: (trueBlock falseBlock) (value trueBlock))
)
(class False Boolean ()
  (method ifTrue:ifFalse: (trueBlock falseBlock) (value falseBlock))
)
;;;usm.nw:4342
(class Block Object 
    () ; internal representation
    (method value primitive value)
    (method whileTrue: (body)
        (ifTrue:ifFalse: (value self)
            [(value body)
             (whileTrue: self body)]
            [nil]))
    (method while (body) (whileTrue: self body))
    (method whileFalse: (body) 
         (ifTrue:ifFalse: (value self)
             [nil]            
             [(value body) 
              (whileFalse: self body)]))
)
;;;usm.nw:7484
(class Symbol Object
    () ; internal representation
    (class-method new () (error: self #can't-send-new-to-Symbol))
    (class-method new:   primitive newSymbol)
    (method       print  primitive printSymbol)
)
;;;usm.nw:5208
(class Magnitude Object 
    () ; abstract class
    (method =  (x) (subclassResponsibility self)) ; may not inherit from Object
    (method <  (x) (subclassResponsibility self))
    (method >  (y) (< y self))
    (method <= (x) (not (> self x)))
    (method >= (x) (not (< self x)))
    (method min: (aMagnitude) (if (< self aMagnitude) [self] [aMagnitude]))
    (method max: (aMagnitude) (if (> self aMagnitude) [self] [aMagnitude]))
)
;;;usm.nw:5224
(class Number Magnitude
    ()  ; abstract class
    ;;;;;;; basic Number protocol
    (method +   (aNumber)     (subclassResponsibility self))
    (method *   (aNumber)     (subclassResponsibility self))
    (method negated    ()     (subclassResponsibility self))
    (method reciprocal ()     (subclassResponsibility self))
    
    (method asInteger  ()     (subclassResponsibility self))
    (method asFraction ()     (subclassResponsibility self))
    (method asFloat    ()     (subclassResponsibility self))
    (method coerce: (aNumber) (subclassResponsibility self))
    
;;;usm.nw:5245
(method -   (y) (+ self (negated  y)))
(method abs ()  (if (negative self) [(negated  self)] [self]))
(method /   (y) (* self (reciprocal y)))
;;;usm.nw:5254
(method negative         () (<  self (coerce: self 0)))
(method positive         () (>= self (coerce: self 0)))
(method strictlyPositive () (>  self (coerce: self 0)))
;;;usm.nw:5263
(method squared () (* self self))
(method raisedToInteger: (anInteger)
    (if (= anInteger 0)
        [(coerce: self 1)]
        [(if (= anInteger 1) [self]
            [(* (squared (raisedToInteger: self (div: anInteger 2)))
                (raisedToInteger: self (mod: anInteger 2)))])]))
;;;usm.nw:5290
(method sqrt () (sqrtWithin: self (coerce: self (/ 1 10))))
(method sqrtWithin: (epsilon) (locals two x_{i-1} x_{i})
    ; find square root of receiver within epsilon
    (set two     (coerce: self 2))
    (set x_{i-1} (coerce: self 1))
    (set x_{i}   (/ (+ x_{i-1} (/ self x_{i-1})) two))
    (while [(> (abs (- x_{i-1} x_{i})) epsilon)]
           [(set x_{i-1} x_{i})
            (set x_{i} (/ (+ x_{i-1} (/ self x_{i-1})) two))])
    x_{i})
;;;usm.nw:5237
)
;;;usm.nw:5624
(class Integer Number
    () ; abstract class
    (method div: (n) (subclassResponsibility self))
    (method mod: (n) (- self (* n (div: self n))))
    (method gcd: (n) (if (= n (coerce: self 0)) [self] [(gcd: n (mod: self n))]))
    (method lcm: (n) (* self (div: n (gcd: self n))))
    
;;;usm.nw:5649
(method asFraction () (num:den:  Fraction self 1))
(method asFloat    () (mant:exp: Float    self 0))
(method asInteger  () self)
;;;usm.nw:5658
(method coerce: (aNumber) (asInteger aNumber))
;;;usm.nw:5662
(method reciprocal () (num:den: Fraction 1 self)) 
(method / (aNumber) (/ (asFraction self) aNumber))
;;;usm.nw:5668
(method timesRepeat: (aBlock) (locals count)
    (ifTrue: (negative self) [(error: self #negative-repeat-count)])
    (set count self)
    (while [(!= count 0)]
         [(value aBlock)
          (set count (- count 1))]))
;;;usm.nw:5631
)
;;;usm.nw:5695
(class SmallInteger Integer
    () ; primitive representation
    (class-method new: primitive newSmallInteger:)
    (class-method new   () (new: self 0))
    (method negated     () (- 0 self))
    (method print primitive printSmallInteger)
    (method +     primitive +)
    (method -     primitive -)
    (method *     primitive *)
    (method div:  primitive div)
    (method =     primitive eqObject)
    (method <     primitive <)
    (method >     primitive >)
)
;;;usm.nw:5729
(class Fraction Number
    (num den)
    (class-method num:den: (a b) (initNum:den: (new self) a b))
    (method initNum:den: (a b) ; private
        (setNum:den: self a b)
        (signReduce self)
        (divReduce self))
    (method setNum:den: (a b) (set num a) (set den b) self) ; private
    
;;;usm.nw:5768
(method signReduce () ; private
    (ifTrue: (negative den)
        [(set num (negated num)) (set den (negated den))])
    self)

(method divReduce () (locals temp) ; private
    (if (= num 0)
        [(set den 1)]
        [(set temp (gcd: (abs num) den))
         (set num  (div: num temp))
         (set den  (div: den temp))])
    self)
;;;usm.nw:5790
(method num () num)
(method den () den)
;;;usm.nw:5803
(method = (f)
    (and: (= num (num f)) [(= den (den f))]))
(method < (f)
    (< (* num (den f)) (* (num f) den)))
;;;usm.nw:5812
(method negated () (setNum:den: (new Fraction) (negated num) den))
;;;usm.nw:5825
(method * (f)
    (divReduce
        (setNum:den: (new Fraction)
                        (* num (num f))
                        (* den (den f)))))
;;;usm.nw:5844
(method + (f) (locals temp)
    (set temp (lcm: den (den f)))
    (divReduce
        (setNum:den: (new Fraction)
                     (+ (* num (div: temp den)) (* (num f) (div: temp (den f))))
                     temp)))
;;;usm.nw:5859
(method reciprocal ()
   (signReduce (setNum:den: (new Fraction) den num)))
;;;usm.nw:5864
(method print () (print num) (print #/) (print den) self)
;;;usm.nw:5874
(method asInteger  () (div: num den))
(method asFloat    () (/ (asFloat num) (asFloat den)))
(method asFraction () self)
(method coerce: (aNumber) (asFraction aNumber))
;;;usm.nw:5884
(method negative         () (negative num))
(method positive         () (positive num))
(method strictlyPositive () (strictlyPositive num))
;;;usm.nw:5738
)
;;;usm.nw:5920
(class Float Number
    (mant exp)
    (class-method mant:exp: (m e) (initMant:exp: (new self) m e))
    (method initMant:exp: (m e) ; private
        (set mant m) (set exp e) (normalize self))
    (method normalize ()    ; private
        (while [(> (abs mant) 32767)]
               [(set mant (div: mant 10))
                (set exp (+ exp 1))])
        self)
    
;;;usm.nw:5936
(method mant () mant)  ; private
(method exp  () exp)   ; private
;;;usm.nw:5944
(method < (x) (negative (- self x)))
(method = (x) (isZero   (- self x)))
(method isZero () (= mant 0))
;;;usm.nw:5951
(method negated () (mant:exp: Float (negated mant) exp))
;;;usm.nw:5975
(method + (prime) 
    (if (>= exp (exp prime))
        [(mant:exp: Float (+ (* mant (raisedToInteger: 10 (- exp (exp prime))))
                             (mant prime))
                          (exp prime))]
        [(+ prime self)]))
;;;usm.nw:5988
(method * (prime) 
    (mant:exp: Float (* mant (mant prime)) (+ exp (exp prime))))
;;;usm.nw:5997
(method reciprocal ()
    (mant:exp: Float (div: 1000000000 mant) (- -9 exp)))
;;;usm.nw:6003
(method coerce: (aNumber) (asFloat aNumber))
(method asFloat () self)
;;;usm.nw:6009
(method asInteger ()
    (if (< exp 0)
        [(div: mant (raisedToInteger: 10 (negated exp)))]
        [(*    mant (raisedToInteger: 10 exp))]))
;;;usm.nw:6017
(method asFraction ()
    (if (< exp 0)
        [(num:den: Fraction mant (raisedToInteger: 10 (negated exp)))]
        [(num:den: Fraction (* mant (raisedToInteger: 10 exp)) 1)]))
;;;usm.nw:6038
(method negative         () (negative mant))
(method positive         () (positive mant))
(method strictlyPositive () (strictlyPositive mant))
;;;usm.nw:6053
(method print () 
    (print-normalize self) 
    (print mant) (print #x10^) (print exp)
    (normalize self))

(method print-normalize ()
    (while [(and: (< exp 0) [(= (mod: mant 10) 0)])]
        [(set exp (+ exp 1)) (set mant (div: mant 10))]))
;;;usm.nw:5931
)
;;;usm.nw:4403
(class Collection Object
  () ; abstract
  (method do:     (aBlock)    (subclassResponsibility self))
  (method add:    (newObject) (subclassResponsibility self))
  (method remove:ifAbsent: (oldObject exnBlock)
                              (subclassResponsibility self))
  (method species ()          (subclassResponsibility self))
  
;;;usm.nw:4415
(class-method with: (anObject) (locals temp)
    (set temp (new self))
    (add: temp anObject)
    temp)
;;;usm.nw:4432
(method remove: (oldObject) 
    (remove:ifAbsent: self oldObject [(error: self #tried-to-remove-absent-object)]))
(method addAll: (aCollection) 
    (do: aCollection (block (x) (add: self x)))
    aCollection)
(method removeAll: (aCollection) 
    (do: aCollection (block (x) (remove: self x)))
    aCollection)
;;;usm.nw:4456
(method isEmpty () (= (size self) 0))
(method size () (locals temp)
    (set temp 0)
    (do: self (block (_) (set temp (+ temp 1))))
    temp)
(method occurrencesOf: (anObject) (locals temp)
    (set temp 0)
    (do: self (block (x)
       (ifTrue: (= x anObject) [(set temp (+ temp 1))])))
    temp)
(method includes: (anObject) (< 0 (occurrencesOf: self anObject)))
(method detect: (aBlock) 
    (detect:ifNone: self aBlock [(error: self #no-object-detected)]))
(method detect:ifNone: (aBlock exnBlock) (locals answer searching)
    (set searching true)
    (do: self (block (x)
        (ifTrue: (and: searching [(value aBlock x)])
             [(set searching false)
              (set answer x)])))
    (if searching exnBlock [answer]))
;;;usm.nw:4504
(method inject:into: (thisValue binaryBlock)
   (do: self (block (x) (set thisValue (value binaryBlock x thisValue))))
   thisValue)
;;;usm.nw:4515
(method select: (aBlock) (locals temp)
   (set temp (new (species self)))
   (do: self (block (x) (ifTrue: (value aBlock x) [(add: temp x)])))
   temp)
(method reject: (aBlock)
   (select: self (block (x) (not (value aBlock x)))))
(method collect: (aBlock) (locals temp)
   (set temp (new (species self)))
   (do: self (block (x) (add: temp (value aBlock x))))
   temp)
;;;usm.nw:4528
(method asSet () (locals temp)
     (set temp (new Set))
     (do: self (block (x) (add: temp x)))
     temp)
;;;usm.nw:4537
(method print ()
    (printName self)
    (print lparen)
    (do: self (block (x) (print space) (print x)))
    (print space)
    (print rparen)
    self)
(method printName () (print #Collection))
;;;usm.nw:4411
)
;;;usm.nw:4565
(class Set Collection
    (members)  ; list of elements
    (class-method new () (initSet (new super)))
    (method initSet   () (set members (new List)) self) ; private
    (method do: (aBlock) (do: members aBlock))
    (method remove:ifAbsent: (item exnBlock) 
        (remove:ifAbsent: members item exnBlock))
    (method add: (item)
        (ifFalse: (includes: members item) [(add: members item)])
        item)
    (method species   () Set)
    (method printName () (print #Set))
    (method asSet     () self)
)
;;;usm.nw:4622
(class KeyedCollection Collection
    ()  ; abstract class
    (method at:put: (key value)       (subclassResponsibility self))
    (method associationsDo: (aBlock)  (subclassResponsibility self))
    
;;;usm.nw:4649
(method do: (aBlock) 
    (associationsDo: self (block (x) (value aBlock (value x)))))
;;;usm.nw:4656
(method at: (key)    
    (at:ifAbsent: self key [(error: self #key-not-found)]))
(method at:ifAbsent: (key exnBlock) 
    (value (associationAt:ifAbsent: self key 
               [(key:value: Association nil (value exnBlock))])))
(method includesKey: (key) 
    (isKindOf: (associationAt:ifAbsent: self key []) Association))
(method associationAt: (key) 
    (associationAt:ifAbsent: self key [(error: self #key-not-found)]))
(method associationAt:ifAbsent: (key exnBlock) (locals finishBlock)
    (set finishBlock exnBlock)
    (associationsDo: self (block (x) 
        (ifTrue: (= (key x) key) [(set finishBlock [x])])))
    (value finishBlock))
;;;usm.nw:4676
(method keyAtValue: (value) 
    (keyAtValue:ifAbsent: self value [(error: self #value-not-found)]))
(method keyAtValue:ifAbsent: (value aBlock) (locals finishBlock)
    (set finishBlock aBlock)
    (associationsDo: self (block (x) 
        (ifTrue: (= (value x) value) [(set finishBlock [(key x)])])))
    (value finishBlock))
;;;usm.nw:4627
)
;;;usm.nw:4633
(class Association Object 
   (key value)
   (class-method key:value: (a b) (setKey:value: (new self) a b))
   (method setKey:value: (x y) (set key x) (set value y) self) ; private
   (method key    ()  key)
   (method value  ()  value)
   (method key:   (x) (set key   x))
   (method value: (y) (set value y))
)
;;;usm.nw:4720
(class Dictionary KeyedCollection
    (table) ; list of Associations
    (class-method new ()      (initDictionary (new super)))
    (method initDictionary () (set table (new List)) self) ; private
    (method printName ()      (print #Dictionary))
    (method species ()        Dictionary)
    
;;;usm.nw:4735
(method associationsDo: (aBlock) (do: table aBlock))
(method at:put: (key value) (locals tempassoc)
    (set tempassoc (associationAt:ifAbsent: self key []))
    (if (isNil tempassoc)
         [(add: table (key:value: Association key value))]
         [(value: tempassoc value)])
    value)
;;;usm.nw:4746
(method add: (_) (error: self #can't-just-add:-to-a-Dictionary))
;;;usm.nw:4727
)
;;;usm.nw:4759
(class SequenceableCollection KeyedCollection
    () ; abstract class
    (method firstKey () (subclassResponsibility self))
    (method lastKey  () (subclassResponsibility self))
    (method last     () (at: self (lastKey  self)))
    (method first    () (at: self (firstKey self)))
    (method at:ifAbsent: (index exnBlock) (locals current resultBlock)
        (set resultBlock exnBlock)
        (set current (firstKey self))
        (do: self (block (v)
            (ifTrue: (= current index) [(set resultBlock [v])])
            (set current (+ current 1))))
        (value resultBlock))
)
;;;usm.nw:4969
(class Cons Object
    (car cdr)
    (method car ()           car)
    (method cdr ()           cdr)
    (method car: (anObject)  (set car anObject) self)
    (method cdr: (anObject)  (set cdr anObject) self)
    (method pred: (aCons)    nil)
    
;;;usm.nw:4984
(method deleteAfter () (locals answer)
    (set answer (car cdr))
    (set cdr    (cdr cdr))
    (pred: cdr self)
    answer)
(method insertAfter: (anObject)
    (set cdr (car: (cdr: (new Cons) cdr) anObject))
    (pred: (cdr cdr) cdr)
    anObject)
;;;usm.nw:5010
(method do: (aBlock)
    (value aBlock car)
    (do: cdr aBlock))
;;;usm.nw:5024
(method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)
    (if (value aBlock self)
        [(deleteAfter pred)]
        [(rejectOne:ifAbsent:withPred: cdr aBlock exnBlock self)]))
;;;usm.nw:4977
)
;;;usm.nw:5037
(class ListSentinel Cons
    (pred)
    (method pred: (aCons)   (set pred aCons))
    (method pred  ()        pred)
    (class-method new ()    (locals tmp)
        (set tmp (new super))
        (pred: tmp tmp)
        (cdr:  tmp tmp)
        tmp)
    
;;;usm.nw:5014
(method do: (aBlock) nil)
;;;usm.nw:5029
(method rejectOne:ifAbsent:withPred: (aBlock exnBlock pred)
    (value exnBlock))
;;;usm.nw:5046
                                                   )
;;;usm.nw:4890
(class List SequenceableCollection
    (sentinel)
    (class-method new ()        (sentinel: (new super) (new ListSentinel)))
    (method sentinel: (s)       (set sentinel s) self) ; private
    (method isEmpty   ()        (= sentinel (cdr sentinel)))
    (method last      ()        (car (pred sentinel)))
    (method do:       (aBlock)  (do: (cdr sentinel) aBlock))
    (method species   ()        List)
    (method printName ()        (print #List))
    
;;;usm.nw:4909
(method addLast:  (item)   (insertAfter: (pred sentinel) item))
(method addFirst: (item)   (insertAfter: sentinel item))
(method add: (item)        (addLast: self item))
;;;usm.nw:4917
(method removeFirst ()     (deleteAfter sentinel))
;;;usm.nw:4933
(method remove:ifAbsent: (oldObject exnBlock)
    (rejectOne:ifAbsent:withPred:
        (cdr sentinel)
        (block (x) (= oldObject (car x)))
        exnBlock
        sentinel))
;;;usm.nw:4946
(method firstKey () 1)
(method lastKey  () (size self))
(method at:put: (n value) (locals tmp)
    (set tmp (cdr sentinel))
    (whileFalse: [(= n 1)]
       [(set n (- n 1))
        (set tmp (cdr tmp))])
    (car: tmp value))
;;;usm.nw:4900
)
;;;usm.nw:5134
(class Array SequenceableCollection
    () ; representation is primitive
    (class-method new: primitive arrayNew:)
    (class-method new () (error: self #size-of-Array-must-be-specified))
    (method size      primitive arraySize)
    (method at:       primitive arrayAt:)
    (method at:put:   primitive arrayAt:Put:)
    (method species   () Array)
    (method printName () nil) ; names of arrays aren't printed
    
;;;usm.nw:5154
(method add:             (x)   (fixedSizeError self))
(method remove:ifAbsent: (x b) (fixedSizeError self))
(method fixedSizeError   ()    (error: self #arrays-have-fixed-size))
;;;usm.nw:5161
(method select:  (_) (error: self #select-on-arrays-not-implemented))
(method collect: (_) (error: self #collect-on-arrays-not-implemented))
;;;usm.nw:5167
(method firstKey () 1)
(method lastKey  () (size self))
(method do: (aBlock) (locals index)
    (set index (firstKey self))
    (timesRepeat: (size self)
       [(value aBlock (at: self index))
        (set index (+ index 1))]))
;;;usm.nw:5144
)
