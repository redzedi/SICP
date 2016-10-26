#lang racket

;(module genericArithmatic  racket
(provide (all-defined-out))
;(provide install-scheme-number-package)
;(load "chap2_tagBasedTypeSystem.rkt")
;commented for Exercise 2.86
;(define square sqr)

;works only with equal lengthed lists 
(define (my-zip l1 l2)
  (car (foldl (λ (y acc)  (cons (cons (cons y (cadr acc)) (car acc)) (cddr acc)) ) (cons '() l2) l1)))
;language features (??)

(define (attach-tag type-tag contents)
  (if (number? contents) contents
      (cons type-tag contents)))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (cond
        ((number? datum) 'scheme-number)
        ((null? datum) 'NULL)
        (else (error "Bad tagged datum: TYPE-TAG" datum)))
      ))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (cond ((number? datum) datum)
            ((null? datum) null)
            (else (error "Bad tagged datum: CONTENTS" datum)))
      ))

;(define BASE-TYPE 'BASETYPE)
(define type-meta-map (make-hash))
(define type-map (make-hash) )

(define (create-typeName-key typeTags)
  (define (create-string-key symbs)
    ;(display "symbs  ")(display symbs)(newline)
    (apply string-append (map symbol->string symbs)))
  ;(foldl (λ (s acc) (if (pair? s) (string-append "(" (create-string-key s) ")" acc) (string-append acc (symbol->string s)))) "" symbs))
  (if (null? typeTags) "NULL" (create-string-key (if (pair? typeTags) typeTags  (list typeTags)))))

(define (put opName argTypeTags opRef)
  (define (inner-set implMap)
    (hash-set! implMap (create-typeName-key argTypeTags) opRef ))
  (inner-set (hash-ref! type-map opName (make-hash))  ))

(define (get opName typeTags)
  (let ( (fixedArgOpRef (hash-ref (hash-ref type-map opName) (create-typeName-key typeTags) null)))
    (if (not (null? fixedArgOpRef))
        fixedArgOpRef
        (let ( (firstType (car typeTags) ))
          (let (
                (allTypesEq (andmap (λ (x) (eq? x firstType)) typeTags))
                )
            (if (and (not (null? firstType)) allTypesEq)
                (hash-ref (hash-ref type-map opName)  (string-append (symbol->string firstType) "*") null)
                null)
            )))))

(define (put-coercion from-type to-type convFn)
  (put 'type-coercion (list from-type to-type) convFn))

(define (get-coercion from-type to-type)
  ;(display "get-coercion " )(display from-type)(display " --> ")(display to-type   )
  (get 'type-coercion (list from-type to-type)))

;without coercion
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (error "No method for these types: APPLY-GENERIC"
;                 (list op type-tags))))))

;with coercion (max 2 args)
;(define (apply-generic op . args)
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      (if proc
;          (apply proc (map contents args))
;          (if (= (length args) 2)
;              (let ((type1 (car type-tags))
;                    (type2 (cadr type-tags))
;                    (a1 (car args))
;                    (a2 (cadr args)))
;                (let ((t1->t2 (get-coercion type1 type2))
;                      (t2->t1 (get-coercion type2 type1)))
;                  (if (eq? type1 type2)
;                      (error "No method for these types"
;                                     (list op type-tags)) ;Exercise 2.81 c
;                  (cond (t1->t2
;                         (apply-generic op (t1->t2 a1) a2))
;                        (t2->t1
;                         (apply-generic op a1 (t2->t1 a2)))
;                        (else (error "No method for these types"
;                                     (list op type-tags)))))))
;              (error "No method for these types"
;                     (list op type-tags)))))))



;with coercion (multiple args) - looking for direct conversions from 1 type to another
;(define (apply-generic op . args)
;  
;  (let ((type-tags (map type-tag args)))
;    (let ((proc (get op type-tags)))
;      
;      (if (not (null? proc))
;          (apply proc (map contents args))
;          ;nested loop - pair(find the corresponding conversion function if any) each element with everything else
;          ;filter - retain only the list that has all not null values
;          ;until - terminate as soon as 1 such list found
;          (let ( (t-conv-fns  (car (foldl (λ (x acc)
;                                            ; (display "acc -> ") (display acc)                     
;                                            (if (not (null? (car acc))) acc  
;                                                (let ((t-convs (map (λ (y) (if (eq? y x) (λ (z) z) (get-coercion y x))) (cdr acc)) ))
;                                                  ; (newline)(display " t-convs -> ")(display t-convs) (newline)
;                                                  (if (foldl (λ (z1 acc1) (and acc1  (not (null? z1)))) #t t-convs )
;                                                      (cons t-convs (cdr acc))
;                                                      (cons '() (cdr acc)))
;                                                  ))) (cons '() type-tags) type-tags))))
;            
;            (if (not (null? t-conv-fns))
;                (apply apply-generic (cons op (map (λ (x) ((car x)  (cdr x))) (my-zip  t-conv-fns  args))))
;                (error "No method for these types"
;                       (list op type-tags)))
;            )
;          
;          ;          (if (= (length args) 2)
;          ;              (let ((type1 (car type-tags))
;          ;                    (type2 (cadr type-tags))
;          ;                    (a1 (car args))
;          ;                    (a2 (cadr args)))
;          ;                (let ((t1->t2 (get-coercion type1 type2))
;          ;                      (t2->t1 (get-coercion type2 type1)))
;          ;                  (if (eq? type1 type2)
;          ;                      (error "No method for these types"
;          ;                                     (list op type-tags)) ;Exercise 2.81 c
;          ;                  (cond (t1->t2
;          ;                         (apply-generic op (t1->t2 a1) a2))
;          ;                        (t2->t1
;          ;                         (apply-generic op a1 (t2->t1 a2)))
;          ;                        (else (error "No method for these types"
;          ;                                     (list op type-tags)))))))
;          ;              (error "No method for these types"
;          ;                     (list op type-tags)))
;          
;          
;          ))))

;Exercise 2.84
(define (apply-generic op . args)
  (define (dropIfNeeded prc argVals)
    (let ((procRes (apply prc argVals))) (if (or (eq? 'add op) (eq? 'sub op) (eq? 'mul op) (eq? 'div op) (eq? 'square op)) (drop procRes) procRes )))
  (let ((type-tags (map type-tag args)))
    ;(display "**** apply-generic ops "  )(display op)(display " type-tags ")(display type-tags)(display " args ")(display args)(newline)
    (let ((proc (get op type-tags)))
      
      (if (not (null? proc))
          ; (drop (apply proc (map contents args)))
          ; (apply proc (map contents args))
          (dropIfNeeded proc (map contents args))
          
          (let (
                (normalizedArgs (map (λ (tpN) (raise-n-times (car tpN) (cdr tpN) ) ) (my-zip args  (map (λ (diffs) (foldl (λ (x acc) (if (> x acc) x acc)) -99999 diffs)) (map (λ (t) (map (λ (t1) (compare-types t t1)) type-tags) ) type-tags)))))
                )
            ; (newline) (display "normalizedArgs --> ") (display normalizedArgs) (newline)
            
            (let ((newProc (get op (map type-tag normalizedArgs)))) (if (not (null? newProc))
                                                                        ;(drop (apply newProc (map contents normalizedArgs)))
                                                                        (dropIfNeeded newProc (map contents normalizedArgs))
                                                                        (error "No method for these types"
                                                                               (list op type-tags))))
            )
          
          
          ))))

(define (super-type type-tag) (hash-ref type-meta-map type-tag null) )
(define INFINITY 9999)
(define (raise-n-times t n)
  (if (= n 0)
      t
      (raise-n-times (raise t) (- n 1))))

; (- (depth t1) (depth t2))
(define (compare-types t1 t2)
  (define (compare-inner t tgt cnt)
    (cond ((eq? t tgt) cnt)
          ( (null? t) INFINITY)
          (else (compare-inner (super-type t) tgt (+ cnt 1) ))))
  (let ((t1-t2 (compare-inner t1 t2 0)))
    (if (not (= t1-t2 INFINITY))
        t1-t2
        (let ((t2-t1 (compare-inner t2 t1 0)))
          (if (not (= t2-t1 INFINITY))
              (* -1 t2-t1)
              (error " the 2 types are not related " (list t1 t2)    ))))))

;interfaces
;artihmatic package that defines the following basic operation on various types of  object

;(load "chap2_arithmaticPackageIntf.rkt" )
(define (multi-add . x) (apply apply-generic (append  '(multi-add) x)))
(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))

(define (equ? x y) (apply-generic 'equ x y))

(define (zero? x) (apply-generic 'zero x))

(define (raise x) (apply-generic 'raise x))

(define (project x) (apply-generic 'project x))

(define (drop x)
  (cond ((not (pair? x)) x)
        ((list? x) (map drop x))
        (else (let ((nextX ((get 'project (type-tag x)) (contents x))))
                (if  (and (not (null? nextX)) (equ? x ((get 'raise (type-tag nextX)) (contents nextX)))) (drop nextX) x)))))



(define (square x) (apply-generic 'square x))

(define (my-atan x y) (apply-generic 'atan x y))

(define (my-sin x) (apply-generic 'sin x ))

(define (my-cos x) (apply-generic 'cos x ))

(define (negate x) (apply-generic 'negate x ))

(define (greatest-common-divisor x y) (apply-generic 'greatest-common-divisor x y))

 (define (my-exp b n)
   (define (exp-iter currN res)
     (if (zero? currN)
         res
         (exp-iter ((if (> currN 0) sub add) currN 1) ((if (> currN 0) mul div ) res b) )
         ))
   (exp-iter n 1))

(define (reduce x y) (apply-generic 'reduce x y))

;schema number package

(define (install-scheme-number-package)
  
  (install-rational-package)
  (hash-ref! type-meta-map 'scheme-number 'rational)
  (define (tag x) (attach-tag 'scheme-number x))
  
  (define (my-gcd a b)
    (if (= b 0)
        a
        (my-gcd b (remainder a b))))

  (define (reduce-integers n d)
(let ((g (gcd n d)))
(list (/ n g) (/ d g))))
  
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  
  (put 'make 'scheme-number (lambda (x) (tag x)))
  
  (put 'equ '(scheme-number scheme-number) (lambda (x y) (= (contents x) (contents y) )))
  
  (put 'multi-add 'scheme-number* (λ (y . x) (+ y (apply + x))))
  
  
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  
  (put 'zero 'scheme-number (λ (x) (= 0 x)))
  
  (put-coercion 'scheme-number
                'complex
                scheme-number->complex)
  
  (put 'raise 'scheme-number (λ (x) ((get 'make 'rational) x 1)))
  
  ;bottom of type tower
  (put 'project 'scheme-number (λ (x) (display "project scheme-number")  null))
  
  (put 'square 'scheme-number (λ (x) (sqr x)))
  
  (put 'atan '(scheme-number scheme-number) (λ (x y) (atan x y)))
  
  (put 'sin 'scheme-number (λ (x ) (sin x)))
  
  (put 'cos 'scheme-number (λ (x ) (cos x)))
  
  (put 'negate 'scheme-number (λ (x) (tag (* -1 x))))

  (put 'greatest-common-divisor '(scheme-number scheme-number) my-gcd)

  (put 'reduce '(scheme-number scheme-number) reduce-integers)
  
  'done)



;scheme number constructor

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))


;rational number package

(define (install-rational-package)
  (install-complex-package)
  (hash-ref! type-meta-map 'rational 'complex)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    ;(let ((g (greatest-common-divisor n d)))
    ;  (cons (div n g) (div d g)))
    (let ((redND (reduce n d)))
      (cons (car redND) (cadr redND)))
    )
  
  (define (make-rat-from-1 n)
    (make-rat (inexact->exact (numerator n)) (inexact->exact (denominator n)))
    )
  
  (define (add-rat x y)
    (make-rat (add (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (sub (mul (numer x) (denom y))
                   (mul (numer y) (denom x)))
              (mul (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (mul (numer x) (numer y))
              (mul (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (mul (numer x) (denom y))
              (mul (denom x) (numer y))))
  
  (define (val-rat x) (div (numer x) (denom x)))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  
  (put 'equ '(rational rational)
       (lambda (x y) (and (= (numer x) (numer y)) (= (denom x) (denom y)))))
  
  (put 'zero 'rational (λ (x) (zero? (numer x) )))
  
  (put 'raise 'rational (λ (x) ((get 'make-from-real-imag 'complex) (val-rat x) 0)))
  
  (put 'project 'rational (λ (x) (make-scheme-number (numer x))))
  
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put-coercion 'rational
                'complex
                (λ (x)
                  ;(newline) (display "rational->complex  ")(display (numer x))
                  ((get 'make-from-real-imag 'complex) (val-rat (contents x)) 0)) )
  
  ;TODO - these functions should return rational numbers rather than racket primitive numbers
  ; (put 'square 'rational (λ (x)  (tag (make-rat-from-1 (sqr (val-rat x))))))
  (put 'square 'rational (λ (x)   (sqr (val-rat x))))
  (put 'atan '(rational rational) (λ (x y) (atan (val-rat x) (val-rat y))))
  
  (put 'sin 'rational (λ (x ) (sin (val-rat x))))
  
  (put 'cos 'rational (λ (x ) (cos (val-rat x))))
  
  (put 'negate 'rational (λ (x) (tag (make-rat (mul -1 (numer x)) (denom x) ))))
  'done)

;rational number constructor
(define (make-rational n d)
  ((get 'make 'rational) n d))

;complex number package

;complex number interface

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))


;package
(define (install-complex-package)
  (hash-ref! type-meta-map 'complex null)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (multi-add-complex  z)
    (make-from-real-imag (foldl + 0 (map real-part z) )
                         (foldl + 0 (map imag-part z) )))
  
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'multi-add 'complex*
       (lambda (z1 . z2)  (tag (multi-add-complex  (cons z1 z2) ))))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  
  (put 'equ '(complex complex) (λ (x y) (and (= (round (real-part x)) (round (real-part y))) (= (round (imag-part y)) (round (imag-part x)))) ))
  
  (put 'zero 'complex (λ (x) (= 0 (magnitude x))))
  
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  
  (put 'raise 'complex (λ (x) (tag x)))
  
  (put 'project 'complex (λ (x) (make-rational (real-part x) 1)))
  
  (put 'negate 'complex (λ (x) (tag (make-from-real-imag (mul -1 (real-part x)) (mul -1 (imag-part x))) )))
  'done)

;complex number constructors

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;complex number implementations

(define (install-rectangular-package) 
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (add (square (real-part z))
               (square (imag-part z)))))
  (define (angle z)
    (my-atan  (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (mul (magnitude z) (my-cos (angle z))))
  (define (imag-part z) (mul (magnitude z) (my-sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;;;;;;;;;;;;;;;;;;;;;;;;;;;

;)
