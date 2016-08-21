#lang racket

(define (variable? x) (symbol? x)) ;Is e a variable?
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2))) ;Are v1 and v2 the same variable?

(define (expression? x) (not (or (variable? x) (number? x))))

(define (infix-prefix e)
  (define (stripIf1 lst1) (if (and (pair? lst1) (null? (cdr lst1))) (car lst1) lst1))
  (define (inner1 lt rt sign currL currR)
    (cond
      ((null? currR) (list sign (stripIf1 (reverse lt)) (stripIf1 rt)))
        ((eq? '+ (car currR)) (if (or (null? sign) (eq? sign '*)) (inner1 currL (cdr currR) (car currR) (cons (car currR) currL) (cdr currR) ) (inner1 lt rt sign (cons (car currR) currL) (cdr currR) ) ))
        ((eq? '* (car currR)) (if (or (null? sign) ) (inner1 currL (cdr currR) (car currR) (cons (car currR) currL) (cdr currR) ) (inner1 lt rt sign (cons (car currR) currL) (cdr currR) ) ))
        (else (inner1 lt rt sign (cons (car currR) currL) (cdr currR) ))
        ))
  (if (and (pair? e) (not (or (eq? '+ (car e)) (eq? '* (car e))))) (inner1 '() '() null '() e) e))

(define (sum? e) (let ( (x (infix-prefix e))) (and (pair? x) (eq? (car x) '+)) )) ;Is e a sum?
(define (addend e) (let ((s (infix-prefix e))) (cadr s))) ;Addend of the sum e.
;(define (augend s) (caddr s)) ;Augend of the sum e.

(define (augend e)
  (let ( (s (infix-prefix e)) )
      (if (null? (cdddr s)) (caddr s)  (cons '+ (cddr s)  ))))

;Exercise 2.58 - infix operators

(define (infix-sum? x) (and (pair? x) (eq? (cadr x) '+))) ;Is e a sum?
(define (infix-addend s) (car s)) ;Addend of the sum e.
(define (infix-augend s) (if (null? (cdddr s)) (caddr s)   (cddr s)  )) ;Augend of the sum e.

;(define (augend s) (if (null? (cdddr s)) (caddr s)  (cons '+ (cddr s)  )))

(define (product? e) (let ( (x (infix-prefix e))) (and (pair? x) (eq? (car x) '*)) )) ;Is e a product?
(define (multiplier e) (let ((p (infix-prefix e))) (cadr p))) ;Multiplier of the product e.
;(define (multiplicand p) (caddr p)) ;Multiplicand of the product e.

(define (multiplicand e)
  (let ((s (infix-prefix e)))
  (if (null? (cdddr s)) (caddr s)  (cons '* (cddr s)  ))))

;Exercise 2.58 - infix operators

(define (infix-product? x) (and (pair? x) (eq? (cadr x) '*))) ;Is e a product?
(define (infix-multiplier p) (car p)) ;Multiplier of the product e.
(define (infix-multiplicand s) (if (null? (cdddr s)) (caddr s)  (cddr s)  )) ;Multiplicand of the product e.


;version #1
;(define (make-sum a1 a2) (list '+ a1 a2)) ;Construct the sum of a1 and a2.
;(define (make-product m1 m2) (list '* m1 m2)) ;Construct the product of m1 and m2.

(define (=number? exp num) (and (number? exp) (= exp num)))

;(define (evaluate e))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
       ;( (and (expression? a2) (sum? a2)) (make-sum (make-sum a1 (addend a2)) (augend a2) ))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        ;( (expression? m2) (if (product? m2) (make-product (make-product m1 (multiplier m2)) (multiplicand m2) ) ()))
        (else (list '* m1 m2))))

;(define (deriv exp var)
;  (cond ((number? exp) 0)
;        ((variable? exp) (if (same-variable? exp var) 1 0))
;        ((sum? exp) (make-sum (deriv (addend exp) var)
;                              (deriv (augend exp) var)))
;        ((product? exp)
;         (make-sum
;          (make-product (multiplier exp)
;                        (deriv (multiplicand exp) var))
;          (make-product (deriv (multiplier exp) var)
;                        (multiplicand exp))))
;        (else
;         (error "unknown expression type: DERIV" exp))))



;Exercise 2.56

(define (make-exponentiation b exp)
  (define (my-exp x y a) (if (= y 0) a (my-exp x (- y 1) (* a x) )))
  (cond ((=number? exp 0) 1)
        ((=number? exp 1) b)
        ((and (number? b) (number? exp)) (my-exp b exp 1))
        (else (list '** b exp))))

(define (exponentiation? x) (and (pair? x) (eq? (car x) '**)))
(define (base x) (cadr x))
(define (exponent x) (caddr x))

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp) (make-sum (deriv (addend exp) var)
                              (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ( (exponentiation? exp) (make-product (make-product (exponent exp) (make-exponentiation (base exp) (make-sum (exponent exp) -1))) (deriv (base exp) var)))
        (else
         (error "unknown expression type: DERIV" exp))))


(deriv '(+ x 3) 'x)

(deriv '(* x y) 'x)

(deriv '(* (* x y) (+ x 3)) 'x)

(deriv '(** x n) 'x)

(deriv '(** u n) 'x)

;Exercise 2.57

(deriv '(* x y (+ x 3)) 'x)

;Exercise 2.58
(display "infix operators")
(newline)
(infix-addend  '(y + 2))
(infix-augend  '(y + 2))
(deriv '(y + 2) 'x)

(deriv '(x + (3 * (x + (y + 2)))) 'x) ; ==  4

;Exercise 2.58 - b
(display "infix operators - more than 2 terms")
(newline)
(deriv '(x + y + 2) 'x)
(deriv '(x + y + x) 'x) ; == 2
(eq? 4 (deriv '(x + 3 * (x + y + 2)) 'x)) ;== 4

(deriv '(x * 3 + (x + y + 2)) 'x) ;== 4

 (deriv '(x * 3 + x + y + 2) 'x) ;== 4

(eq? 3 (deriv '(x * 3 + 4) 'x)) ; == 3

(make-sum 3 '(+ 4 1)) ; == 8

(make-sum 3 '(4 + 1)) ; == 8

(make-product 3 '(4 + 1)) ; == 15

(infix-prefix '(a + b + c))

(infix-prefix '(a * b + c * d))

(infix-prefix '(a + b * c))

(infix-prefix '(a * b * c))