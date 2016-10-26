#lang racket

(require "chap2_genericArithmaticPackage.rkt")
;(require 'genericArithmatic)

(provide (all-defined-out))
(provide (all-from-out "chap2_genericArithmaticPackage.rkt"))

;test
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)


(define (order term) (apply-generic 'order term))
(define (coeff term) (apply-generic 'coeff term))
(define (make-term order coeff) ((get 'make 'term) order coeff))

(define (make-sparse-termlist xs) ((get 'make-sparse-termlist 'termlist) xs))
(define (make-dense-termlist xs) ((get 'make-dense-termlist 'termlist) xs))

(define (adjoin-term term term-list)
  (apply-generic 'adjoin-term term term-list))

(define (the-empty-termlist) (apply-generic 'the-empty-termlist))
(define (first-term term-list) (apply-generic 'first-term term-list))
(define (rest-terms term-list) (apply-generic 'rest-terms term-list))
(define (empty-termlist? term-list) (apply-generic 'empty-termlist term-list))

(define (div-poly p1 p2) ((get 'div-poly '(polynomial polynomial)) (contents p1) (contents p2)))




(define (install-polynomial-package)
  (install-termlist-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  
  
  ;; representation of terms and term lists
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  
  
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (negate-termlist ts)
    (if (empty-termlist? ts)
        (the-empty-termlist)
        (let ((ft (first-term ts)))
          ;(display  "in negate termlist ts - **** ")(display ts) (display "rest terms ts *** ") (display (rest-terms ts)) (display "--   DONE $$ "   )
          ;(display (adjoin-term ft (rest-terms ts)))
          ;(newline)
          
          (adjoin-term (make-term (order ft) (negate (coeff ft))) (negate-termlist (rest-terms ts)))
          )))
  (define (negate-poly x) (make-poly (variable x) (negate-termlist (term-list x))))
  
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((res-terms (div-terms (term-list p1) (term-list p2))))
          (list (tag (make-poly (variable p1) (car res-terms))) (tag (make-poly (variable p1) (cadr res-terms)))))
        (error "Polys not in same var: DIV-POLY" (list p1 p2))))
  
  (define (div-terms L1 L2)
   ; (display "**** L1 ")(display L1)(display "  **  L2 " )(display L2)(newline)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (sub (order t1) (order t2))))
                (let ((rest-of-result (div-terms (add-terms L1 (negate-termlist (mul-term-by-all-terms (make-term new-o new-c) L2))) L2)))
                  ;(display "**** rest-of-the-result ")(display rest-of-result)(newline)
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result)) (cadr rest-of-result) ) ))))))
  
  (define (gcd-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (gcd-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))

  (define (get-coeff-list tl res)
    (if (empty-termlist? tl)
        res
        (get-coeff-list (rest-terms tl) (cons (coeff (first-term tl)) res))))

  (define (mul-all-terms-by-number n tl) (mul-term-by-all-terms (make-term 0 n) tl ))
  
  (define (simplify-termlist a)
    (let  ((gf (apply gcd (get-coeff-list a '()))))
          (display " gcd-terms ** greatest factor *** "  )(display gf)(newline)
          (mul-all-terms-by-number  (div 1 gf) a)))

  (define (gcd-terms a b)
    (if (empty-termlist? b)
        (simplify-termlist a) 
        (gcd-terms b (pseudoremainder-terms a b))))
  
  (define (remainder-terms l1 l2)
    (cadr (div-terms l1 l2)))

   (define (pseudoremainder-terms l1 l2)
     (display "first term l1 ****** base " )(display (coeff (first-term l2)))(newline)
     (display "first term l2 ****** " )(display (multi-add (order (first-term l1)) (negate (order (first-term l2))) 1 ))(newline)
       (let ((c1 (my-exp (coeff (first-term l2)) (multi-add (order (first-term l1)) (negate (order (first-term l2))) 1 )) ))
         (display "c1 **** " ) (display c1)(newline)
    (cadr (div-terms (mul-all-terms-by-number  c1 l1 ) l2))))

   
  (define (reduce-terms n d)
    (let ((gf (gcd-terms n d)))
      (let ((c1 (coeff (first-term gf)))
            (o2 (negate (order (first-term gf))))
            (on1 (order (first-term n)))
            (dn1 (order (first-term d))))
        (let ((f1 (my-exp c1 (multi-add 1 (if (> on1 dn1) on1 dn1) o2))))
         (list
          (simplify-termlist (car (div-terms (mul-all-terms-by-number f1 n) gf)))
          (simplify-termlist (car (div-terms (mul-all-terms-by-number f1 d) gf)))
          )))))

  (define (reduce-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((resTl (reduce-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car resTl))
                (make-poly (variable p2) (cadr resTl))))
        (error "Polys not in same var: GCD-POLY" (list p1 p2))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'make 'polynomial
       (lambda (var terms) (tag (make-poly var terms))))
  
  (put 'make-sparse-poly 'polynomial
       (lambda (var terms) (tag (make-poly var (make-sparse-termlist terms)))))
  
  (put 'make-dense-poly 'polynomial
       (lambda (var terms) (tag (make-poly var (make-dense-termlist terms)))))
  
  (put 'project 'polynomial (λ (x) null))
  (put 'raise 'polynomial (λ (x) null))
  
  (define (isZero ts prevRes) (if (and prevRes (zero? (coeff (first-term ts)))) (isZero (rest-terms ts) #t) #f ))
  
  (put 'zero 'polynomial (λ (x) (display "zero? impl *** --> ")(display (term-list x)) (newline) (isZero (term-list x) #t)))
  
  (put 'negate 'polynomial (λ (x) (tag (negate-poly x))) )
  (put 'sub '(polynomial polynomial) (λ (p1 p2) (tag (add-poly p1 (negate-poly p2)))) )
  (put 'div '(polynomial polynomial)
       (lambda (p1 p2)  (car (div-poly p1 p2))))

  (put 'div-poly '(polynomial polynomial)
       (lambda (p1 p2)   (div-poly p1 p2)))

  (put 'greatest-common-divisor '(polynomial polynomial) (λ (p1 p2) (tag (gcd-poly p1  p2))))
  
  (put 'reduce '(polynomial polynomial) (λ (p1 p2)  (let ((res (reduce-poly p1  p2))) (list (tag (car res)) (tag (cadr res))) )))
  'done)

(define (make-sparse-polynomial var terms)
  ((get 'make-sparse-poly 'polynomial) var terms))

(define (make-dense-polynomial var terms)
  ((get 'make-dense-poly 'polynomial) var terms))

;;;;;;;;;;;;;

(define (install-termlist-package)
  
  (install-sparse-termlist-package)
  (install-dense-termlist-package)
  
  (define (term-tag x) (attach-tag 'term x))
  (define (tag x) (attach-tag 'termlist x))
  
  (put 'make-sparse-termlist 'termlist (λ (xs) (tag ((get 'make-sparse-termlist 'sparse) xs))))
  (put 'make-dense-termlist 'termlist (λ (xs) (tag ((get 'make-dense-termlist 'dense) xs))))
  (put  'adjoin-term '(term termlist) (λ (t ts)
                                       ; (display "adjoin-term t s*** --> ") (display t) (display " --   ") (display ts) (newline)
                                        (tag (apply-generic 'adjoin-term (term-tag t) ts))))
  
  ;NULL is a type of its own !!
  (put  'adjoin-term '(term NULL) (λ (t ts)  (tag ((get 'make-sparse-termlist 'sparse) ts))))
  
  
  (put  'the-empty-termlist 'NULL (λ ()  (tag (attach-tag 'sparse '()))) )
  (put 'first-term 'termlist (λ (term-list) (apply-generic 'first-term term-list)))
  (put 'rest-terms 'termlist (λ (term-list) (tag (apply-generic 'rest-terms term-list))))
  (put  'empty-termlist 'termlist (λ (term-list)  (apply-generic 'empty-termlist term-list)))
  (put  'empty-termlist 'NULL (λ (term-list)  #t))
  
  
  (put  'order 'term (λ (term) (car term)))
  (put  'coeff 'term (λ (term)
                       ;(display "coeff --> ") (display (cadr term))
                       (cadr term)))
  (put  'make 'term (λ (order coeff) (term-tag (list order coeff))))
  
  'done)

;;;;;;;;;;;;;

(define (install-sparse-termlist-package)
  (define (tag x) (attach-tag 'sparse x))
  (define (term-tag x) (attach-tag 'term x))
  
  (define (get-first-term term-list) (if (not (zero? (cadar term-list))) (make-term (caar term-list) (cadar term-list)) (get-first-term (cdr term-list))))
  
  (put  'make-sparse-termlist 'sparse (λ (xs) (tag xs)))
  (put 'first-term 'sparse get-first-term)
  (put  'rest-terms 'sparse (λ (term-list)
                              ;(display "*** rest-terms ")(display term-list)(newline)
                              (tag (cdr term-list))))
  (put 'empty-termlist 'sparse (λ (term-list) (or (null? term-list) (ormap (λ (x) (zero? (cadr x))) term-list))))
  (put  'adjoin-term '(term sparse) (λ (term term-list)
                                      ; (display "term-list in sparse adjoin-term ")(display term-list)(newline)
                                      (if (zero? (coeff (term-tag term)))
                                          (tag term-list)
                                          (tag (cons term term-list))))) 
  'done)

;;;;;;;;;;;;;;;;;;;;
(define (install-dense-termlist-package)
  (define (tag x) (attach-tag 'dense x))
  (define (term-tag x) (attach-tag 'term x))
  
  (put  'make-dense-termlist 'dense (λ (xs) (tag xs)))
  (put 'first-term 'dense (λ (term-list)
                            (make-term (foldl (λ (t acc) (+ acc 1)) -1 term-list) (car term-list))))
  (put  'rest-terms 'dense (λ (term-list) (tag (cdr term-list))))
  (put 'empty-termlist 'dense (λ (term-list) (or (null? term-list) (ormap (λ (x) (zero? x)) term-list) ) ))
  
  (put  'adjoin-term '(term dense) (λ (term term-list)
                                     ; (display "term-list in sparse adjoin-term ")(display term-list)(newline)
                                     (if (= (length term-list)  (order (term-tag term)))
                                         (tag (cons (coeff term) term-list))
                                         (error "only higest term can be adjoined " (order term)))))
  
  'done)



