#lang racket

(require (lib "27.ss" "srfi"))
;Exercise 1.3: Define a procedure that takes three numbers
;as arguments and returns the sum of the squares of the two
;larger numbers

(define (sum-of-squares-of-larger-nums x y z)
  (cond
    ((and (< z x) (< z y)) (+ (* x x) (* y y)))
    ( (> x y (+ (* z z) (* x x))))
    (else (+ (* z z ) (* y y)))))

(sum-of-squares-of-larger-nums 3 4 5)
(= (sum-of-squares-of-larger-nums 3 4 5) 41)

;Exercise 1.8: Newton’s method for cube roots is based on
;the fact that if y is an approximation to the cube root of x,
;then a beer approximation is given by the value
;(x/y^2 + 2y)/3
;
;:
;Use this formula to implement a cube-root procedure analogous
;to the square-root
(define (cube-root x)
  
  (define (cube-root-iter guess)
    (if (is-good-enough? guess )
        guess
        (cube-root-iter (improve guess ) )))
  
  (define (improve guess )
    (/ (+ (/ x (* guess guess)) (* 2 guess)) 3))
  
  (define (is-good-enough? guess )
    (< (absVal (- x (* guess guess guess))) 0.001))
  
  (cube-root-iter 1.0))


(define (absVal x) ((if (< x 0) - +) x))


(cube-root 27)
(cube-root 125)
;(= (cube-root 27) 3.0)
;(absVal -1)

(define (fib n)
  (define (fib-iter a b count)
    (if (= count 0)
        a
        (fib-iter (+ a b) a (- count 1))))
  (fib-iter 0 1 (- n 1)))

(fib 10)
(= (fib 10) 34)

;Exercise 1.11: A function f is defined by the rule that
;f (n) =
;:
;n if n < 3,
;f (n - 1) + 2f (n - 2) + 3f (n - 3) if n >=  3.
;Write a procedure that computes f by means of a recursive
;process. Write a procedure that computes f by means of an
;iterative process.

(define (f-recur n)
  (if (< n 3)
      n
      ( + (f-recur (- n 1)) (* 2 (f-recur (- n 2))) (* 3 (f-recur (- n 3))) )))

(f-recur 20)

(define (f-iter n)
  (define (f-iter-inner a b c count)
    (if (= count 0)
        c
        (f-iter-inner b c (+ c (* 2 b) (* 3 a)) (- count 1)) ))
  (f-iter-inner 0 1 2 (- n 2)))

(f-iter 20)
(f-iter 50)

;Exercise 1.12: Pascal's triangle by recursive procedure

(define (pascal-triangle n)
  (define (pascal-triangle-inner x y)
    (cond  ((= x 1) 1)
           ( (= y 1) 1)
           ((= x y) 1)
           (else (+ (pascal-triangle-inner (- x 1) (- y 1) ) (pascal-triangle-inner (- x 1) y ))) ))
  
  (define (pascal-triangle-iter colIdx )
    (if (> colIdx n)
        (list )
        (append  (pascal-triangle-iter (+ colIdx 1)) (list (pascal-triangle-inner n colIdx)) ) ))
  
  (pascal-triangle-iter 1 ))

(pascal-triangle 5)

;Exercise 1.15

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)

;Exercise 1.16 - iterative exponentiation by squaring

(define (fast-expt b n)
  (define (fast-expt-iter a counter)
    (cond ( (= counter 1) a )
          ( (even? counter) (fast-expt-iter (* a (sqr b)) (/ counter 2)))
          (else (fast-expt-iter (* (* a b) (sqr b)) (/ (- counter 1) 2)))))
  (define (sqr x) (* x x))
  (define (even? x) (if (= (remainder x 2) 0) #t #f))
  
  (fast-expt-iter 1 n))

(fast-expt 3 4)
(fast-expt 3 3)

(fast-expt 2 1000)

;Exercise 1.17 & 1.18

(define (fast-mult x y)
  (define (even? x) (if (= (remainder x 2) 0) #t #f))
  (define (double x) (* 2 x))
  (define (halve x) (/ x 2))
  (define (fast-mult-iter remainder a b)
    (cond ((= b 1) (+ remainder a))
          ((even? b) (fast-mult-iter remainder (double a) (halve b)))
          (else  (fast-mult-iter (+ a remainder) a (- b 1)))))
  (fast-mult-iter 0 x y)
  )

(fast-mult 8 4)
(fast-mult 8 3)
(fast-mult 8 6)
(fast-mult 5432 3124)

;Exercise 1.19

(define (T pr) (cons (+ (car pr) (cdr pr)) (car pr)))

(define (fib-iter n)
  (define (T-pq pr p q)
    (cons (+ (* (cdr pr) q) (* (car pr) (+ p q)) )  (+ (*(cdr pr) p) (* (car pr) q)) ))
  (define (square_T-pq pr p q)
    (T-pq pr (+ (* p p) (* q q)) (* 2 q (+ p q)) ))
  ;T-pq ^ n
  (define (fib-iter-inner pr p q k)
    (cond ((< k 3) pr)
          ((even? k) (fib-iter-inner (square_T-pq pr p q) p q (/ k 2)))
          (else (square_T-pq (fib-iter-inner (square_T-pq pr p q) p q (/ (- k 1) 2)) p q ))))
  (car (fib-iter-inner (cons 0 1) 0 1 n))
  )

(fib-iter 10)
(fib-iter 1000)

;Exercise 1.20
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(gcd 206 40)

; normal order evaluation - (gcd 206 40) =>

;Exercise 1.21: Use the smallest-divisor procedure to find
;the smallest divisor of each of the following numbers: 199,
;1999, 19999.


(define (smallest-divisor n)
  (define (smallest-divisor-inner test-divisor)
    (cond ((divides? n test-divisor) test-divisor)
          ((> (square test-divisor) n) n)
          (else (smallest-divisor-inner (+ 1 test-divisor )))
          ))
  (define (divides? p q) (= (remainder p q) 0))
  (define (square k) (* k k))
  (smallest-divisor-inner 2)
  )
(smallest-divisor 199)
(smallest-divisor 1999)
(smallest-divisor 19999)

;Exercise 1.22

(define (prime? n) (= (smallest-divisor n) n))

(define (timed-prime-test prime-test n)
  ;(newline)
  ;(display n)
  (start-prime-test prime-test n (current-inexact-milliseconds)))

(define (start-prime-test prime-test n start-time)
  (if (prime-test n)
      (report-prime n (- (current-inexact-milliseconds) start-time)) #f ))

(define (report-prime n elapsed-time)
  (display " *** ")
  (display n)
  (display " ")
  (display elapsed-time)
  (newline)
  #t)

(timed-prime-test prime? 1999)

(define (search-for-primes prime-test a n)
  (cond ( (= n 0) `())
        ((even? a) (search-for-primes prime-test (+ a 1) n))
        (else (if (timed-prime-test prime-test a) (search-for-primes prime-test (+ a 2) (- n 1)) (search-for-primes prime-test (+ a 2) n ) ) )
        ))
(search-for-primes prime? 1000 3)
(search-for-primes prime? 10000 3)
(search-for-primes prime? 100000 3)
(search-for-primes prime? 1000000 3)

;Exercise 1.23
(display "Exercise 1.23")
(newline)
(define (next k) (if (= k 2) 3 (+ k 2)))
(define (smallest-divisor-1 n)
  (define (smallest-divisor-inner test-divisor)
    (cond ((divides? n test-divisor) test-divisor)
          ((> (square test-divisor) n) n)
          (else (smallest-divisor-inner (next test-divisor )))
          ))
  (define (divides? p q) (= (remainder p q) 0))
  (define (square k) (* k k))
  (smallest-divisor-inner 2)
  )
(define (prime-1? n) (= (smallest-divisor-1 n) n))


(search-for-primes prime-1? 1000 3)
(search-for-primes prime-1? 10000 3)
(search-for-primes prime-1? 100000 3)
(search-for-primes prime-1? 1000000 3)
(search-for-primes prime-1? 10000000000 3)

;Exercise 1.24

(display "Exercise 1.24")
(newline)
(define (square x) (* x x))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder
          (square (expmod base (/ exp 2) m))
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))
(define (fermat-test  n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random-integer (- n 1)))))

(define (fast-prime? fermat-test-fn n times)
(cond ((= times 0) #t)
((fermat-test-fn  n) (fast-prime? fermat-test-fn n (- times 1)))
(else #f)))

(define (fast-prime-test? n) (fast-prime? fermat-test n 5))


(search-for-primes fast-prime-test? 1000 3)
(search-for-primes fast-prime-test? 10000 3)
(search-for-primes fast-prime-test? 100000 3)
(search-for-primes fast-prime-test? 1000000 3)
(search-for-primes fast-prime-test? 10000000000 3)

;Exercise 1.27

(define (test-fermat n)
  (define (test-fermat-inner a)
    (cond ((= a n) #t)
          ((= (expmod a n n) a) (test-fermat-inner (+ a 1)))
          (else #f)))
  (test-fermat-inner 2))
(display "fermat theorem for Carmichael number - 561 ")
(newline)
(test-fermat 561)
(newline)


(display "fermat theorem for Carmichael number -  1105")
(newline)
(test-fermat 1105)
(newline)

(display "fermat theorem for Carmichael number - 1729")
(newline)
(test-fermat 1729)
(newline)

(display "fermat theorem for Carmichael number - 2465")
(newline)
(test-fermat 2465)
(newline)

;Exercise 1.28
(define (divides? p q) (= 0 (remainder p q)))

(define (miller-sabin-test x n)
  (if (and  (not (= x 1)) (not (= x (- n 1))) (divides? (- x 1) n))
          0
          (remainder (square x) n)
          )) 
(define (expmod-miller-sabin base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (miller-sabin-test
            (expmod base (/ exp 2) m)
          m))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))

(define (fermat-test-miller-sabin  n)
  (define (try-it a)
    (= (expmod-miller-sabin a (- n 1) n) 1))
  (try-it (+ 2 (random-integer (- n 2)))))

(define (miller-sabin-prime-test? n) (fast-prime? fermat-test-miller-sabin n 5))

(display "miller-sabin for Carmichael number - 2465")
(newline)
(miller-sabin-prime-test? 2465)
(newline)
(miller-sabin-prime-test? 6700417)
(miller-sabin-prime-test? 11)
(fast-prime-test? 6700417 )

;Exercise 1.29

(define (summation a b term next)
  (if (> a b)
      0
      (+ (term a) (summation (next a) b term next))))

(define (inc n) (+ n 1))

(define (simpson-integral f a b n)
  (define (simpson-integral-inner h)
    (define (simpsonFn x)
      (* (if (or (= x 0) (= x n)) 1 (if (even? x) 2 4) )
         (f (+ a (* x h)))))
    (exact->inexact(* (/ h 3) (summation 0 n simpsonFn inc))))
  (simpson-integral-inner (/ (- b a) n) ))

(simpson-integral cube 0 1 100)

(simpson-integral cube 0 1 1000)

;Exercise 1.30

(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)) )))
  (iter a 0))

(define (identity x) x)

(sum identity 1 inc 10)

;Exercise 1.31

;Exercise 1.30

(define (prod term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)) )))
  (iter a 1))

(define (factorial n)
  (prod identity 1 inc n))

(factorial 8)

(define (wallis-product n)
  (define (wallis-term k)
    (exact->inexact (* (/ (* 2 k) ( - (* 2 k) 1)) (/ (* 2 k) (+ (* 2 k) 1) ))))
  (prod wallis-term 1 inc n))

(* 2 (wallis-product 1000))

(define (prod-recur term a next b)
  (if (> a b)
      1
      (* (term a) (prod-recur term (next a) next b))))

(define (factorial-prod-recur n)
  (prod-recur identity 1 inc n))

(factorial-prod-recur 8)

;Exercise 1.32

(define (accumulate combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a) (accumulate combiner null-value term (next a) next b))))

(define (sum-from-accumulate term a next b)
  (accumulate + 0 term a next b))

(sum-from-accumulate identity 1 inc 10)

(define (prod-from-accumulate term a next b)
  (accumulate * 1 term a next b))

(define (factorial-prod-accumulate n)
  (prod-from-accumulate  identity 1 inc n))

(factorial-prod-accumulate 100)

(define (accumulate-iter combiner null-value term a next b)
  (define (iter curr-a result)
    (if (> curr-a b)
        result
        (iter (next curr-a) (combiner (term curr-a) result))))
  (iter a null-value))

(define (prod-from-accumulate-iter term a next b)
  (accumulate-iter * 1 term a next b))

(define (factorial-prod-accumulate-iter n)
  (prod-from-accumulate  identity 1 inc n))

(factorial-prod-accumulate-iter 100)

;Exercise 1.33

(define (accumulate-filtered combiner null-value term a next b filter?)
  (define (apply-filter x)
    (if (filter? x) x null-value))
  (define (iter curr-a result)
    (if (> curr-a b)
        result
        (iter (next curr-a) (combiner (apply-filter (term curr-a)) result))))
  (iter a null-value))

(+ (accumulate-filtered + 0 identity 3 inc 100 miller-sabin-prime-test?) 2)

(accumulate-filtered + 0 identity 2 inc 100 fast-prime-test?)

(accumulate-filtered + 0 identity 2 inc 10 fast-prime-test?)

(+ (accumulate-filtered + 0 identity 3 inc 2000000 miller-sabin-prime-test?) 2)
(accumulate-filtered + 0 identity 2 inc 2000000 fast-prime-test?)

(accumulate-filtered * 1 identity 1 inc 100 (lambda (x) (= (gcd x 100) 1)))

;Exercise 1.35

(define tolerance 0.00001)

(define (fixed-point f  first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  (define (try guess indx)
    (display "try the approximation --> ")
    (display indx)
    (display "  --  ")
    (display guess)
    (newline)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          guess
          (try next (+ indx 1)))))
  (try first-guess 1))

(display "golden ratio")
(newline)
(fixed-point (lambda (x) (+ 1 (/ 1 x)))  1.0)

;Exercise 1.36

;no damping
(fixed-point (lambda (x) (/ (log 1000) (log x)))   (log 100))

;damping
(define (damping-fn g)
  (lambda (x)
    (/ (+ (g x) (g (g x))) 2)))
(fixed-point (damping-fn (lambda (x) (/ (log 1000) (log x))))   (log 100))

;Exercise 1.37

(define (cont-frac n d k)
 (define (recur i)
   (if (> i k)
       0
       (/ (n i) (+ (d i) (recur (+ i 1))))))
  (recur 1))

(display "1/phi ")
(display (/ 1 1.61803398875))
(newline)
(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

(define (cont-frac-iter n d k)
 (define (iter res i)
   (if (= i 0)
       res
      (iter (/ (n i) (+ (d i) res)) (- i 1))))
  (iter 0 k))

(cont-frac-iter (lambda (i) 1.0)
           (lambda (i) 1.0)
           100)

;Exercise 1.38

;e-2
(cont-frac-iter (lambda (i) 1.0)
                (lambda (i)
                  (if (divides? (+ i 1) 3)
                      (* 2 (/ (+ i 1) 3))
                      1))
                10)

;Exercise 1.39

(define (tan-cf x k)
 (cont-frac-iter (lambda (i)
                   (if (= i 1)
                       x
                       (* -1 (square x))))
                 (lambda (i) (- (* 2 i) 1))
                 k) )
(tan-cf 0.78539816339 10)

(define dx 0.00001)
(define (deriv g)
(lambda (x) (/ (- (g (+ x dx)) (g x)) dx)))

(define (newton-transform g)
(lambda (x) (- x (/ (g x) ((deriv g) x)))))

(define (fixed-point-of-transform g transform guess)
(fixed-point (transform g) guess))

(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

;Exercise 1.40
; find roots of cubic function x3 + ax2 + bx + c.
;(newtons-method (cubic a b c) 1)

(define (cubic a b c)
  (lambda (x) (+ (cube x) (* a (square x) (* b x) c) )))

;(newtons-method (cubic -3 -144 432) 1)

;(newtons-method (cubic 4 -8 7) 1)

; FIX-IT:
(= (newtons-method (cubic 3 -2.4 6) 1) -3.981336651146034 )

;Exercise 1.41

(define (double g)
  (lambda (x) (g (g x))))

(((double (double double)) inc) 5)

;Exercise 1.42

(define (compose f g)
  (lambda(x) (f (g x))))

((compose square inc) 6)

;Exercise 1.43

(define (repeated f n)
  (if (= n 1)
      f
      (repeated (compose f f) (- n 1))))

((repeated square 2) 5)

;Exercise 1.44

(define (smoothed   dx f)
  (lambda (x) (/  (+ (f x) (f (+ x dx)) (f (- x dx) )) 3)))

(define (smoothed-n-fold dx f n)
  (repeated  (smoothed dx f)  n))

((smoothed 0.7 sin ) (/ pi 2))

;FIX-IT
((smoothed-n-fold 0.7 sin 4) (/ pi 2))

((smoothed-n-fold 0.7 sin 3) (/ pi 2))

;Exercise 1.45
;TODO

;Exercise 1.46

(define (iterative-improve good-enough? improve)
  (define  (inner x)
    (let
        ( (next (improve x)) )
      (if (good-enough? x next)
          x
          (inner next))))
  inner
  )

(define (fixed-point1 f  first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       tolerance))
  
  ((iterative-improve close-enough? f) first-guess ))

(display "golden ratio")
(newline)
(fixed-point1 (lambda (x) (+ 1 (/ 1 x)))  1.0)

(define (sqrt1 x)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2))
       0.001))
  ((iterative-improve close-enough? (lambda (y) (/ (+ y (/ x y)) 2))) 1.0))

(sqrt1 2)