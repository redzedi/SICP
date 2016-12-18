#lang racket

(define (average n1 n2) (/ (+ n1 n2) 2))
;(require racket/promise)

;(require racket/stream)
(define (memo-proc proc)
  (let ((already-run? false) (result false))
    (lambda ()
      (if (not already-run?)
          (begin (set! result (proc))
                 (set! already-run? true)
                 result)
          result))))

(define (force p)  (if (null? p) p (p)))

(define stream-null? null?)
(define the-empty-stream '())

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))



;(define cons-stream cons)

(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))

(define (stream-from s n)
  (if (= n 0)
      s
      (stream-from (stream-cdr s) (- n 1))))

(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons
       low
       (memo-proc (λ () (stream-enumerate-interval (+ low 1) high))))))

(define (stream-map proc s)
  (if (stream-null? s)
      the-empty-stream
      (cons (proc (stream-car s))
            (memo-proc (λ () (stream-map proc (stream-cdr s)))))))

(define (stream-until termCond proc s)
  (if (or (stream-null? s) (termCond))
      'done
      (begin (proc (stream-car s))
             (stream-until termCond proc (stream-cdr s)))))

(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))

(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons (stream-car stream)
               (memo-proc (λ () (stream-filter
                                 pred
                                 (stream-cdr stream))))))
        (else (stream-filter pred (stream-cdr stream)))))

(define (display-line x) (newline) (display x))
(define (display-stream s) (stream-for-each display-line s) )

(define (display-stream-until termCond s) (stream-until termCond display-line s) )

(define (create-iterator n)
  (let ((currVal 1))
    (define (incr-and-check)
      (set! currVal (+ currVal 1))
      (> currVal n))
    incr-and-check))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))


;Exercise 3.50

(define (stream-map1 proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons
       (apply proc (map stream-car argstreams))
       (memo-proc (λ () (apply stream-map1
                               (cons proc (map stream-cdr argstreams))))))))

;Exercise 3.51

(define (show x)
  (display x)
  (display "\n")
  x)
(define x
  (stream-map show
              (stream-enumerate-interval 0 10)))
(stream-ref x 5)
(stream-ref x 7)

;Exercise 3.52: Consider the sequence of expressions
(display "value of sum --> ")
(newline)
(define sum 0)
(define (accum x) (set! sum (+ x sum)) sum)
(define seq
  (stream-map accum
              (stream-enumerate-interval 1 20)))
sum
(define y (stream-filter even? seq))
sum
(define z
  (stream-filter (lambda (x) (= (remainder x 5) 0))
                 seq))
sum
(stream-ref y 7)
sum
(display-stream z)
sum

;sieve of Erastosthenes
(define (divisible? x y) (= 0 (remainder x y)))
(define (integers-starting-from n)
  (cons n (memo-proc (λ () (integers-starting-from (+ n 1))))))

;(define integers (integers-starting-from 1))

(define (sieve stream)
  (cons
   (stream-car stream)
   (memo-proc (λ () (sieve (stream-filter
                            (lambda (x)
                              (not (divisible? x (stream-car stream))))
                            (stream-cdr stream)))))))

(define primes (sieve (integers-starting-from 2)))

(= 233 (stream-ref primes 50))

(define ones (cons 1 (memo-proc (λ () ones))))

(define (add-streams s1 s2) (stream-map1 + s1 s2))

(define integers
  (cons 1 (memo-proc (λ () (add-streams ones integers)))))

(stream-ref integers 50)

(define fibs
  (cons
   1
   (memo-proc (λ () (cons 1 (memo-proc (λ () (add-streams (stream-cdr fibs) fibs))))))))

(= 34 (stream-ref fibs 9))

(define primes1
  (cons
   2
   (memo-proc (λ () (stream-filter prime? (integers-starting-from 3))))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (sqr (stream-car ps)) n) true)
          ((divisible? n (stream-car ps)) false)
          (else (iter (stream-cdr ps)))))
  (iter primes1))

(stream-ref primes1 7)

;Exercise 3.53

(define s (cons 1 (memo-proc (λ () (add-streams s s)))))

(= (stream-ref s 19) 2)
(/ (stream-ref s 23)  (stream-ref s 19))

;Exercise  3.54

(define (mul-streams s1 s2) (stream-map1 * s1 s2))

(define factorials
  (cons 1  (memo-proc (λ () (mul-streams factorials (integers-starting-from 2))))))

(stream-ref factorials 3)

;Exercise 3.55

(define (partial-sums s)
  (cons (stream-car s)  (memo-proc (λ () (add-streams (partial-sums s) (stream-cdr s))))))

(stream-ref (partial-sums integers) 4)

;Exercise 3.56

(define (merge s1 s2)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (cond ((< s1car s2car)
                  (cons
                   s1car
                   (memo-proc (λ () (merge (stream-cdr s1) s2)))))
                 ((> s1car s2car)
                  (cons
                   s2car
                   (memo-proc (λ () (merge s1 (stream-cdr s2))))))
                 (else
                  (cons
                   s1car
                   (memo-proc (λ () (merge (stream-cdr s1)
                                           s2))))))))))

(define S (cons 1 (memo-proc (λ () (merge (scale-stream S 2)  (merge (scale-stream S 3) (scale-stream S 5)))))))

(stream-ref S 10)
(display "Displaying hamming numbers")
(newline)
(display-stream-until (create-iterator 10) S)

;Exercise 3.59

(define (zip s1 s2)
  (cons (cons (stream-car s1) (stream-car s2)) (memo-proc (λ () (zip (stream-cdr s1) (stream-cdr s2) )))))

;(define (integrate-series cs)
; (stream-map (λ (xpr) (/  (car xpr) (+ 1 (cdr xpr))) )  (zip cs (integers-starting-from 0)) ))

(define (integrate-series cs)
  (stream-map1 (λ (x y) (exact->inexact (/ x y))) cs integers ))

(define exp-series
  (cons 1 (memo-proc (λ () (integrate-series exp-series)))))

;(define cosine-series (cons-stream 1 ⟨??⟩))
;(define sine-series (cons-stream 0 ⟨??⟩))

(define cosine-series
  (cons 1 (memo-proc (λ () (scale-stream (integrate-series sine-series) -1) ))))

(define sine-series
  (cons 0 (memo-proc (λ ()  (integrate-series cosine-series) ))))

;Exercise 3.60

(define (mul-series s1 s2)
  (cons (* (stream-car s1) (stream-car s2)) (memo-proc (λ () (add-streams (scale-stream (stream-cdr s2) (stream-car s1) ) (stream-cdr s1))))))

(display "sin^2 x + cos^2 x")
(newline)
(display-stream-until (create-iterator 10) (add-streams (mul-series sine-series sine-series) (mul-series cosine-series cosine-series)))


;Exercise 3.61
(define (invert-unit-series s)
  (define x (cons 1 (memo-proc (λ () (scale-stream (mul-series (stream-cdr s) x) -1)))))
  x)

;Exercise 3.62
(define (div-series s1 s2)
  (mul-series s1 (invert-unit-series s2)))

(define tan-series (div-series sine-series cosine-series))

(display "tan x")
(newline)
(display-stream-until (create-iterator 10) tan-series)

;;;;;;;;

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons
     1.0
     (memo-proc (λ () (stream-map (lambda (guess) (sqrt-improve guess x))
                                  guesses)))))
  guesses)

(display-stream-until (create-iterator 10) (sqrt-stream 2))

(define (pi-summands n)
  (cons (/ 1.0 n)
        (memo-proc (λ () (stream-map - (pi-summands (+ n 2)))))))

(define pi-stream
  (scale-stream (partial-sums (pi-summands 1)) 4))

(display-stream-until (create-iterator 10) pi-stream)

(define (euler-transform s)
  (let ((s0 (stream-ref s 0)) ; 
        (s1 (stream-ref s 1)) ; Sn
        (s2 (stream-ref s 2))) ; Sn+1
    (cons (- s2 (/ (sqr (- s2 s1))
                   (+ s0 (* -2 s1) s2)))
          (memo-proc (λ () (euler-transform (stream-cdr s)))))))

(display-stream-until (create-iterator 10) (euler-transform pi-stream))

;stream of streams

(define (make-tableau transform s)
  (cons s (memo-proc (λ () (make-tableau transform (transform s))))))

(define (accelerated-sequence transform s)
  (stream-map stream-car (make-tableau transform s)))

(display-stream-until (create-iterator 10)
                      (accelerated-sequence euler-transform pi-stream))

;Exercise 3.64
(define (stream-limit s tolerance)
  (let ((s1 (stream-ref s 0))
        (s2 (stream-ref s 1)))
    (cons s1  (memo-proc  (λ () (if (> tolerance (abs (- s1 s2)))  (cons s2 null) (stream-limit (stream-cdr s) tolerance) ))))))

(define (sqrt1 x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(display-stream (sqrt1 2 0.00000000001))

;Exercise 3.65

(define (ln2-summands n)
  (cons (/ 1.0 n)
        (memo-proc (λ () (stream-map - (ln2-summands (+ n 1)))))))

(define ln2-stream
  (partial-sums (ln2-summands 1)) )

(display-stream-until (create-iterator 10)
                      (accelerated-sequence euler-transform ln2-stream))

;;;;;

(define (interleave s1 s2)
  (if (stream-null? s1)
      s2
      (cons (stream-car s1)
            (memo-proc  (λ () (interleave s2 (stream-cdr s1)))))))

(define (pairs s t)
  (cons
   (if (list? (stream-car s)) (append (stream-car s) (list (stream-car t))) (list (stream-car s) (stream-car t)))
   (memo-proc  (λ () (interleave
                      (stream-map (lambda (x) (if (list? (stream-car s)) (append (stream-car s) (list x)) (list (stream-car s) x)))
                                  (stream-cdr t))
                      (pairs (stream-cdr s) (stream-cdr t)))))))


(display-stream-until (create-iterator 10) (pairs integers integers))

;;;;;;;;
;Exercise 3.67

(define (tri-interleave s1 s2 s3)
  (cond
    ((stream-null? s1) (interleave s2 s3))
    ((stream-null? s2) (interleave s1 s3))
    ((stream-null? s3) (interleave s1 s2))
    (else (cons (stream-car s1)
                (memo-proc  (λ () (tri-interleave s2 s3 (stream-cdr s1))))))))

(define (all-pairs s t)
  (cons
   (list (stream-car s) (stream-car t))
   (memo-proc  (λ ()
                 (tri-interleave
                  (stream-map (lambda (x) (list (stream-car s) x))
                              (stream-cdr t))
                  (stream-map (lambda (x) (list x (stream-car t) ))
                              (stream-cdr s))
                  (all-pairs (stream-cdr s) (stream-cdr t)))))))

(display-stream-until (create-iterator 10) (all-pairs integers integers))


(define (triples s t u)
  (cons
   (list (stream-car s) (stream-car t) (stream-car u))
   (memo-proc  (λ ()
                 (interleave
                  (pairs (stream-map (lambda (x) (list (stream-car s) x))
                                     (stream-cdr t))
                         (stream-cdr u))
                  (triples (stream-cdr s) (stream-cdr t) (stream-cdr u)))))))

(display-stream-until (create-iterator 10) (triples integers integers integers))
(display-stream-until (create-iterator 6) (stream-filter  (λ (trp) (= (+ (expt (car trp) 2) (expt (cadr trp) 2)) (expt (caddr trp) 2) ) ) (triples integers integers integers)))
;(stream-filter  (λ (trp) (= (+ (expt (car trp) 2) (expt (cadr trp) 2)) (expt (caddr trp) 2) ) ) (triples integers integers integers))


;Exercise 3.70

; s1 s2 are assumed to be streams of pairs
(define (merge-weighted s1 s2 w)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
         (let ((s1car (stream-car s1))
               (s2car (stream-car s2)))
           (if (<= (w s1car) (w s2car))
               (cons
                s1car
                (memo-proc (λ () (merge-weighted (stream-cdr s1) s2 w))))
               (cons
                s2car
                (memo-proc (λ () (merge-weighted s1 (stream-cdr s2) w)))))
           ))))

(define (weighted-pairs s t w)
  (cons
   (if (list? (stream-car s)) (append (stream-car s) (list (stream-car t))) (list (stream-car s) (stream-car t)))
   (memo-proc  (λ () (merge-weighted
                      (stream-map (lambda (x) (if (list? (stream-car s)) (append (stream-car s) (list x)) (list (stream-car s) x)))
                                  (stream-cdr t))
                      (weighted-pairs (stream-cdr s) (stream-cdr t) w)
                      w)))))

(display-stream-until (create-iterator 10) (weighted-pairs integers integers (λ (pr) (+ (car pr) (cadr pr)))))

(define S1 (stream-filter (λ (x) (not (or (divisible? x 3) (divisible? x 2) (divisible? x 5) )) ) integers))

(display-stream-until (create-iterator 10) (weighted-pairs S1 S1 (λ (pr) (+ (* 2 (car pr) (* 3 (cadr pr) (* 5 (car pr) (cadr pr)) )) ))))

;Exercise 3.71 - Ramanujan numbers
(define sum-of-cubes (stream-map (λ (pr) (+ (expt (car pr) 3) (expt (cadr pr) 3)) ) (weighted-pairs integers integers (λ (pr) (+ (expt (car pr) 3) (expt (cadr pr) 3)) ))))
(define ramanujan-nums  (stream-map cdr
                                    (stream-filter (λ (pr) (car pr))
                                                   (stream-map1 (λ (n1 n2)  (if (= n1 n2) (cons #t n1) (cons #f null) ))
                                                                sum-of-cubes
                                                                (stream-from sum-of-cubes 1)))))

(display-stream-until (create-iterator 7) ramanujan-nums)

;Exercise 3.72

(define sum-of-squares (stream-map (λ (pr) (cons (+ (expt (car pr) 2) (expt (cadr pr) 2)) pr) ) (weighted-pairs integers integers (λ (pr) (+ (expt (car pr) 2) (expt (cadr pr) 2)) ))))

(define sum-of-squares-3-ways  (stream-map (λ (resPr) (let ((rawPr (cdr resPr))) (list (caar rawPr) (cdar rawPr) (cdadr rawPr) (cdaddr rawPr))))
                                           (stream-filter (λ (pr) (car pr))
                                                          (stream-map1 (λ (n1 n2 n3)  (if (= (car n1) (car n2) (car n3)) (list #t n1 n2 n3) (cons #f null) ))
                                                                       sum-of-squares
                                                                       (stream-from sum-of-squares 1)
                                                                       (stream-from sum-of-squares 2)))))

(display-stream-until (create-iterator 7) sum-of-squares-3-ways)

;;;;;;;;;;

(define (integral integrand initial-value dt)
  (define int
    (cons initial-value
          (memo-proc (λ () (add-streams (scale-stream integrand dt)
                                        int)))))
  int)

;Exercise 3.73


(define (RC R C dt)
  (define (create-circuit v0 i)
    (add-streams (integral (scale-stream i (expt C -1)) v0 dt)  (scale-stream i R)))
  create-circuit)

(define RC1 (RC 5 1 0.5))
(display-stream-until (create-iterator 7) (RC1 0.2 integers))

;Exercise 3.74

(define (list->stream xs)
  (foldr (lambda (x ys)
           (cons x (memo-proc (λ () ys))))
         the-empty-stream 
         xs))

(define sense-data 
  (list->stream 
   (list 1 2 1.5 1 0.5 -0.1 -2 -3 -2 -0.5 0.2 3 4 0 0 0 0 0 -1 0 0 0 0 1 0 0)))

(define (sign-change-detector currN prevN)
  (cond
    ((and (< prevN 0) (> currN 0) ) 1 )
    ((and (> prevN 0) (< currN 0) ) -1 )
    (else 0)))

(define (make-zero-crossings input-stream last-value)
  (cons
   (sign-change-detector
    (stream-car input-stream)
    last-value)
   (memo-proc (λ () (make-zero-crossings
                     (stream-cdr input-stream)
                     (stream-car input-stream))))))

(define zero-crossings
  (make-zero-crossings sense-data 0))

(define zero-crossings1
  (stream-map1 sign-change-detector
               sense-data
               (cons (stream-car sense-data) (memo-proc (λ () sense-data)))))

(display-stream-until (create-iterator 16) zero-crossings)
(display-stream-until (create-iterator 16) zero-crossings1)

;Exercise 3.75
;(define (make-zero-crossings2 input-stream last-value init)
;  (let ((avpt (/ (+ (stream-car input-stream)
;                    last-value)
;                 2)))
;    (cons
;     (sign-change-detector avpt last-value)
;     (memo-proc (λ () (make-zero-crossings2
;                       (stream-cdr input-stream) avpt #f))))))
;
;(display-stream-until (create-iterator 16) (make-zero-crossings2 sense-data 0 #t))

(define (make-zero-crossings2 input-stream last-value last-avg)
  (let ((avpt (/ (+ (stream-car input-stream) last-value) 2)))
    (cons (sign-change-detector avpt last-avg)
          (memo-proc (λ () (make-zero-crossings2 (stream-cdr input-stream)
                                                 (stream-car input-stream)
                                                 avpt))))))

(define smoothed-zero-crossings 
  (make-zero-crossings2 sense-data 
                        0
                        0))

(display-stream-until (create-iterator 16) smoothed-zero-crossings)

(define (smooth s)
  (stream-map1 (λ (n1 n2) (exact->inexact (/ (+ n1 n2) 2))) s (cons 0.0 (memo-proc (λ () s)))))

(display-stream-until (create-iterator 16) (smooth sense-data))

(display-stream-until (create-iterator 16) (make-zero-crossings (smooth sense-data) 0))

;;;;;;;;;;;;;;

(define (delayed-integral delayed-integrand initial-value dt)
  (define int
    (cons
     initial-value
     (memo-proc (λ () (let ((integrand (force delayed-integrand)))
                        (add-streams (scale-stream integrand dt) int))))))
  int)

(define (solve f y0 dt)
  (define y (delayed-integral  (memo-proc (λ ()  dy)) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve (lambda (y) y)
                   1
                   0.001)
            1000)

;Exercise 3.77

(define (delayed-integral1 delayed-integrand initial-value dt)
  (cons
   initial-value
   (memo-proc (λ ()
                (let ((integrand (force delayed-integrand)))
                  (if (stream-null? integrand)
                      the-empty-stream
                      (delayed-integral1 (memo-proc (λ () (stream-cdr integrand)))
                                         (+ (* dt (stream-car integrand))
                                            initial-value)
                                         dt)))))))

; dy/dt = f(y)

(define (solve1 f y0 dt)
  (define y (delayed-integral1  (memo-proc (λ ()  dy)) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve1 (lambda (y) y)
                    1
                    0.001)
            1000)

;Exercise 3.78

(define (solve-2nd a b dt y0 dy0)
  (define dy (delayed-integral1  (memo-proc (λ ()  ddy)) dy0 dt))
  (define y (delayed-integral1  (memo-proc (λ ()  dy)) y0 dt))
  (define ddy (add-streams (scale-stream dy a) (scale-stream y b) ))
  y
  )

(stream-ref (solve-2nd 1 0 0.001 1 1) 1000)

;Exercise 3.79

(define (solve-2nd-generic f dt y0 dy0 )
  (define dy (delayed-integral1  (memo-proc (λ ()  ddy)) dy0 dt))
  (define y (delayed-integral1  (memo-proc (λ ()  dy)) y0 dt))
  (define ddy (stream-map1 f  dy   y  ))
  y
  )

(stream-ref (solve-2nd-generic (λ (y dy) dy) 0.001 1 1) 1000)

;Exercise 3.80

(define (RLC R L C dt)
  (λ (vc0 il0)
    (define vc (delayed-integral1  (memo-proc (λ ()  dvc)) vc0 dt))
    (define il (delayed-integral1  (memo-proc (λ ()  dil)) il0 dt))
    (define dvc (scale-stream il (/ -1 C)))
    
    (define tmp1 (scale-stream il (/ (* -1 R) L)))
    (define tmp2 (scale-stream vc (/ 1 L)))
    
    (define dil (add-streams tmp1 tmp2))
    
    (stream-map1 (λ (v i) (cons v i)) vc il)
    ))

(define RLC1 (RLC 1 1 0.2  0.1))

(display-stream-until (create-iterator 7) (RLC1 10 0))


;;;;;;;;;

(define random-init 13)
(define a 37)
(define b 23)
(define m 957)

;(define (rand-update x)
;  (modulo (+ (* a x) b ) m))

(define initial-seed 317)
(random-seed initial-seed)
(define (rand-update x)
  (random  (expt 2 31) )
  ;(random x)
  )

(define random-numbers
  (cons
   random-init
   (memo-proc (λ () (stream-map rand-update random-numbers)))))



(display-stream-until (create-iterator 10) random-numbers)

(define (map-successive-pairs f s)
  (cons
   (f (stream-car s) (stream-car (stream-cdr s)))
   (memo-proc (λ () (map-successive-pairs f (stream-cdr (stream-cdr s)))))))

(define cesaro-stream
  (map-successive-pairs
   (lambda (r1 r2) (= (gcd r1 r2) 1))
   random-numbers))





(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons
     (/ passed (+ passed failed))
     (memo-proc (λ ()
                  (monte-carlo
                   (stream-cdr experiment-stream) passed failed)))))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define pi
  (stream-map
   (lambda (p) (sqrt (/ 6 p)))
   (monte-carlo cesaro-stream 0 0)))

(stream-ref pi 3000000)
;
;(define rand-gen (let ((x random-init))
;                   (lambda (cmd)
;                     (cond
;                       ((eq? 'generate cmd) (set! x (rand-update x)) x)
;                       ((eq? 'reset cmd) (λ (y) (set! x y) x))
;                       (else (error "Unknown Cmd " cmd))
;                       ))))
;
;(define r1 (rand-gen 'generate))
;r1
;(rand-gen 'generate)
;(rand-gen 'generate)
;((rand-gen 'reset) random-init)
;(define rr1 (rand-gen 'generate))
;rr1
;(display "Exercise 3.6 ***   ")
;
;(= r1 rr1)
;(newline)


;Exercise 3.82

(define (create-circle-pred-stream rps cntr r x1 y1 x2 y2 )
  (stream-map (λ (rp) (or (< (+ (sqr (- (car rp) (car cntr))) (sqr (- (cadr rp) (cdr cntr)))) (sqr r))
          (= (+ (sqr (- (car rp) (car cntr))) (sqr (- (cadr rp) (cdr cntr)))) (sqr r)))) rps))



(define (integral-experiments cntr r x1 x2 y1 y2)
  (create-circle-pred-stream
                (all-pairs (stream-filter (λ (x) (and (>= x x1) (<= x x2))) random-numbers) (stream-filter (λ (y) (and (>= y y1) (<= y y2))) random-numbers))
                cntr r x1 y1 x2 y2)
  )

(define (integral-estimate-stream cntr r x1 x2 y1 y2)
  (let ((d1 (* (- y2 y1) (- x2 x1))))
  (stream-map (λ (p) (* p d1 ))
              (monte-carlo (integral-experiments cntr r x1 x2 y1 y2) 0 0))))

;FIX-IT - hangs miserably , missing a delay somewhere

;(stream-ref (integral-estimate-stream (cons 5 7) 1 4 6 6 8) 2 )

(define (stream-withdraw balance amount-stream)
(cons
balance
 (memo-proc (λ () (stream-withdraw (- balance (stream-car amount-stream))
(stream-cdr amount-stream))))))


