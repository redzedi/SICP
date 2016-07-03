#lang racket

(require math/number-theory)

;(require (planet "sicp.ss" ("soegaard" "sicp.plt" 2 1)))

(require sicp-pict)

(define (gcd1 a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))


(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (/ d g))))

(define (numer x) (car x))
(define (denom x) (cdr x))


;;;;;;;;;  public interface

(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y))
               (* (numer y) (denom x)))
            (* (denom x) (denom y))))
(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))
(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer y))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

;;;;;;;;;

(define one-fifth (make-rat 5 25))

(print-rat one-fifth)

;Exercise 2.2

(define (make-point x y) (cons x y))

(define (x-point p) (car p))

(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))

(define (start-segment l) (car l))

(define (end-segment l) (cdr l))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))



(define (midpoint-segment l)
  (let (
        (p1 (start-segment l))
        (p2 (end-segment l)))
    (make-point (/ (+ (x-point p1) (x-point p2) ) 2) (/ (+ (y-point p1) (y-point p2) ) 2) )))

(define mp1 (midpoint-segment (make-segment (make-point 0 0) (make-point 2 2) )))

(print-point mp1)

;Exercise 2.3

(define (cartesian-length l)
  (let (
        (x1 (x-point (start-segment l)))
        (y1 (y-point (start-segment l)))
        (x2 (x-point (end-segment l)))
        (y2 (y-point (end-segment l)))
        )
    (sqrt (abs (+ (expt (- x2 x1) 2) (expt (- y2 y1) 2) )) )))

(define (make-rectangle l b) (cons l b))

(define (length-rectangle r) (car r) )

(define (breadth-rectangle r) (cdr r) )

(define (area-rectangle r) (* (cartesian-length (length-rectangle r)) (cartesian-length (breadth-rectangle r))))

(define (perimeter-rectangle r) (+ (* 2 (cartesian-length (length-rectangle r))) (* 2 (cartesian-length (breadth-rectangle r))) ))

(define l1 (make-segment (make-point 0 0) (make-point 2 0)))

(define b1 (make-segment (make-point 0 0) (make-point 0 2)))

(define r1 (make-rectangle l1 b1))

(area-rectangle r1)

(perimeter-rectangle r1)

; TODO - different representation of rectangle ??

;Exercise 2.4

(define (cons2 x y)
  (lambda (m) (m x y)))

(define (car2 z)
  (z (lambda (p q) p) ))

(define (cdr2 z)
  (z (lambda (p q) q) ))

; (= (car2 (cons2 x y)) x)
;
; (= (car2 (lambda (m) (m x y)) ) x)
;
; (= ((lambda (m) (m x y)) (lambda (p q) p) ) x)
;
; (= ( ((lambda (p q) p) x y)  ) x)
;
; (= x x) ;QED

;Exercise 2.5
(define (cons1 a b)
  (define (inner1 prod )
    (define (inner2 b)
      (define (inner2-iter q cnt)
        (if (= (gcd q b) 1)
            cnt
            (inner2-iter (/ q b) (+ cnt 1))))
      (inner2-iter prod 0))
    (lambda (x)
      (cond ((= x 0) (inner2 2))
            ( (= x 1) (inner2 3) )
            (else (error "Expected: 0 or 1 passed: " x)))
      )
    
    )
  (inner1 (* (expt 2 a) (expt 3 b))))

(define (car1 pr) (pr 0))
(define (cdr1 pr) (pr 1))

(car1 (cons1 5 2))

(cdr1 (cons1 5 2))

;Exercise 2.6

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

;(((add-1 zero) (lambda (x) (+ x 1))) 0)
; f --> a process of combining elements in a given context
; 0 th element of that context
;(define one (add-1 zero))
;
;(define one (add-1 (lambda (f) (lambda (x) x))))
;
;(define one ((lambda (n) (lambda (f) (lambda (x) (f ((n f) x)))) ) (lambda (f) (lambda (x) x))))
;
;(define one  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) x)) f) x)))) )
;
;(define one  (lambda (f) (lambda (x) (f ( (lambda (x) x) x)))) )
;
;(define one  (lambda (f) (lambda (x) (f x))) )
;
;(define two (add-1 one))
;
;(define two ((lambda (n)  (lambda (f) (lambda (x) (f ((n f) x)))) ) (lambda (f) (lambda (x) (f x)))))
;
;(define two  (lambda (f) (lambda (x) (f (((lambda (f) (lambda (x) (f x))) f) x))) ))
;
;(define two  (lambda (f) (lambda (x) (f ( (lambda (x) (f x)) x))) ))
;
;(define two  (lambda (f) (lambda (x) (f  (f x))) ))

;Exercise 2.7

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

(define (make-interval a b) (cons a b))

(define (upper-bound x) (cdr x))

(define (lower-bound x) (car x))

;Exercise 2.8

(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))

(sub-interval (make-interval 3.45 2.55) (make-interval 2.2 1.8))

;Exercise 2.9

(define (width-interval x) (/ (- (upper-bound x) (lower-bound x)) 2))

;let x,y,z be intervals such that z = y -x
; (width-interval z)
; (/ (- (upper-bound z) (lower-bound z)) 2)
; (/ (- (- (upper-bound y) (lower-bound x)) (- (lower-bound y) (upper-bound x))) 2)
; (/ (+  (upper-bound y)  (* -1 (lower-bound x)) (* -1 (lower-bound y)) (upper-bound x)) 2)
;(/ (+  (- (upper-bound y) (lower-bound y))  (-  (upper-bound x) (lower-bound x)) ) 2)

;(+ (/  (- (upper-bound y) (lower-bound y)) 2)  (/ (-  (upper-bound x) (lower-bound x)) 2) )

;(+ (width-interval y)  (width-interval x) )

;Exercise 2.10

;Exercise 2.11

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

;Exercise 2.12

(define (make-center-percent c p)
  (make-center-width c (/ (* c p) 100)))

(define (percent-interval i)
  (* (/ (width i) (center i) )100))

(make-center-percent 6.8 10)

(center (make-center-percent 6.8 10))

(percent-interval (make-center-percent 6.8 10))

(width (make-center-percent 6.8 10))

(define (par1 r1 r2)
  (div-interval (mul-interval r1 r2)
                (add-interval r1 r2)))

(define (par2 r1 r2)
  (let ((one (make-interval 1 1)))
    (div-interval
     one (add-interval (div-interval one r1)
                       (div-interval one r2)))))

(define res1 (make-center-percent 6.8 10))

(define res2 (make-center-percent 4.7 5))

(define res121 (par1 res1 res2))

(define res122 (par2 res1 res2))

(center res121)
(percent-interval res121)

(center res122)
(percent-interval res122)

(define intA (make-center-percent 6.8 1))

(newline)
(display "intA ")
(center intA)
(percent-interval intA)

(define intB (make-center-percent 4.7 2))

(newline)
(display "intB ")
(center intB)
(percent-interval intB)

(define intAbyB (div-interval intA intB))

(newline)
(display "intAbyB ")
(center intAbyB)
(percent-interval intAbyB)

(define intAbyA (div-interval intA intA))

(newline)
(display "intAbyA ")
(center intAbyA)
(percent-interval intAbyA)

;insight - in the div result the error percents are getting added

;Exercise 2.17

(define (last-pair xs)
  (define (last-pair-iter prev tail)
    (if (null? tail)
        prev
        (last-pair-iter (car tail) (cdr tail))))
  (last-pair-iter null xs))


(last-pair (list 23 72 149 34))

;Exercise 2.18

(define (reverse xs)
  (define (reverse-iter currXs revs)
    (if (null? currXs)
        revs          ;emptying stack
        (reverse-iter (cdr currXs) (cons (car currXs) revs ) ))) ;pushing onto stack (iterative calling) - stack state is the state variable
  (reverse-iter xs null))

(reverse (list 1 4 9 16 25))

;Exercise 2.19

(define (cc amount coin-values)
  (define (no-more? xs) (null? xs) )
  (define (except-first-denomination xs)
    (cdr xs))
  (define (first-denomination xs) (car xs) )
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount
                (except-first-denomination
                 coin-values))
            (cc (- amount
                   (first-denomination
                    coin-values))
                coin-values)))))

(define us-coins (list 50 25 10 5 1))
(define uk-coins (list 100 50 20 10 5 2 1 0.5))

(cc 100 us-coins)

;Exercise 2.20

(define (same-parity x . z)
  (define (parity-iter-outer parity?)
    (define (parity-iter currZs revZs)
      (cond   ( (null? currZs) (reverse revZs) )
              ( (parity? (car currZs)) (parity-iter (cdr currZs) (cons (car currZs) revZs)) )
              (else (parity-iter (cdr currZs) revZs ) )))
    (parity-iter z (list x)))
  (if (even? x) (parity-iter-outer even?) (parity-iter-outer odd?)))

(same-parity 1 2 3 4 5)

(same-parity 2 3 4 5 6 7 8)

;Exercise 2.21

(define (square-list items)
  (if (null? items)
      null
      (cons (sqr (car items)) (square-list (cdr items))) ))

(define (square-list1 items)
  (map sqr items))

(square-list (list 1 2 3 4))

(square-list1 (list 1 2 3 4))

;Exercise 2.22

(define (square-list3 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons (sqr (car things))
                    answer))))
  (iter items null))

;the things and answer list act as 2 stacks items popped off one pushed onto other and finally when answer is returned the elements are reverse to things

(define (square-list4 items)
  (define (iter things answer)
    (if (null? things)
        answer
        (iter (cdr things)
              (cons answer
                    (sqr (car things))))))
  (iter items null))

(square-list4 (list 1 2 3 4))

;Exercise 2.23

(define (for-each proc xs)
  (cond ( (null? xs) #t)
        (else (proc (car xs)) (for-each proc (cdr xs)) )))

(for-each (lambda (x)
            (newline)
            (display x))
          (list 57 321 88))


;Exercise 2.25
(car (cdr (car (cdr (cdr '( 1 3 ( 5 7) 9))))))

(car ( car '((7))))

(car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr '( 1 (2 (3 (4 (5 ( 6 7))))))) )))))))))))

;Exercise 2.26

(define x (list 1 2 3))
(define y (list 4 5 6))

(append x y)

;'(1 2 3 4 5 6)

(cons x y)

; '( (1 2 3) 4 5 6)

(list x y)

; '( (1 2 3) (4 5 6) )

;Exercise 2.27

(define (deep-reverse xs)
  (define (reverse-iter currXs revs)
    (cond ((null? currXs) revs)
          ((not (pair? currXs)) currXs)
          (else (reverse-iter (cdr currXs) (cons (reverse-iter (car currXs) null) revs ) )))) ;pushing onto stack (iterative calling) - stack state is the state variable
  (reverse-iter xs null))

(define x11 (list (list 1 2) (list 3 4)))

(reverse x11)

(deep-reverse x11)

;Exercise 2.28

(define (fringe xs)
  (cond ((null? xs) null)
        ((not(pair? xs)) (list xs) )
        (else (append (fringe (car xs)) (fringe (cdr xs))))))
(fringe x11)
(fringe (list x11 x11))

;Exercise 2.29

(define (make-mobile left right)
  (list left right))

(define (make-branch length structure)
  (list length structure))

(define (right-branch mobile ) (car (cdr mobile)))
(define (branch-structure branch ) (car (cdr branch)))

;alternative constructor -start

;(define (make-mobile left right)
;(cons left right))
;
;(define (make-branch length structure)
;(cons length structure))
;
;(define (branch-structure branch ) (cdr branch))
;(define (right-branch mobile ) (cdr mobile))

;alternative constructor - end

(define (left-branch mobile ) (car mobile))
(define (branch-length branch ) (car branch))

(define (total-branch-weight branch)
  (let (
        (struk (branch-structure branch))
        )
    (if (pair? struk)
        (+ (total-branch-weight (left-branch struk)) (total-branch-weight (right-branch struk)))
        struk)))

(define (total-weight mobile)
  (+ (total-branch-weight (left-branch mobile)) (total-branch-weight (right-branch mobile)) ))

(define mobile-a (make-mobile (make-branch 2 3) (make-branch 2 3)))

(define mobile-b (make-mobile (make-branch 2 3) (make-branch 4 5)))

(branch-structure (make-branch 2 3))
(pair? (branch-structure (make-branch 2 3)))

(= 3 (total-branch-weight (make-branch 2 3)))

(= 6 (total-weight mobile-a))

(= 8 (total-weight mobile-b))

(define (branch-torque br) (* (branch-length br) (total-branch-weight br)))

;2-legged recursion
(define (branch-balanced? br)
  (if (pair? (branch-structure br))
      (mobile-balanced? (branch-structure br))
      #t))
(define (mobile-balanced? mobile)
  (and
   (= (branch-torque (right-branch mobile)) (branch-torque (left-branch mobile)))
   (branch-balanced? (right-branch mobile))
   (branch-balanced? (left-branch mobile))))

(mobile-balanced? mobile-a)

(mobile-balanced? mobile-b)

(define mobile-c (make-mobile (make-branch 5 mobile-a) (make-branch 3 mobile-b)))

(= 14 (total-weight mobile-c))

(not (mobile-balanced? mobile-c))

(define mobile-d (make-mobile (make-branch 10 mobile-a) (make-branch 12 5)))

(mobile-balanced? mobile-d)

;Exercise 2.30

(define (square-tree tr)
  (cond
    ( (null? tr) null)
    ( (not (pair? tr)) (sqr tr))
    (else (cons (square-tree (car tr)) (square-tree (cdr tr))))))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

(define (square-tree1 tr)
  (map (lambda (sub-tr)
         (if (pair? sub-tr)
             (square-tree1 sub-tr)
             (sqr sub-tr)))
       tr))

(square-tree1
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;Exercise 2.31

(define (tree-map fn tr)
  (map (lambda (sub-tr)
         (if (pair? sub-tr)
             (tree-map fn sub-tr)
             (fn sub-tr))) tr))

(define (square-tree2 tree) (tree-map sqr tree))

(square-tree2
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))

;Exercise 2.32

(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (lambda (x) (cons (car s) x)) rest)))))

(subsets (list 1 2 3))

;Exercise 2.33
(define (reverse1 xs)
  (foldl cons null xs))

(define (map1 p sequence)
  (foldl (lambda (x y) (if (pair? y)
                           (append1 y (list (p x)))
                           (cons (p x) y) )) null sequence))

(define (append1 seq1 seq2)
  (foldl cons seq2 (reverse1 seq1)))

(define (length1 sequence)
  (foldl (lambda (x y) (+ y 1)) 0 sequence))

(map1 (lambda (x) (* 2 x)) (list 1 2 3))

(append1 (list 1 2 3) (list 4 5 6))

(length1 (list 1 2 3))

;Exercise 2.34

(define (horner-eval x coefficient-sequence)
  (foldl (lambda (this-coeff higher-terms) (+ (* higher-terms x) this-coeff))
         0
         (reverse coefficient-sequence)))

(= 79 (horner-eval 2 (list 1 3 0 5 0 1)))

;Exercise 2.35
;
;(define (count-leaves t)
;  (foldl + 0 (map (λ (x) (cond
;                           ((pair? x) (count-leaves x))
;                           ((null? x) 0 )
;                           (else 1))) t)))

(define (enumerate-tree tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (list tree))
        (else (append (enumerate-tree (car tree))
                      (enumerate-tree (cdr tree))))))

(define (count-leaves t)
  (foldl + 0 (map (lambda (x) 1)
                  (enumerate-tree t))))

(define x-tree-1 (cons (list 1 2) (list 3 4)))

(count-leaves x-tree-1)

;Exercise 2.36

(define (accumulate op start xs)
  (if (null? xs)
      start
      (op (car xs) (accumulate op start (cdr xs)))))

(accumulate + 0 (list 1 2 3))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      null
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))

(accumulate-n + 0 (list (list 1 2 3) (list 4 5 6) (list 7 8 9) (list 10 11 12)))

;Exercise 2.37

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  ; (display m)
  ;(display v)
  (map (λ (mr) (dot-product mr v)) m))

(define (transpose mat)
  (accumulate-n cons null mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (λ (mr) (matrix-*-vector  cols mr)) m)))

(define vec-1 (list 1 1 1))
(define mat-1 (list (list 1 2 3) (list 4 5 6)))

(define mat-2 (list (list 1 1) (list 1 1) (list 1 1)))

(matrix-*-vector mat-1 vec-1)
(transpose mat-1)

(transpose mat-2)

(matrix-*-matrix mat-1 mat-2)

;Exercise 2.38

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

;Exercise 2.39

(define (reverse11 sequence)
  (accumulate (lambda (x acc) (append acc (list x))) null sequence))

(define (reverse21 sequence)
  (fold-left (lambda (x y) (cons y x)) null sequence))

(reverse11 (list 1 2 3))

(reverse21 (list 1 2 3))

;accumulate is fold-right
; the op signature is always (left or right fold) (lambda (x acc) ??? )

(define (enumerate-interval start end)
  (define (inner-iter end1 res)
    (if (< end1 start)
        res
        (inner-iter (- end1 1) (cons end1 res) )))
  (inner-iter end null))

(define (flatmap proc seq)
  (accumulate append null (map proc seq)))


;(pair-factors-with-subfactors 6)

(map
 (λ (pr) (let ((p1 (car pr)) (p2 (cadr pr)) ) (list p1 p2 (+ p1 p2))))
 (filter
  (λ (pr) (prime? (+ (car pr) (cadr pr))))
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 6))))

;Exercise 2.40

(define (unique-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j) (list i j))
                  (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(map
 (λ (pr) (let ((p1 (car pr)) (p2 (cadr pr)) ) (list p1 p2 (+ p1 p2))))
 (filter
  (λ (pr) (prime? (+ (car pr) (cadr pr))))
  (unique-pairs 6)))

;Exercise 2.41

(define (all-triples-with-sum n s)
  (let (
        (seq (enumerate-interval 1 n)))
    (filter (λ (tr) (= s (+ (car tr) (cadr tr) (car (cddr tr)))))
            (flatmap (λ (x) (map (λ (prx) (list x (car prx) (cadr prx))) (unique-pairs (-  x 1)))) seq))))

(all-triples-with-sum 6 7)

;Exercise 2.42
(define empty-board null)
;(define (safe? k positions)
;  (let ( (idx-poss (map (λ (x y) (cons x y)) (enumerate-interval 1 (length positions)) positions)))
;    (let ( (kval (foldl (λ (xpr acc) (if (= k (car xpr)) (cdr xpr) acc)) null idx-poss)) )
;      (display "kval --> ")
;      (display kval)
;      (newline)
;      (foldl (λ (x acc) (if (= k (car x)) acc (or acc (not (or (= kval (cdr x)) (not (= (abs (- kval (cdr x))) (abs (- k (car x))))) ))))) #f idx-poss))
;    ; #t)
;    ))

(define (safe? k positions)
  
  (let ( (kvalpr  (filter (λ (pr)   (= k (car pr))) (map (λ (idx poss) (cons idx poss)) (enumerate-interval 1  (length positions)  ) positions )) ))
    
    
    (let ( (kval (if (pair? kvalpr) (cdr (car kvalpr)) -1 )) )
      
      
      (define (safe-iter? cols)
        (cond
          ((null? cols) #t)
          (  (or (= kval (car cols)) (= (abs (- kval (car cols))) (length cols))) #f )
          (else (safe-iter? (cdr cols)) )))
      (safe-iter? (map cdr (filter (λ (pr)  (not (= k (car pr)))) (map (λ (idx poss) (cons idx poss)) (enumerate-interval 1  (length positions)  ) positions ))))
      ; #t)
      )))

(define  (adjoin-position
          new-row k rest-of-queens)
  (append rest-of-queens (list new-row)))

(define (queens board-size)
  (define (queen-cols k) ; (list (list row-positions) )
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position
                    new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))

(define nQueensSol (queens 8))

nQueensSol

(length nQueensSol)

;;;;;; Picture language


(define (painter1) (display "A"))

(define (beside1 p1 p2) (λ () (p1) (display "   ") (p2)))

((beside1 painter1 (beside1 painter1 painter1)))


(define (flipped-pairs painter)
  (let ((painter2 (beside painter (flip-vert painter))))
    (below painter2 painter2)))


(define (right-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (right-split painter (- n 1))))
        (beside painter (below smaller smaller)))))

;Exercise 2.44
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))



(define (corner-split painter n)
  (if (= n 0)
      painter
      
      (let ((up (up-split painter (- n 1)))
            (right (right-split painter (- n 1))))
        (let ((top-left (beside up up))
              (bottom-right (below right right))
              (corner (corner-split painter (- n 1))))
          (beside (below painter top-left)
                  (below bottom-right corner))))))

(define (square-limit painter n)
  (let ((quarter (corner-split painter n)))
    (let ((half (beside (flip-horiz quarter) quarter)))
      (below (flip-vert half) half))))

(paint (square-limit einstein 5))

(define (square-of-four tl tr bl br)
  (lambda (painter)
    (let ((top (beside (tl painter) (tr painter)))
          (bottom (beside (bl painter) (br painter))))
      (below bottom top))))

(define (flipped-pairs1 painter)
  ((square-of-four identity flip-vert identity flip-vert) painter))

(paint (flipped-pairs1 einstein))

(define (square-limit1 painter n)
  ((square-of-four (λ (p) (flip-horiz (corner-split p n)))
                   (λ (p) (corner-split p n))
                   (λ (p) (flip-vert (flip-horiz (corner-split p n))))
                   (λ (p) (flip-vert (corner-split p n)))) painter))

(paint (square-limit1 einstein 5))

;Exercise 2.45
(define (split c1 c2)
  (define  (split-inner p n)
    (if (= n 0)
        p
        (let ((smaller (split-inner p (- n 1))))
          (c1 p (c2 smaller smaller)))))
  split-inner)

(define right-split1 (split beside below))
(define up-split1 (split below beside))

(paint (right-split1 einstein 5))

(paint (up-split1 einstein 5))

;Exercise 2.46

(define (make-vect x y)
  (λ (idx) (cond
             ((= idx 0) x)
             ((= idx 1) y)
             (else (error "Expected: 0 or 1 passed: " x)))))

(define (xcor-vect v) (v 0))

(define (ycor-vect v) (v 1))

(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2)) (+ (ycor-vect v1) (ycor-vect v2)) ))

(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2)) (- (ycor-vect v1) (ycor-vect v2)) ))

(define (scale-vect s v)
  (make-vect (* s (xcor-vect v)) (* s (ycor-vect v))))

(define (print-vect v) (display "( ") (display (xcor-vect v)) (display " , ") (display (ycor-vect v)) (display " )"))

;Exercise 2.47: Here are two possible constructors for frames:

(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame1 origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

(define (origin-frame f) (car f))

(define (edge1-frame f) (cadr f))

(define (edge2-frame f)  (caddr f))

(define (print-frame f) (display " Frame[ ")  (print-vect (origin-frame f)) (display " , ")  (print-vect (edge1-frame f)) (display " , ") (print-vect (edge2-frame f)) (display " ]  ") )

(print-frame (make-frame (make-vect 0 0) (make-vect 1 1) (make-vect -1 1)))

(define (origin-frame1 f) (car f))

(define (edge1-frame1 f) (cadr f))

(define (edge2-frame1 f)  (cdr (cdr f)))

(define (print-frame1 f) (display " Frame[ ")  (print-vect (origin-frame1 f)) (display " , ")  (print-vect (edge1-frame1 f)) (display " , ") (print-vect (edge2-frame1 f)) (display " ]  ") )

(newline)
(print-frame1 (make-frame1 (make-vect 0 0) (make-vect 1 1) (make-vect -1 1)))

(define (frame-coord-map frame)
  (lambda (v)
    (add-vect
     (origin-frame frame)
     (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
               (scale-vect (ycor-vect v) (edge2-frame frame))))))

(newline)
;should equal (0,1)
(print-vect ((frame-coord-map (make-frame (make-vect 0 1) (make-vect 1 2) (make-vect -1 2))) (make-vect 0 0)))

(define (segments->painter segment-list)
  (lambda (frame)
    (for-each
     (lambda (segment)
       (draw-line1
        ((frame-coord-map frame)
         (start-segment1 segment))
        ((frame-coord-map frame)
         (end-segment1 segment))))
     segment-list)))


;Exercise 2.48

(define (make-segment1 v1 v2)
  (λ (idx)
    (cond
      ( (= idx 0) v1)
      ( (= idx 1) v2 )
      (else error "Expected 0 or 1 Actual: "x))))

(define (start-segment1 s) (s 0))

(define (end-segment1 s) (s 1))

(define (print-segement1 s) (display "Segment[ ") (print-vect (start-segment1 s)) (display " , ") (print-vect (end-segment1 s)) (display " ] ") )

(require graphics/graphics)

(open-graphics)
(define vp (open-viewport "SICP pict language" 500 500))

(define (draw-line1 v1 v2)
  (define (vector->posn v) (make-posn (xcor-vect v) (ycor-vect v)))
  ((draw-line vp) (vector->posn v1) (vector->posn v2)))

(draw-line1 (make-vect 50 50) (make-vect 50 250))
(draw-line1 (make-vect 50 50) (make-vect 250 50))
(draw-line1 (make-vect 50 250) (make-vect 250 250))
(draw-line1 (make-vect 250 50) (make-vect 250 250))

(define screen-frame (make-frame (make-vect 50 50) (make-vect 200 0)  (make-vect 0 200)  ))

(define (my-paint pntr)
  (pntr screen-frame))
;(draw-line1 (make-vect 0 0) (make-vect 500 500))
;Exercise 2.49

;(my-paint (segments->painter (list (make-segment1 (make-vect 0 0 ) (make-vect 0 1)) (make-segment1 (make-vect 1 0) (make-vect 1 1)) (make-segment1 (make-vect 0 0) (make-vect 1 0)) (make-segment1 (make-vect 0 1) (make-vect 1 1)) )) )

;(my-paint (segments->painter (list (make-segment1 (make-vect 0 0 ) (make-vect 1 1)) (make-segment1 (make-vect 1 0 ) (make-vect 0 1)) )))

;(my-paint (segments->painter (list (make-segment1 (make-vect 0.5 0 ) (make-vect 0 0.5)) (make-segment1 (make-vect 0.5 0) (make-vect 1 0.5)) (make-segment1 (make-vect 1 0.5) (make-vect 0.5 1)) (make-segment1 (make-vect 0.5 1) (make-vect 0 0.5)) )) )




;wave figure
(define wave-painter (segments->painter (list
                                         (make-segment1
                                          (make-vect 0.006 0.840)
                                          (make-vect 0.155 0.591))
                                         (make-segment1
                                          (make-vect 0.006 0.635)
                                          (make-vect 0.155 0.392))
                                         (make-segment1
                                          (make-vect 0.304 0.646)
                                          (make-vect 0.155 0.591))
                                         (make-segment1
                                          (make-vect 0.298 0.591)
                                          (make-vect 0.155 0.392))
                                         (make-segment1
                                          (make-vect 0.304 0.646)
                                          (make-vect 0.403 0.646))
                                         (make-segment1
                                          (make-vect 0.298 0.591)
                                          (make-vect 0.354 0.492))
                                         (make-segment1
                                          (make-vect 0.403 0.646)
                                          (make-vect 0.348 0.845))
                                         (make-segment1
                                          (make-vect 0.354 0.492)
                                          (make-vect 0.249 0.000))
                                         (make-segment1
                                          (make-vect 0.403 0.000)
                                          (make-vect 0.502 0.293))
                                         (make-segment1
                                          (make-vect 0.502 0.293)
                                          (make-vect 0.602 0.000))
                                         (make-segment1
                                          (make-vect 0.348 0.845)
                                          (make-vect 0.403 0.999))
                                         (make-segment1
                                          (make-vect 0.602 0.999)
                                          (make-vect 0.652 0.845))
                                         (make-segment1
                                          (make-vect 0.652 0.845)
                                          (make-vect 0.602 0.646))
                                         (make-segment1
                                          (make-vect 0.602 0.646)
                                          (make-vect 0.751 0.646))
                                         (make-segment1
                                          (make-vect 0.751 0.646)
                                          (make-vect 0.999 0.343))
                                         (make-segment1
                                          (make-vect 0.751 0.000)
                                          (make-vect 0.597 0.442))
                                         (make-segment1
                                          (make-vect 0.597 0.442)
                                          (make-vect 0.999 0.144)))))

;(my-paint wave-painter)

(define (transform-painter painter origin corner1 corner2)
  (lambda (frame)
    (let ((m (frame-coord-map frame)))
      (let ((new-origin (m origin)))
        (painter (make-frame
                  new-origin
                  (sub-vect (m corner1) new-origin)
                  (sub-vect (m corner2) new-origin)))))))

(define (flip-vert1 painter)
  (transform-painter painter
                     (make-vect 0.0 1.0) ; new origin
                     (make-vect 1.0 1.0) ; new end of edge1
                     (make-vect 0.0 0.0))) ; new end of edge2

;(my-paint (flip-vert1 wave-painter))

(define (shrink-to-upper-right1 painter)
  (transform-painter
   painter (make-vect 0.5 0.5)
   (make-vect 1.0 0.5) (make-vect 0.5 1.0)))

;(my-paint (shrink-to-upper-right1 (flip-vert1 wave-painter)))

(define (rotate90-1 painter)
  (transform-painter painter
                     (make-vect 1.0 0.0)
                     (make-vect 1.0 1.0)
                     (make-vect 0.0 0.0)))

;(my-paint (rotate90-1 wave-painter))

(define (squash-inwards1 painter)
  (transform-painter painter
                     (make-vect 0.0 0.0)
                     (make-vect 0.65 0.35)
                     (make-vect 0.35 0.65)))

;(my-paint (squash-inwards1 wave-painter))

(define (my-beside painter1 painter2)
  (let ((split-point (make-vect 0.5 0.0)))
    (let ((paint-left
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            split-point
            (make-vect 0.0 1.0)))
          (paint-right
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.0)
            (make-vect 0.5 1.0))))
      (lambda (frame)
        (paint-left frame)
        (paint-right frame)))))

;(my-paint (my-beside (flip-vert1 wave-painter) wave-painter))

;Exercise 2.50
; painter datatype is frame -> void
; transform-painter datatype is (painter , v1, v2 v3) -> painter
; painter combinator (flip-vert , rotate90 ...) datatype is (painter) -> painter
(define (my-compose-combinators c1 c2) (λ (p) (c1 (c2 p))))

(define rotate180-1  (my-compose-combinators rotate90-1 rotate90-1))
(define rotate270-1  (my-compose-combinators rotate90-1 rotate180-1))
(define flip-horiz1 (my-compose-combinators flip-vert1 rotate180-1))

;(my-paint  wave-painter)
;(my-paint (flip-horiz1 wave-painter))

;Exercise 2.51
(define (my-below1 painter1 painter2)
  (let ((split-point (make-vect 0.0 0.5)))
    (let ((paint-bottom
           (transform-painter
            painter1
            (make-vect 0.0 0.0)
            (make-vect 1.0 0.0)
            split-point
            ))
          (paint-top
           (transform-painter
            painter2
            split-point
            (make-vect 1.0 0.5)
            (make-vect 0.0 1.0))))
      (lambda (frame)
        (paint-bottom frame)
        (paint-top frame)
        ))))

;(my-paint (my-below1 wave-painter wave-painter))

(define (my-below2 p1 p2)
  (rotate90-1 (my-beside (rotate270-1 p1) (rotate270-1 p2))))

(my-paint (my-below2 wave-painter wave-painter))


(define (memq item x)
(cond ((null? x) false)
((eq? item (car x)) x)
(else (memq item (cdr x)))))

;Exercise 2.53

(list 'a 'b 'c)
; '(a b c)
(list (list 'george))
'( (george) )
(cdr '((x1 x2) (y1 y2)))
; '((y1 y2)) 
(cadr '((x1 x2) (y1 y2)))
; '(y1 y2)

(pair? (car '(a short list)))
; #f
(memq 'red '((red shoes) (blue socks)))
; #f
(memq 'red '(red shoes blue socks))
;'(red shoes blue socks)

;Exercise 2.54

(define (equal? p1 p2)
  (cond ((and (null? p1) (null? p2)) #t)
        ((or (null? p1) (null? p2)) #f)
        ((and (pair? p1) (pair? p2))
         (and (equal? (car p1) (car p2))
              (equal? (cdr p1) (cdr p2))))
        ((or (pair? p1) (pair? p2)) #f)
        (else (eq? p1 p2))))

(equal? '(this is a list) '(this is a list))

(equal? '(this is a list) '(this (is a) list))

(car ''abracadabra)