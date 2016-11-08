#lang racket

;Exercise 3.1

(define (make-accumulator init-val)
  (λ (x)
    (set! init-val (+ x init-val))
    init-val))

(define A (make-accumulator 5))
(= 15 (A 10))
(= 25 (A 10))

;Exercise 3.2

(define (make-monitored f)
  (let ((num-of-calls 0))
    (λ (args)
      (cond
        ((eq? 'how-many-calls? args) num-of-calls)
        ((eq? 'reset-count args) (set! num-of-calls 0) num-of-calls)
        (else (set! num-of-calls (+ 1 num-of-calls)) (f args))))))

(define s (make-monitored sqrt))
(= (s 100) 10)
(= (s 'how-many-calls?) 1)


;Exercise 3.3

(define (make-account balance pwd)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  
  (define (create-security-wrapper wrong-access-count)
    (define (security-wrapper gp f)
      (λ (a)
        (if (eq? gp pwd)
            (begin (set! wrong-access-count 0) (f a))
            (begin (set! wrong-access-count (+ 1 wrong-access-count)) (if (< wrong-access-count 4) "Incorrect Password" "Call the cops !!!")))))
    security-wrapper)
  
  (define (create-dispatch security-wrapper)
    (define (dispatch given-pwd m)
      (cond ((eq? m 'withdraw) (security-wrapper given-pwd withdraw))
            ((eq? m 'deposit) (security-wrapper given-pwd deposit))
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch)
  (create-dispatch (create-security-wrapper 0)))

(define peter-acc (make-account 100 'secret-password))

(= ((peter-acc 'secret-password 'withdraw) 40) 60)

;Exercise 3.7

(define (make-joint acc origP newP)
  
  (define (create-security-wrapper wrong-access-count)
    (define (security-wrapper gp m)
      
      (if (eq? gp newP)
          (begin (set! wrong-access-count 0) (acc origP m))
          (begin (set! wrong-access-count (+ 1 wrong-access-count)) (if (< wrong-access-count 4) "Incorrect Password" "Call the cops !!!"))))
    security-wrapper)
  
  (create-security-wrapper 0)
  )

(define paul-acc
  (make-joint peter-acc 'secret-password 'rosebud))

(= ((paul-acc 'rosebud 'withdraw) 40) 20)

;throws exception
((peter-acc 'some-other-password 'deposit) 50)

((peter-acc 'secret-password 'deposit) 50)

((peter-acc 'some-other-password 'deposit) 50)

((peter-acc 'secret-password 'deposit) 50)

((peter-acc 'some-other-password 'deposit) 50)

((peter-acc 'secret-password 'deposit) 50)

;((acc 'some-other-password 'deposit) 50)

((peter-acc 'secret-password 'deposit) 50)

((peter-acc 'some-other-password 'deposit) 50)
((peter-acc 'some-other-password 'deposit) 50)
((peter-acc 'some-other-password 'deposit) 50)

;"Call the cops"
((peter-acc 'some-other-password 'deposit) 50)

;Monte Carlo method

(define random-init 13)
(define a 37)
(define b 23)
(define m 957)

(define (rand-update x)
  (modulo (+ (* a x) b ) m))

(define (rand-update1 m x)
  (modulo (+ (* a x) b ) m))

(define rand (let ((x random-init))
               (lambda ()
                 (set! x (rand-update x))
                 x)))

(define rand-range (let ((x random-init))
                     (lambda (r)
                       (set! x  (rand-update1 r x))
                       x)))

(define (estimate-pi trials)
  (sqrt (/ 6 (monte-carlo trials cesaro-test))))

(define (cesaro-test)
  (= (gcd (rand ) (rand )) 1))


(define (monte-carlo trials experiment)
  (define (iter trials-remaining trials-passed)
    (cond ((= trials-remaining 0)
           (exact->inexact
            (/ trials-passed trials))
           )
          ((experiment)
           (iter (- trials-remaining 1)
                 (+ trials-passed 1)))
          (else
           (iter (- trials-remaining 1)
                 trials-passed))))
  (iter trials 0))

(estimate-pi 3000000)

;version 2

(define (estimate-pi1 trials)
  (sqrt (/ 6 (random-gcd-test trials random-init))))

(define (random-gcd-test trials initial-x)  
  (define (iter trials-remaining trials-passed x)
    (let ((x1 (rand-update x)))
      (let ((x2 (rand-update x1)))
        (cond ((= trials-remaining 0)
               (/ trials-passed trials))
              ((= (gcd x1 x2) 1)
               (iter (- trials-remaining 1)
                     (+ trials-passed 1)
                     x2))
              (else
               (iter (- trials-remaining 1)
                     trials-passed
                     x2))))))
  (iter trials 0 initial-x))

(estimate-pi1 3000000)

(define (estimate-integrals p x1 y1 x2 y2 trials)
  (* (monte-carlo trials p)
     (* (- y2 y1) (- x2 x1))))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low  (random range ) )))

(define (create-circle-pred cntr r x1 y1 x2 y2 )
  (λ ()
    (let ((rp (cons (random-in-range x1 x2)  (random-in-range y1 y2 ))))
      (or (< (+ (sqr (- (car rp) (car cntr))) (sqr (- (cdr rp) (cdr cntr)))) (sqr r))
          (= (+ (sqr (- (car rp) (car cntr))) (sqr (- (cdr rp) (cdr cntr)))) (sqr r))) )))

(estimate-integrals (create-circle-pred (cons 5 7) 1 4 6 6 8) 4 6 6 8 3000000)

;Exercise 3.6
;how to perist state between method calls

(define rand-gen (let ((x random-init))
                   (lambda (cmd)
                     (cond
                       ((eq? 'generate cmd) (set! x (rand-update x)) x)
                       ((eq? 'reset cmd) (λ (y) (set! x y) x))
                       (else (error "Unknown Cmd " cmd))
                       ))))

(define r1 (rand-gen 'generate))
r1
(rand-gen 'generate)
(rand-gen 'generate)
((rand-gen 'reset) random-init)
(define rr1 (rand-gen 'generate))
rr1

(= r1 rr1)

;Exercise 3.17

(define (mpairEq? p1 p2)
  (if (not (and (mpair? p1) (mpair? p2)))
      (eq? p1 p2)
      (and (mpairEq? (mcar p1) (mcar p2)) (mpairEq? (mcdr p1) (mcdr p2)))))

(define (pairEq? p1 p2)
  (if (not (and (pair? p1) (pair? p2)))
      (eq? p1 p2)
      (and (pairEq? (car p1) (car p2)) (pairEq? (cdr p1) (cdr p2)))))

(define (count-pairs ps )
  (define (count-pairs-inner x seens)
    (display seens)(newline)
    (if (not (pair? x))
        (cons 0 seens)
        (if (ormap (λ (s) (eq? s x)) seens )
            (cons 0 seens)
            (let ((carRes (count-pairs-inner (car x) (cons x seens)) ))
              (let ((cdrRes (count-pairs-inner (cdr x) (cdr carRes) ) ))
                (cons (+ 1 (car carRes) (car cdrRes)) (cons x seens) )))
            )))
  (car (count-pairs-inner ps (list))))

(define x1 (list 'a 'b))

(define z1 (cons x1 x1))

;when inserting something by ref in a datastructure , the ref alone is stored and can be retrieved and matched by eq?

;to use cache - 1. use direction sensitive evaluation  2.  

;correct answer is 3 - as per books implementation (intentional bug) the answer is 5
(= 3 (count-pairs z1 ))

;Exercise 3.18
(define (last-pair x)
  (if (null? (mcdr x)) x (last-pair (mcdr x))))

(define (make-cycle x)
  (set-mcdr! (last-pair x) x)
  x)


(define mlst1 (mcons 5 (mcons 4 (mcons 3 (mcons 2 (mcons 1 '()))))))

;TODO - detect cycle

(define (has-cycle mls)
  ( define (has-cycle-inner xs acc)
     (cond
       ((null? xs) #f)
       ((ormap (λ (s) (eq? s xs)) acc) #t)
       (else (has-cycle-inner (mcdr xs) (cons xs acc)))
       ))
  (has-cycle-inner mls (cons '() '())))

(not (has-cycle mlst1))

(has-cycle (make-cycle mlst1))

;Exercise 3.19 - no extra memory

(define (has-cycle1 mls)
  (define (get-third-pointer p3) (if (not(mpair? (mcdr p3))) null (if (not (mpair? (mcdr (mcdr p3)))) null (mcdr (mcdr (mcdr p3))))))
  ( define (has-cycle-inner xs  p3)
     ; (display p3)(newline)
     (cond
       ((or (null? p3) (null? xs)) #f)
       ((and (not (null? p3)) (eq? xs p3))  #t)
       (else (has-cycle-inner (mcdr xs)  (get-third-pointer p3)  ))
       ))
  (has-cycle-inner mls  (get-third-pointer mls)))

(display "Exercise 3.19") (newline)
(define mlst2  (mcons 5 (mcons 4 (mcons 3 (mcons 2 (mcons 1 '()))))))
(not (has-cycle1 mlst2))


(has-cycle1 (make-cycle mlst2))

;Queue implementation

(define (front-ptr queue) (mcar queue))
(define (rear-ptr queue) (mcdr queue))
(define (set-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (mcons '() '()))

(define (front-queue queue)
  (if (empty-queue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (front-ptr queue))))

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))


(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else (set-front-ptr! queue (mcdr (front-ptr queue)))
              queue)))

;Exercise 3.21

(define (print-queue q) (display (front-ptr q))(newline))

(define q1 (make-queue))
(insert-queue! q1 'a)
(print-queue q1)

(insert-queue! q1 'b)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)
(delete-queue! q1)
(print-queue q1)

;Exercise 3.22

(define (make-queue1)
  (let ((front-ptr '() )
        (rear-ptr '() ))

    (define (set-front-ptr!  item)
  (set! front-ptr item))
(define (set-rear-ptr!  item)
  (set! rear-ptr item))

(define (empty-queue?)
  (null? front-ptr))



(define (front-queue)
  (if (empty-queue? )
      (error "FRONT called with an empty queue" )
      front-ptr))

(define (insert-queue!  item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? )
           (set-front-ptr!  new-pair)
           (set-rear-ptr!  new-pair)
           front-ptr)
          (else
           (set-mcdr! rear-ptr  new-pair)
           (set-rear-ptr!  new-pair)
           front-ptr))))


(define (delete-queue!)
  (cond ((empty-queue? )
         (error "DELETE! called with an empty queue"))
        (else (set-front-ptr!  (mcdr front-ptr ))
              front-ptr)))
    
    (define (dispatch m)
      (cond
        ((eq? 'empty-queue? m) empty-queue?)
        ((eq? 'front-queue m) front-queue)
        ((eq? 'insert-queue! m) insert-queue!)
        ((eq? 'delete-queue! m) delete-queue!)
        (else "UNKNOWN METHOD CALLED "m)
        ))
    dispatch))

(define q2 (make-queue1))
((q2 'insert-queue!)  'a)
;(print-queue q2)

((q2 'insert-queue!)  'b)

((q2 'delete-queue!))

((q2 'delete-queue!))

;Exercise 3.23 - Deque


(define (dq-front-ptr queue) (mcar queue))
(define (dq-rear-ptr queue) (mcdr queue))
(define (set-dq-front-ptr! queue item)
  (set-mcar! queue item))
(define (set-dq-rear-ptr! queue item)
  (set-mcdr! queue item))

(define (empty-dequeue? queue)
  (null? (dq-front-ptr queue)))

(define (make-dequeue) (mcons '() (mcons '() '())))

(define (front-dequeue queue)
  (if (empty-dequeue? queue)
      (error "FRONT called with an empty queue" queue)
      (mcar (dq-front-ptr queue))))

(define (rear-dequeue queue)
  (if (empty-dequeue? queue)
      (error "FRONT called with an empty queue" queue)
      (car (dq-rear-ptr queue))))

(define (rear-insert-dequeue! queue item)
  (let ((new-pair (mcons item (mcons '() '()))))
    (cond ((empty-dequeue? queue)
           (set-dq-front-ptr! queue new-pair)
           (set-dq-rear-ptr! queue new-pair)
           queue)
          
           (else
           (set-mcdr! (mcdr (dq-rear-ptr queue)) new-pair)
           (set-mcar! (mcdr new-pair) (dq-rear-ptr queue))
           (set-dq-rear-ptr! queue new-pair)
           queue))))

(define (front-insert-dequeue! queue item)
  (let ((new-pair (mcons item (mcons '() '()))))
    (cond ((empty-dequeue? queue)
           (set-dq-front-ptr! queue new-pair)
           (set-dq-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (mcdr new-pair) (dq-front-ptr queue))
           (set-mcar!  (mcdr (dq-front-ptr queue))  new-pair)
           (set-dq-front-ptr! queue new-pair)
           queue))))


(define (front-delete-dequeue! queue)
  (cond ((empty-dequeue? queue)
         (error "DELETE! called with an empty queue" queue))
        
        (else
         (set-dq-front-ptr! queue (mcdr (mcdr (dq-front-ptr queue))))
          (set-mcar!  (mcdr (dq-front-ptr queue))  '())    
              queue)))

(define (rear-delete-dequeue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        
        (else
         (set-dq-rear-ptr! queue (mcar (mcdr (dq-rear-ptr queue))))
         (set-mcdr!  (mcdr (dq-rear-ptr queue))  '())
              queue)))

(define (print-deque dq)
  (if (null? dq) (newline)
      (begin ( display (if (null? (mcar dq)) " "  (mcar dq))) (display " ") (print-deque (if (null? (mcdr dq)) null (mcdr (mcdr dq)))))))

(define q3 (make-dequeue))
(print-deque (dq-front-ptr q3))
(rear-insert-dequeue! q3 'a)
(print-deque (dq-front-ptr q3))
  (front-insert-dequeue! q3 'b)
(print-deque (dq-front-ptr q3))
  (front-delete-dequeue! q3 )
(print-deque (dq-front-ptr q3))
  (front-insert-dequeue! q3 'c)
(print-deque (dq-front-ptr q3))
  (rear-delete-dequeue! q3 )
(print-deque (dq-front-ptr q3))

