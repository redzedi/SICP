#lang racket

(define (test-and-set! cell)
  (if (car cell) true (begin (set-mcar! cell true) false)))

;(define (make-mutex)
;  (let ((cell (mcons false null)))
;    (define (the-mutex m)
;      (cond ((eq? m 'acquire)
;             (if (test-and-set! cell)
;                 (the-mutex 'acquire)
;                 'acquired)) ; retry
;            ((eq? m 'release) (clear! cell))))
;    the-mutex))

(define (make-mutex)
  (let ((cell (box #f)))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (box-cas! cell #f #t)
                 'acquired
                 (the-mutex 'acquire)
                 )) ; retry
            ((eq? m 'release) (box-cas! cell #t #f))))
    the-mutex))

(define (clear! cell) (set-mcar! cell false))

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (parallel-execute ps)
  (if (null? ps)
      null
      (cons (thread (car ps)) (parallel-execute (cdr ps)))))

(define x 10)

(define s (make-serializer))

(parallel-execute
 (list (s (lambda () (set! x (* x x))))
       (s (lambda () (set! x (+ x 1))))))

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((protected (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (protected withdraw))
            ((eq? m 'deposit) (protected deposit))
            ((eq? m 'balance) balance)
            (else (error "Unknown request: MAKE-ACCOUNT"
                         m))))
    dispatch))


(define (make-account-and-serializer balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (let ((balance-serializer (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) withdraw)
            ((eq? m 'deposit) deposit)
            ((eq? m 'balance) balance)
            ((eq? m 'serializer) balance-serializer)
            (else (error "Unknown request: MAKE-ACCOUNT" m))))
    dispatch))

(define (exchange account1 account2)
  (let ((difference (- (account1 'balance)
                       (account2 'balance))))
    ((account1 'withdraw) difference)
    ((account2 'deposit) difference)))

(define (serialized-exchange account1 account2)
  (let ((serializer1 (account1 'serializer))
        (serializer2 (account2 'serializer)))
    ((serializer1 (serializer2 exchange))
     account1
     account2)))


;Exercise 3.39 -- x == 101 121 100

;Exercise 3.40 -- x == 10^6 10^12 10^5 10^4
;Exercise 3.40 -- x == 10^6

;Exercise 3.47

(define (make-my-semaphore n)
  (let ((count n)
        (my-mutex (make-mutex)))
  (define (the-semaphore mthd)
    (cond
      ((eq? 'acquire mthd)
       (begin
         (my-mutex 'acquire)
         (if (> count 0)
             (begin (set! count (- count 1)) (my-mutex 'release) )
             (begin (my-mutex 'release) (the-semaphore mthd) ))))
      ((eq? 'release mthd)
       (begin
         (my-mutex 'acquire)
         (if (= n count)
             'junk
             (set! count (+ count 1)))
         (my-mutex 'release)))))
    the-semaphore))

(define (make-my-semaphore1 n)
  (let ((count n)
        (cell (box #f)))
  (define (the-semaphore mthd)
    (cond
      ((eq? 'acquire mthd)
       (begin
         (if (box-cas! cell #f #t) 'acquired (the-semaphore mthd) )
         (if (> count 0)
             (begin (set! count (- count 1)) (box-cas! cell #t #f) )
             (begin (box-cas! cell #t #f) (the-semaphore mthd) ))))
      ((eq? 'release mthd)
       (begin
         (if (box-cas! cell #f #t) 'acquired (the-semaphore mthd) )
         (if (= n count)
             'junk
             (set! count (+ count 1)))
         (box-cas! cell #t #f)))))
    the-semaphore))