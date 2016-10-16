#lang racket

(define square sqr)
;language features (??)

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum: CONTENTS" datum)))

(define type-map (make-hash) )

(define (put opName implName opRef)
  (define (inner-set implMap)
    (hash-set! implMap (if (pair? implName) (car implName) implName) opRef ))
  (inner-set (hash-ref! type-map opName (make-hash))  ))

(define (get opName typeTags)
  (hash-ref (hash-ref type-map opName) (if (pair? typeTags) (car typeTags) typeTags)))

(define (apply-generic op div . args)
  (let ((type-tags (type-tag div)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc  (contents div) args)
          (error "No method for these types: APPLY-GENERIC"
                 (list op type-tags))))))

;divisions are the implementation of the interface
;interface
 (define divisions-list (make-hash) )

(define (get-division name) (hash-ref divisions-list name))

(define (getRecord division employee-name)
   (apply-generic 'get-record division employee-name))

(define (get-salary rec)
   (apply-generic 'get-salary  rec ))

(define (putRecord division . flds)
   (let
       (
        (newState (apply-generic 'put-record division flds))
        )
     (hash-set! divisions-list (type-tag newState) newState)
     ))

(define (find-employee-record divs emp-name)
    (let ((currRec (getRecord (car divs) emp-name)))
     (if (null? currRec) (find-employee-record (cdr divs) emp-name)   (contents (car currRec)) ) ))

;Implementations

;division #1

(define (install-division1-package)
  (define division1-file (attach-tag 'division1 '() ))
  (define (get-record recs name)
    
    (cond
      ((null? recs) '())
      ((eq? name (caadar recs)) (cons (car recs) (get-record (cdr recs) name)) )
      (else (display (cadar recs)) (get-record (cdr recs) name))))

  (define (put-record recs . vals) (attach-tag 'division1 (cons (attach-tag 'division1 vals) recs)))

  (define (getSalary rec) (display rec) (if (or (null? rec) (not (pair? rec))) (error "Null or not pair rec " rec) (cadar rec) ))

  
  
  (hash-set! divisions-list  'division1 division1-file)
  (put 'get-record 'division1 get-record)
  (put 'put-record 'division1 put-record)
  (put 'get-salary 'division1 getSalary)
  )

;test
(install-division1-package)

(getRecord (get-division 'division1) "suman")

(putRecord (get-division 'division1) "suman" 1000)

(get-division 'division1)

(getRecord (get-division 'division1) "suman")

(get-salary (car (getRecord (get-division 'division1) "suman")))

(find-employee-record (hash-values divisions-list) "suman")

