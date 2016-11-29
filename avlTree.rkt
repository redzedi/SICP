#lang racket

(provide (all-defined-out))

;TODO implement remove method
;TODO implement balancing

(define (make-tree compare-fn)

  ;'(value height parent left right)
  (define (make-node k v) 
    (let ((nd (mcons v (mcons 0 (mcons '() (mcons '() '()))))))

        (define (get-key) k)
        (define (get-value) (mcar nd))
      (define (get-height) (mcar (mcdr nd)))
      (define (get-parent) (mcar (mcdr (mcdr nd))))
      (define (get-left-child) (mcar (mcdr (mcdr (mcdr nd)))))
      (define (get-right-child) (mcdr (mcdr (mcdr (mcdr nd)))))

      (define (set-value newVal) (set-mcar! nd newVal))
      (define (set-parent p) (set-mcar! (mcdr (mcdr nd)) p))

      (define (update-height h)
        (set-mcar! (mcdr nd) (+ h 1))
        (if (not (null? (get-parent))) (((get-parent) 'update-height) (get-height)) null))

      (define (set-left-child chld)
        ((chld 'set-parent) dispatch)
        (set-mcar! (mcdr (mcdr (mcdr nd))) chld)
        (update-height ((chld 'get-height))))

      (define (set-right-child chld)
        ((chld 'set-parent) dispatch)
        (set-mcdr! (mcdr (mcdr (mcdr nd))) chld)
        (update-height ((chld 'get-height))))

      
      (define (dispatch mthdNm)
        (cond
          ((eq? 'get-key mthdNm) get-key)
          ((eq? 'get-value mthdNm) get-value)
          ((eq? 'get-parent mthdNm) get-parent)
          ((eq? 'get-height mthdNm) get-height)
          ((eq? 'get-left-child mthdNm) get-left-child)
          ((eq? 'get-right-child mthdNm) get-right-child)
          ((eq? 'set-value mthdNm) set-value)
          ((eq? 'set-parent mthdNm) set-parent)
          ((eq? 'update-height mthdNm) update-height)
          ((eq? 'set-left-child mthdNm) set-left-child)
          ((eq? 'set-right-child mthdNm) set-right-child)
          (else (error "UNKNOWN method called on tree node - " mthdNm))
          ))
      dispatch)
  )

  (define (lookup-inner currNode key1)
        (let ((compare-key (compare-fn key1 ((currNode 'get-key)))))
          (cond
            ((= 0 compare-key) (cons #t currNode))
            ((<  compare-key 0) (if (null? ((currNode 'get-left-child))) (cons #f (currNode 'set-left-child)) (lookup-inner ((currNode 'get-left-child))  key1)))
            (else (if (null? ((currNode 'get-right-child))) (cons #f (currNode 'set-right-child)) (lookup-inner ((currNode 'get-right-child))  key1)))
                )))

  (let ((trE (mcons '**tree** '())))

    (define (lookup key)
      (if (null? (mcdr trE))
          #f
          (let ((res (lookup-inner (mcdr trE) key)))
        (if (car res) (((cdr res) 'get-value)) #f ))))
    
    (define (insert k v)
      (if (null? (mcdr trE))
           (set-mcdr! trE (make-node k v))
          (let ((res (lookup-inner (mcdr trE) k)))
        (if (car res) (((cdr res) 'set-value) v) ((cdr res) (make-node k v)) )))
      (cons 'tree dispatch)) ;for chaining etc

    (define (dispatch mthdNm)
      (cond
        ((eq? 'insert mthdNm) insert)
        ((eq? 'lookup mthdNm) lookup)
        (else (error "UNKOWN method called on tree - " mthdNm))))
 (cons 'tree dispatch))
  )

(define (tree? trE) (and (pair? trE) (eq? 'tree  (car trE))))