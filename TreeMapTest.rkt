#lang racket

(require "avlTree.rkt")

(define number-key-tree (make-tree -))

(( (cdr number-key-tree) 'insert) 5 'a)

(((cdr number-key-tree) 'insert) 7 'b)

(((cdr number-key-tree) 'insert) 6 'c)

(((cdr number-key-tree) 'insert) 8 'd)

(((cdr number-key-tree) 'insert) 2 'e)

(((cdr number-key-tree) 'insert) 3 'f)

(((cdr number-key-tree) 'insert) 4 'g)
(((cdr number-key-tree) 'insert) 4 'h)


;lookup
(display "map lookups!! ") (newline)
(((cdr number-key-tree) 'lookup) 5 )

(((cdr number-key-tree) 'lookup) 7 )

(((cdr number-key-tree) 'lookup) 6 )

(((cdr number-key-tree) 'lookup) 8 )

(((cdr number-key-tree) 'lookup) 2 )

(((cdr number-key-tree) 'lookup) 3 )

(((cdr number-key-tree) 'lookup) 4 )

(((cdr number-key-tree) 'lookup) 44 )