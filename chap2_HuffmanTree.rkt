#lang racket
;Huffman Tree

(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))


(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))

(define (decode bits tree)
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

;Exercis 2.67

(decode sample-message sample-tree)

(define (create-huffman-tree nds)
  (cond
    ( (null? nds) '())
    ( (null? (cdr nds)) (car nds))
    (else (create-huffman-tree (adjoin-set (make-code-tree (car nds) (cadr nds))  (cddr nds))))
    ))

(define created-tree1 (create-huffman-tree  (foldl adjoin-set '() (list (make-leaf 'A 4) (make-leaf 'B 2) (make-leaf 'D 1)  (make-leaf 'C 1) )) ))

(define created-tree11 (create-huffman-tree  (make-leaf-set (list (list 'A 4) (list 'B 2) (list 'D 1)  (list 'C 1) )) ))

(decode sample-message created-tree1 )

(decode sample-message created-tree11 )

;Exercise 2.68

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (encode-symbol symb tree)
  (define (choose-branch-and-encoding subTr)
    (cond
      ((ormap (λ (x) (eq? x symb)) (symbols (left-branch subTr))) (cons 0 (left-branch subTr)))
      ((ormap (λ (x) (eq? x symb)) (symbols (right-branch subTr))) (cons 1 (right-branch subTr)))
      (else (error "unknown symbol " symb))
      ))
  (cond
    ( (null? tree) (error symb "symbol not found in the tree"))
    ( (and (leaf? tree) (eq? symb (car (symbols tree)))) '())
    (else (let ((code-n-branch (choose-branch-and-encoding tree))) (cons (car code-n-branch) (encode-symbol symb (cdr code-n-branch))) ))
    ))

(define sample-cleartext '(A D A B B C A))

(encode sample-cleartext sample-tree)

(encode sample-cleartext created-tree1)

;Exercise 2.69

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

(define successive-merge create-huffman-tree)

(define created-tree2  (generate-huffman-tree (list (list 'A 4) (list 'B 2) (list 'D 1)  (list 'C 1) )) )

(decode sample-message created-tree2 )

;Exercise 2.70
(define song-tree  (generate-huffman-tree (list (list 'A 2) (list 'GET 2) (list 'SHA 3)  (list 'WAH 1) (list 'BOOM 1) (list 'JOB 2) (list 'NA 16) (list 'YIP 9) )) )

(define song-cleartext (list 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'GET 'A 'JOB 'SHA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'NA 'WAH 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'YIP 'SHA 'BOOM))

(encode song-cleartext song-tree)

(define encoded-song '(1   1   1   1   1   1   1   0   0   1   1   1   1   0   1   1   1   0   0   0   0   0   0   0   0   0   1   1   1   1   1   1   1   0   0   1   1   1   1   0   1   1   1   0   0   0   0   0   0   0   0   0   1   1   0   1   1   1   0   1   0   1   0   1   0   1   0   1   0   1   0   1   0   1   0   1   1   1   0   1   1   0   1   0))

(decode encoded-song song-tree)
