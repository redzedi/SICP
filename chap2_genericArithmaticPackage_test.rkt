#lang racket


 
;(parameterize ([current-namespace (make-base-namespace)])
;  (load"chap2_genericArithmaticPackage.rkts"))

(require "chap2_genericArithmaticPackage.rkt")
;(require 'genericArithmatic)

;test
(install-scheme-number-package)
(install-rational-package)
(install-rectangular-package)
(install-polar-package)
(install-complex-package)

;Exercise 2.77

;3+4i
(define z1 (make-complex-from-real-imag 3 4))
z1
(magnitude z1)

(angle z1)

;Exercise 2.78

(add (make-scheme-number 3) (make-scheme-number 4))

(add 3 4)

;Exercise 2.79

(equ? 4 4)

(equ? 4 41)

(equ? (make-rational 1 2) (make-rational 2 4))
(define z1-polar (make-complex-from-mag-ang (magnitude z1) (angle z1)))
(real-part z1-polar)
(imag-part z1-polar)
(equ? z1 z1-polar)

;Exercise 2.80

(zero? 0)

(zero? (make-rational 0 2))

(zero? (make-complex-from-real-imag 0 0))

(zero? z1)

(add z1 5)

(add 5 z1)

(multi-add 1 2 3 4 5 6)

(define z2 (make-complex-from-real-imag 5 6))

(define z3 (make-complex-from-real-imag 7 8))

(multi-add z1 z2 z3)

(multi-add z1 z2 3 (make-rational 4 2))

(multi-add 3 z1 z2 3 (make-rational 4 2))

;Exercise 2.83 raise function integer -->  rational --> real --> complex

(raise (raise (raise 5)))

;(multi-add z1 3 (make-rational 7 3))

;in this exercise the type tower is
; integer -->  rational -->  complex

(compare-types 'scheme-number 'rational)

(compare-types 'complex 'scheme-number )

(compare-types 'scheme-number 'scheme-number)

(add (make-rational 1 1) (make-rational 3 2))

(add 1 (make-rational 3 2))

(project z1)

(raise (project z1))

(project (project z1))

(equ? z1 (raise (project z1)))

(define z4 (make-complex-from-real-imag 3 0))

(define z5 (make-complex-from-mag-ang 3 0))

(raise (project z4))

(raise (project z5))

(equ? z4 (raise (project z4)))

(equ? z5 (raise (project z5)))

(drop z5)

;Exercise 2.85 - expect result 1
(add (make-rational 1 2) (make-rational 1 2))

(define z6 (make-complex-from-real-imag (make-rational 3 2) 2))

(magnitude z6)

(angle z6)

(define z7 (make-complex-from-mag-ang (make-rational 4776742944770517 1125899906842624) 0.7854))

z7
(real-part z7)

(imag-part z7)

(define z8 (make-complex-from-mag-ang (make-rational 4776742944770517 1125899906842624) (make-rational 7074254294673575 9007199254740992 )))

z8
(real-part z8)

(imag-part z8)

(define z9 (make-complex-from-real-imag (make-rational 3 2) (make-rational 3 2)))

z9
    
(magnitude z9)

(angle z9)