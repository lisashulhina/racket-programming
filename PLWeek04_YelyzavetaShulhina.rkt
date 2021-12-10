#lang racket
; using named subprogram
(define (leibniz n)
    (if (= n 0)
        1
    ((findSign n) (leibniz (- n 1)) (/ 1.0 (+ (* n 2) 1)))
    )
)

; using modulo operator
(define (findSign n)
  (if (= (modulo n 2) 0)
      +
      -
  )
)

; test function
; testing 99, since we start from 0
(leibniz 99)

; using named odd subprogram
(define (leibnizOdd n)
    (if (= n 0)
        1
    ((findSignOdd n) (leibnizOdd (- n 1)) (/ 1.0 (+ (* n 2) 1)))
    )
)
; usign odd operator
(define (findSignOdd n)
  (if (odd? n)
      -
      +)
 )

; test function
; testing 99, since we start from 0
(leibnizOdd 99)

; using lambda
(define (leibnizLambda n2)
    (if (= n2 0)
        1
    (((lambda (x) (if (= (modulo x 2) 0) + -)) n2) (leibnizLambda (- n2 1)) (/ 1.0 (+ (* n2 2) 1)))
    )
)

; test function with lambda
; testing 99, since we start from 0
(leibnizLambda 99)
