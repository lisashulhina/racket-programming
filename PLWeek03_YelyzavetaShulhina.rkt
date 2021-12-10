#lang racket
(define (basel n)
    (if (< n 1)
        0
    (+ (/ 1.0 (expt n 2)) (basel (- n 1)))
    )
)