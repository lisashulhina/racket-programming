#lang racket

; function to avoid duplication of code (for 2 similar polynomial)
(define (myPolynomialBase numx2 numx num x)
    (+ (+ (* numx2 (* x x )) (* numx x)) num)
)

; defining first polynomial function
(define (myPolynomial1 x)
    (myPolynomialBase 5 -1 -8 x)
)

; defining second polynomial function
(define (myPolynomial2 x)
    (myPolynomialBase 4 -1 -2 x)
)

; defining afunction that define the halfway of 2 numbers
(define (halfway num1 num2)
    (/ (+ num1 num2) 2)
)

; defining accurance number to avoid magic constant in code
(define accurancenum 0.00001)

; defining a function to fing a polynomial zero
(define (polynomialZero functionpoly startingvalue1 startingvalue2)

  (let ([y1 (functionpoly startingvalue1)]
        [y3 (functionpoly (halfway startingvalue1 startingvalue2))]
        )
    
    (cond
     [(< (abs (- startingvalue1 startingvalue2)) accurancenum) (exact->inexact startingvalue1)] 
     [(> (* y1 y3) 0) (polynomialZero functionpoly (halfway startingvalue1 startingvalue2) startingvalue2) ]
     [(< (* y1 y3) 0) (polynomialZero functionpoly startingvalue1 (halfway startingvalue1 startingvalue2)) ]
     )
    
   )

)

; testing with the first polynom
; should be around 1.369
(display "y = 5x2 - x – 8 with starting values 1 and 2:")
(newline)
(polynomialZero myPolynomial1 1 2)

; testing with the second polynom
; should be around -0.593
(display "y = 4x2 - x – 2 with starting values -3 and 0:")
(newline)
(polynomialZero myPolynomial2 -3 0)