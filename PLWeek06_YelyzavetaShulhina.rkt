#lang racket
; recursive function with racket's append 
(define (oddRevList originList)
  (if (equal? originList `())
      `()
      (cond [(odd? (car originList)) (append (oddRevList (cdr originList)) (list (car originList)))]
            [ else (oddRevList (cdr originList))]
          )
   )
)

;testing oddRevList
(oddRevList `( 1 2 3 4 5 6 7 8))

(oddRevList `(3 5 2 3 7 1 2 3 1 8)) 

(newline)



; using inner function with append
(define (oddRevList2 originList)
  (define finallist `())
  (oddRevListA originList finallist)
)

; inner recursive function with racket's append 
(define (oddRevListA originList finallist)
  (if (equal? originList `())
      finallist
      (cond [(odd? (car originList)) (set! finallist (cons (car originList) finallist)) (oddRevListA (cdr originList) finallist)]
            [ else (oddRevListA (cdr originList) finallist)]
          )
   )
)

;testing oddRevList
(oddRevList2 `( 1 2 3 4 5 6 7 8))

(oddRevList2 `(3 5 2 3 7 1 2 3 1 8)) 

(newline)



; using foldl and filter
; foldl traversed from left to right
; didn't use racket's append or reverse
(define (oddRevListFF originList)
  (foldl cons null (filter odd? originList))
)

;testing oddRevListFF
(oddRevListFF `( 1 2 3 4 5 6 7 8))

(oddRevListFF `(3 5 2 3 7 1 2 3 1 8)) 

(newline)



; using reverse and filter
(define (oddRevListRF originList)
  (reverse (filter odd? originList))
)

;testing oddRevListRF
(oddRevListRF `( 1 2 3 4 5 6 7 8))

(oddRevListRF `(3 5 2 3 7 1 2 3 1 8))
