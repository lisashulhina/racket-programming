#lang racket

;calculates the sum of the list 
(define (sumList inputList)
  (if (null? inputList)
      0
      (+ (car inputList) (sumList (cdr inputList)))
  )
)

;calculates the average of the list 
(define (average inputList)
  (/ (sumList inputList) (length inputList)))

; creates partitions for quicksort
(define (partition compare inputList)
      (cond
         [(empty? inputList) inputList] ; if the list is empty then return itself
         [(compare (car inputList)) (cons (car inputList) (partition compare (cdr inputList)))] ; comparing (depending on case) the first element with pivot element (average)
         [else (partition compare (cdr inputList))] ; if the first one do not satisty the case it goes to the rest
      )
 )

; quicksort using partition function
(define (myQuicksort inputList)
      (cond
         [(empty? inputList) inputList] ; if the list is empty then return itself
         [else
               (let ((pivot (average inputList))) ; assign average to the pivot
                 (append
                    (myQuicksort (partition (lambda (x) (< x pivot)) inputList)) ; case 1: if less than average - recursion 
                    (partition (lambda (x) (= x pivot)) inputList) ; case 2: if equal average 
                    (myQuicksort (partition (lambda (x) (> x pivot)) inputList)) ; case 3: partition if greater than average - recursion
                 )
                )
         ]
      )
)

(myQuicksort `( 20 13 74 5 12 9 22 95 22 6 101 72 3 53 33 21 96))



; quicksort using partition and sorting in one function using filter
(define (myQuicksort2 inputList)
  (cond
    [(empty? inputList) inputList] ; if the list is empty then return itself
    [else
     (let ([pivot (average inputList)]) ; assign average to the pivot
       (append
        (myQuicksort2 (filter (lambda (x) (< x pivot)) inputList)) ; case 1: myQuicksort2 on list of elements that are less than pivot (average)
        (filter (lambda (x) (= x pivot)) inputList) ; case 2: list of elements that are same as pivot (average)
        (myQuicksort2 (filter (lambda (x) (> x pivot)) inputList)) ; case 3: myQuicksort2 on list of elements that are greater than pivot (average)
       )
      )
    ]
  )
)


(myQuicksort2 `( 20 13 74 5 12 9 22 95 22 6 101 72 3 53 33 21 96))