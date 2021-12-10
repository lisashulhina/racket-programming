#lang racket

; this function inserts number into existing binary tree

(define (insert-into-tree inputNumber tree)
  (cond
    [ (empty? tree) (list inputNumber)]
    [else
     (let ((node (car tree)))
       (cond
         [ (empty? node) `() ]
         [ (empty? (cdr tree))
           (if (< node inputNumber)
               (append (list node) (list `()) (list (list inputNumber)))
               (append (list node) (list (list inputNumber)) (list `()))
           )
         ] ; last leaf
         [(< node inputNumber)
          (append
                       (list node)
                       (list (car (cdr tree)))
                       (list (insert-into-tree inputNumber (car (cdr (cdr tree)))))
          )
          ] ; inputNumber is greater than node and we go to the left branch (right bruch keeps the same)
         [else
                
          (append
                       (list node)
                       (list (insert-into-tree inputNumber (car (cdr tree))))
                       (list (car (cdr (cdr tree))))
          )
         ] ; inputNumber is less than or equal to node and we go to the right branch (left bruch keeps the same)
     )
     )
    ]
  )
)

; test-cases from presentation

(display "insert-into-tree 8 `(): ")
; should be `(8)
(insert-into-tree 8 `() )

(display "insert-into-tree 5 `(8): ")
; should be `(8 (5) ())
(insert-into-tree 5 `(8) )

(display "insert-into-tree 3 `(6 () (7)): ")
; should be `(6 (3) (7))
(insert-into-tree 3 `(6 () (7)) )

(display "insert-into-tree 4 `(6 (3) (7)): ")
; should be `(6 (3 () (4)) (7))
(insert-into-tree 4 `(6 (3) (7)) )

(display "insert-into-tree 4 '(6 (3) (7 (7) (9))): ")
; should be `(6 (3 () (4)) (7 (7) (9)))
(insert-into-tree 4 '(6 (3) (7 (7) (9))) )







; this function inserts numbers from list into existing binary tree (recursively calls insert-into-tree function for each element of the list)

(define (insert-list-to-tree inputList tree) 
  (cond
    [ (empty? inputList) tree ]
    [ else
      (insert-list-to-tree (cdr inputList) (insert-into-tree (car inputList) tree))
    ]
  )
)

(display "insert-list-to-tree `(22 25 7 16 8 34 67 7 32 17 8 4 5 3) `(6 (3) (7 (7) (9))): ")
(insert-list-to-tree `(22 25 7 16 8 34 67 7 32 17 8 4 5 3) `(6 (3) (7 (7) (9))))

(display "insert-list-to-tree `(22 25 7 16 8 34 67 7 32 17 8 4 5 3) `(): ")
; should be `(22 (7 (7 (4 (3) (5)) ()) (16 (8 (8) ()) (17))) (25 () (34 (32) (67))))
(insert-list-to-tree `(22 25 7 16 8 34 67 7 32 17 8 4 5 3) `())

(display "insert-list-to-tree `(5 4 3 2) `(): ")
; should be `(5 (4 (3 (2) ()) ()) ())
(insert-list-to-tree `(5 4 3 2) `())









; this was my initial function for inserting list into empty binary tree
; this function inserts numbers from list into empty binary tree
; if tree is not empty recursively calls insert-list-to-tree function 

(define (list-to-emptytree inputList tree) 
  (cond
    [ (empty? inputList) `() ]
    [ (empty? tree) 
      (let ((listNumber (car inputList))) 
                 (cond
                   [ (and (equal? (filter (lambda (x) (<= x listNumber) ) (cdr inputList)) `()) (equal? (filter (lambda (x) (> x listNumber) ) (cdr inputList)) `()) ) (list listNumber)]
                   [ else
                  (append                  
                     (list listNumber)
                     (list (list-to-emptytree (filter (lambda (x) (<= x listNumber) ) (cdr inputList)) tree)) ; less than or equal to node numbers go to the right branch
                     (list (list-to-emptytree (filter (lambda (x) (> x listNumber) ) (cdr inputList)) tree)) ; greater than node numbers go to the left branch
                  )
                  ]
                 )
      )
    ]
    [ else (insert-list-to-tree inputList tree) ] ; in case tree is not empty
  )
)

; test-cases from presentation

(display "list-to-emptytree `(22 25 7 16 8 34 67 7 32 17 8 4 5 3) `(): ")
; should be `(22 (7 (7 (4 (3) (5)) ()) (16 (8 (8) ()) (17))) (25 () (34 (32) (67))))
(list-to-emptytree `(22 25 7 16 8 34 67 7 32 17 8 4 5 3) `())

(display "list-to-emptytree `(5 4 3 2) `(): ")
; should be `(5 (4 (3 (2) ()) ()) ())
(list-to-emptytree `(5 4 3 2) `())
