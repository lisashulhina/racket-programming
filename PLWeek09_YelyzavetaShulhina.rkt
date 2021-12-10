#lang racket

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
         ] 
         [(< node inputNumber)
          (append
                       (list node)
                       (list (car (cdr tree)))
                       (list (insert-into-tree inputNumber (car (cdr (cdr tree)))))
          )
          ] 
         [else
                
          (append
                       (list node)
                       (list (insert-into-tree inputNumber (car (cdr tree))))
                       (list (car (cdr (cdr tree))))
          )
         ] 
     )
     )
    ]
  )
)

(define (insert-list-to-tree inputList tree) 
  (cond
    [ (empty? inputList) tree ]
    [ else
      (insert-list-to-tree (cdr inputList) (insert-into-tree (car inputList) tree))
    ]
  )
)




; this function converts list to tree and backwards (bassically sorts the list that way)

(define (tree-to-list tree)
  (cond
    [ (empty? tree) `() ]
    [else
     (let ((node (car tree)))
       (cond
         [ (empty? node) `() ]
         [ (empty? (cdr tree)) (list node) ]
         [else
          (append
                       (tree-to-list (car (cdr tree)))
                       (list node)
                       (tree-to-list (car (cdr (cdr tree))))
          )
         ]
     )
     )
    ]
  )
)

(display "`(5 4 3 2):\n ")
(tree-to-list (insert-list-to-tree `(5 4 3 2) `()))

(display "`(22 25 7 16 8 34 67 7 32 17 8 4 5 3):\n ")
(tree-to-list (insert-list-to-tree `(22 25 7 16 8 34 67 7 32 17 8 4 5 3) `()) )




