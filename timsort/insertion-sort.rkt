#lang racket

(provide insertion-sort insert-into)

; Inserts x into the appropriate position in list l.
(define (insert-into l x)
  (cond
    [(empty? l) (list x)]
    [(<= (first l) x) (append (list (first l)) (insert-into (rest l) x))]
    [(> (first l) x) (append (list x) l)]
  )
)

; Sorts a list in ascending order.
(define (insertion-sort l)
  (foldr (lambda (x sorted-l) (insert-into sorted-l x)) '() l)
)
